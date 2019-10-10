package com.acrylplatform.dex.model

import cats.implicits._
import cats.kernel.Monoid
import com.acrylplatform.account.{Address, PublicKey}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.dex.error._
import com.acrylplatform.dex.market.OrderBookActor.MarketStatus
import com.acrylplatform.dex.model.MatcherModel.Normalization
import com.acrylplatform.dex.settings.OrderFeeSettings._
import com.acrylplatform.dex.settings.{AssetType, DeviationsSettings, MatcherSettings, OrderRestrictionsSettings}
import com.acrylplatform.dex.smart.MatcherScriptRunner
import com.acrylplatform.dex.{RateCache, error}
import com.acrylplatform.features.BlockchainFeatures
import com.acrylplatform.features.FeatureProvider._
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.acrylplatform.metrics.TimerExt
import com.acrylplatform.state._
import com.acrylplatform.state.diffs.FeeValidation
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction._
import com.acrylplatform.transaction.assets.exchange.OrderOps._
import com.acrylplatform.transaction.assets.exchange._
import com.acrylplatform.transaction.smart.Verifier
import com.acrylplatform.transaction.smart.script.ScriptRunner
import com.acrylplatform.utils.{ScorexLogging, Time}
import kamon.Kamon
import shapeless.Coproduct

import scala.Either.cond
import scala.math.BigDecimal.RoundingMode
import scala.util.control.NonFatal

object OrderValidator extends ScorexLogging {

  type Result[T] = Either[MatcherError, T]

  private val timer = Kamon.timer("matcher.validation").refine("type" -> "blockchain")

  val MinExpiration: Long  = 60 * 1000L
  val MaxActiveOrders: Int = 200

  val exchangeTransactionCreationFee: Long = FeeValidation.OldFeeUnits(ExchangeTransaction.typeId) * FeeValidation.FeeUnit

  private[dex] def multiplyAmountByDouble(a: Long, d: Double): Long = (BigDecimal(a) * d).setScale(0, RoundingMode.HALF_UP).toLong
  private[dex] def multiplyPriceByDouble(p: Long, d: Double): Long  = (BigDecimal(p) * d).setScale(0, RoundingMode.HALF_UP).toLong
  private[dex] def multiplyFeeByDouble(f: Long, d: Double): Long    = (BigDecimal(f) * d).setScale(0, RoundingMode.CEILING).toLong

  private def verifySignature(order: Order): Result[Order] =
    Verifier.verifyAsEllipticCurveSignature(order).leftMap(x => error.OrderInvalidSignature(order.id(), x.toString))

  private def verifyOrderByAccountScript(blockchain: Blockchain, address: Address, order: Order): Result[Order] =
    blockchain.accountScript(address).fold(verifySignature(order)) { script =>
      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height))
        error.AccountFeatureUnsupported(BlockchainFeatures.SmartAccountTrading).asLeft
      else if (order.version <= 1) error.AccountNotSupportOrderVersion(address, 2, order.version).asLeft
      else
        try MatcherScriptRunner(script, order) match {
          case (_, Left(execError)) => error.AccountScriptReturnedError(address, execError).asLeft
          case (_, Right(FALSE))    => error.AccountScriptDeniedOrder(address).asLeft
          case (_, Right(TRUE))     => lift(order)
          case (_, Right(x))        => error.AccountScriptUnexpectResult(address, x.toString).asLeft
        } catch {
          case NonFatal(e) =>
            log.trace(error.formatStackTrace(e))
            error.AccountScriptException(address, e.getClass.getCanonicalName, e.getMessage).asLeft
        }
    }

  private def verifySmartToken(blockchain: Blockchain, asset: IssuedAsset, tx: ExchangeTransaction): Result[Unit] =
    blockchain.assetScript(asset).fold(success) { script =>
      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAssets, blockchain.height))
        error.AssetFeatureUnsupported(BlockchainFeatures.SmartAssets, asset).asLeft
      else
        try ScriptRunner(blockchain.height, Coproduct(tx), blockchain, script, isAssetScript = true, asset.id) match {
          case (_, Left(execError)) => error.AssetScriptReturnedError(asset, execError).asLeft
          case (_, Right(FALSE))    => error.AssetScriptDeniedOrder(asset).asLeft
          case (_, Right(TRUE))     => success
          case (_, Right(x))        => error.AssetScriptUnexpectResult(asset, x.toString).asLeft
        } catch {
          case NonFatal(e) =>
            error.AssetScriptException(asset, e.getClass.getCanonicalName, Option(e.getMessage).getOrElse("No message")).asLeft
        }
    }

  private def decimals(blockchain: Blockchain, assetId: Asset): Result[Int] =
    assetId.fold(lift(8)) { aid =>
      blockchain.assetDescription(aid).map(_.decimals).toRight(error.AssetNotFound(aid))
    }

  private def validateDecimals(blockchain: Blockchain, o: Order): Result[(Int, Int)] =
    for {
      pd <- decimals(blockchain, o.assetPair.priceAsset)
      ad <- decimals(blockchain, o.assetPair.amountAsset)
      insignificantDecimals = (pd - ad).max(0)
      _ <- cond(
        o.price % BigDecimal(10).pow(insignificantDecimals).toLongExact == 0,
        (),
        error.PriceLastDecimalsMustBeZero(insignificantDecimals)
      )
    } yield ad -> pd

  private def validateAmountAndPrice(order: Order,
                                     decimalsPair: (Int, Int),
                                     orderRestrictions: Map[AssetPair, OrderRestrictionsSettings]): Result[Order] = {
    if (!(orderRestrictions contains order.assetPair)) lift(order)
    else {
      val (amountAssetDecimals, priceAssetDecimals) = decimalsPair
      val restrictions                              = orderRestrictions(order.assetPair)

      def normalizeAmount(amt: Double): Long = Normalization.normalizeAmountAndFee(amt, amountAssetDecimals)
      def normalizePrice(prc: Double): Long  = Normalization.normalizePrice(prc, amountAssetDecimals, priceAssetDecimals)

      lift(order)
        .ensure(error.OrderInvalidAmount(order, restrictions)) { o =>
          normalizeAmount(restrictions.minAmount) <= o.amount && o.amount <= normalizeAmount(restrictions.maxAmount) &&
          o.amount % normalizeAmount(restrictions.stepAmount).max(1) == 0
        }
        .ensure(error.OrderInvalidPrice(order, restrictions)) { o =>
          normalizePrice(restrictions.minPrice) <= o.price && o.price <= normalizePrice(restrictions.maxPrice) &&
          o.price % normalizePrice(restrictions.stepPrice).max(1) == 0
        }
    }
  }

  private[dex] def checkOrderVersion(version: Byte, blockchain: Blockchain): Result[Unit] = version match {
    case 1 => success
    case 2 =>
      if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height)) success
      else Left(error.OrderVersionUnsupported(version, BlockchainFeatures.SmartAccountTrading))
    case 3 =>
      if (blockchain.isFeatureActivated(BlockchainFeatures.OrderV3, blockchain.height)) success
      else Left(error.OrderVersionUnsupported(version, BlockchainFeatures.OrderV3))
    case _ => Left(error.UnsupportedOrderVersion(version))
  }

  def blockchainAware(blockchain: Blockchain,
                      transactionCreator: (LimitOrder, LimitOrder, Long) => Either[ValidationError, ExchangeTransaction],
                      matcherAddress: Address,
                      time: Time,
                      orderFeeSettings: OrderFeeSettings,
                      orderRestrictions: Map[AssetPair, OrderRestrictionsSettings],
                      rateCache: RateCache)(order: Order): Result[Order] = timer.measure {

    lazy val exchangeTx: Result[ExchangeTransaction] = {
      val fakeOrder: Order = order.updateType(order.orderType.opposite)
      transactionCreator(LimitOrder(fakeOrder), LimitOrder(order), time.correctedTime()).left.map { x =>
        error.CanNotCreateExchangeTransaction(x.toString)
      }
    }

    def verifyAssetScript(assetId: Asset): Result[Unit] = assetId.fold(success) { assetId =>
      exchangeTx.flatMap(verifySmartToken(blockchain, assetId, _))
    }

    def verifyMatcherFeeAssetScript(matcherFeeAsset: Asset): Result[Unit] = {
      if (matcherFeeAsset == order.assetPair.amountAsset || matcherFeeAsset == order.assetPair.priceAsset) success
      else verifyAssetScript(matcherFeeAsset)
    }

    /** Checks whether order fee is enough to cover matcher's expenses for the Exchange transaction issue */
    lazy val validateOrderFeeByTransactionRequirements = orderFeeSettings match {
      case DynamicSettings(baseFee) =>
        val mof =
          multiplyFeeByDouble(
            ExchangeTransactionCreator.minFee(blockchain, matcherAddress, order.assetPair, baseFee),
            rateCache.getRate(order.matcherFeeAssetId).get
          )
        Either.cond(order.matcherFee >= mof, order, error.FeeNotEnough(mof, order.matcherFee, Acryl))
      case _ => lift(order)
    }

    for {
      _            <- checkOrderVersion(order.version, blockchain)
      _            <- validateOrderFeeByTransactionRequirements
      decimalsPair <- validateDecimals(blockchain, order)
      _            <- validateAmountAndPrice(order, decimalsPair, orderRestrictions)
      _            <- verifyOrderByAccountScript(blockchain, order.sender, order)
      _            <- verifyAssetScript(order.assetPair.amountAsset)
      _            <- verifyAssetScript(order.assetPair.priceAsset)
      _            <- verifyMatcherFeeAssetScript(order.matcherFeeAssetId)
    } yield order
  }

  private def validateBalance(order: Order, tradableBalance: Asset => Long): Result[Order] = {
    val lo               = LimitOrder(order)
    val requiredForOrder = lo.requiredBalance

    val available = requiredForOrder.keySet.map { assetId =>
      assetId -> tradableBalance(assetId)
    }.toMap

    val negativeBalances = Monoid.combine(available, requiredForOrder.mapValues(-_)).filter(_._2 < 0)
    cond(negativeBalances.isEmpty, order, error.BalanceNotEnough(requiredForOrder, available))
  }

  private[dex] def getValidFeeAssetForSettings(order: Order, orderFeeSettings: OrderFeeSettings, rateCache: RateCache): Set[Asset] =
    orderFeeSettings match {
      case _: DynamicSettings               => rateCache.getAllRates.keySet
      case FixedSettings(defaultAssetId, _) => Set(defaultAssetId)
      case PercentSettings(assetType, _) =>
        Set(
          assetType match {
            case AssetType.AMOUNT    => order.assetPair.amountAsset
            case AssetType.PRICE     => order.assetPair.priceAsset
            case AssetType.RECEIVING => order.getReceiveAssetId
            case AssetType.SPENDING  => order.getSpendAssetId
          }
        )
    }

  /**
    * Returns minimal valid fee that should be paid to the matcher when order is placed
    *
    * @param order            placed order
    * @param orderFeeSettings matcher settings for the fee of orders
    * @param matchPrice       price at which order is executed
    * @param rateCache        assets rates (asset cost in Acryl)
    * @param multiplier       coefficient that is used in market aware for specifying deviation bounds
    */
  private[dex] def getMinValidFeeForSettings(order: Order,
                                             orderFeeSettings: OrderFeeSettings,
                                             matchPrice: Long,
                                             rateCache: RateCache,
                                             multiplier: Double = 1): Long = {

    orderFeeSettings match {
      case DynamicSettings(dynamicBaseFee) => multiplyFeeByDouble(dynamicBaseFee, rateCache.getRate(order.matcherFeeAssetId).get)
      case FixedSettings(_, fixedMinFee)   => fixedMinFee
      case PercentSettings(assetType, minFeeInPercent) =>
        lazy val receiveAmount = order.getReceiveAmount(order.amount, matchPrice).explicitGet()
        lazy val spentAmount   = order.getSpendAmount(order.amount, matchPrice).explicitGet()

        val amountFactor = assetType match {
          case AssetType.AMOUNT    => order.amount
          case AssetType.PRICE     => if (order.orderType == OrderType.BUY) spentAmount else receiveAmount
          case AssetType.RECEIVING => receiveAmount
          case AssetType.SPENDING  => spentAmount
        }

        multiplyAmountByDouble(amountFactor, multiplier * minFeeInPercent / 100)
    }
  }

  private def validateOrderFee(order: Order, orderFeeSettings: OrderFeeSettings, rateCache: RateCache): Result[Order] = {
    if (order.version < 3) lift(order)
    else {

      lazy val requiredFeeAssetIds = getValidFeeAssetForSettings(order, orderFeeSettings, rateCache)
      lazy val requiredFee         = getMinValidFeeForSettings(order, orderFeeSettings, order.price, rateCache)

      lift(order)
        .ensure(error.UnexpectedFeeAsset(requiredFeeAssetIds, order.matcherFeeAssetId))(o => requiredFeeAssetIds contains o.matcherFeeAssetId)
        .ensure(error.FeeNotEnough(requiredFee, order.matcherFee, order.matcherFeeAssetId))(_.matcherFee >= requiredFee)
    }
  }

  def matcherSettingsAware(matcherPublicKey: PublicKey,
                           blacklistedAddresses: Set[Address],
                           blacklistedAssets: Set[IssuedAsset],
                           matcherSettings: MatcherSettings,
                           rateCache: RateCache)(order: Order): Result[Order] = {

    def validateBlacklistedAsset(assetId: Asset, e: IssuedAsset => MatcherError): Result[Unit] =
      assetId.fold(success)(x => cond(!blacklistedAssets(x), (), e(x)))

    for {
      _ <- lift(order)
        .ensure(error.UnexpectedMatcherPublicKey(matcherPublicKey, order.matcherPublicKey))(_.matcherPublicKey == matcherPublicKey)
        .ensure(error.AddressIsBlacklisted(order.sender))(o => !blacklistedAddresses.contains(o.sender.toAddress))
        .ensure(error.OrderVersionDenied(order.version, matcherSettings.allowedOrderVersions)) { o =>
          matcherSettings.allowedOrderVersions(o.version)
        }
      _ <- validateBlacklistedAsset(order.matcherFeeAssetId, error.FeeAssetBlacklisted)
      _ <- validateOrderFee(order, matcherSettings.orderFee, rateCache)
    } yield order
  }

  /**
    * Checks if price is in deviation bounds
    *
    *   For BUY orders:  (1 - p) * best bid <= price <= (1 + l) * best ask
    *   For SELL orders: (1 - l) * best bid <= price <= (1 + p) * best ask,
    *
    * where:
    *
    *   p = max price deviation profit,
    *   l = max price deviation loss,
    *   best bid = highest price of buy
    *   best ask = lowest price of sell
    */
  private def validatePriceDeviation(order: Order, deviationSettings: DeviationsSettings, marketStatus: Option[MarketStatus]): Result[Order] = {

    def isPriceInDeviationBounds(subtractedPercent: Double, addedPercent: Double): Boolean = marketStatus forall { ms =>
      lazy val isPriceHigherThanMinDeviation = ms.bestBid forall { bestBid =>
        order.price >= multiplyPriceByDouble(bestBid.price, 1 - (subtractedPercent / 100))
      }

      lazy val isPriceLessThanMaxDeviation = ms.bestAsk forall { bestAsk =>
        order.price <= multiplyPriceByDouble(bestAsk.price, 1 + (addedPercent / 100))
      }

      isPriceHigherThanMinDeviation && isPriceLessThanMaxDeviation
    }

    lift(order).ensure(error.DeviantOrderPrice(order, deviationSettings)) { _ =>
      if (order.orderType == OrderType.BUY) isPriceInDeviationBounds(deviationSettings.maxPriceProfit, deviationSettings.maxPriceLoss)
      else isPriceInDeviationBounds(deviationSettings.maxPriceLoss, deviationSettings.maxPriceProfit)
    }
  }

  /**
    * Checks if fee is in deviation bounds. Only applicable for the percent order fee settings
    *
    *   For BUY orders:  fee >= fee percent * (1 - f) * best ask * amount
    *   For SELL orders: fee >= fee percent * (1 - f) * best bid * amount,
    *
    * where:
    *
    *   f = max fee deviation
    *   best bid = highest price of buy
    *   best ask = lowest price of sell
    */
  private def validateFeeDeviation(order: Order,
                                   deviationSettings: DeviationsSettings,
                                   orderFeeSettings: OrderFeeSettings,
                                   marketStatus: Option[MarketStatus],
                                   rateCache: RateCache): Result[Order] = {

    def isFeeInDeviationBoundsForMatchedPrice(matchedPrice: Long): Boolean = orderFeeSettings match {
      case percentSettings: PercentSettings =>
        order.matcherFee >= getMinValidFeeForSettings(order, percentSettings, matchedPrice, rateCache, 1 - (deviationSettings.maxFeeDeviation / 100))
      case _ => true
    }

    val isFeeInDeviationBounds = marketStatus forall { ms =>
      (order.orderType, ms.bestAsk.isDefined, ms.bestBid.isDefined) match {
        case (OrderType.BUY, true, _)  => isFeeInDeviationBoundsForMatchedPrice(ms.bestAsk.get.price) // validate fee for the best (lowest) sell price
        case (OrderType.SELL, _, true) => isFeeInDeviationBoundsForMatchedPrice(ms.bestBid.get.price) // validate fee for the best (highest) buy price
        case _                         => true
      }
    }

    Either.cond(isFeeInDeviationBounds, order, error.DeviantOrderMatcherFee(order, deviationSettings))
  }

  def marketAware(orderFeeSettings: OrderFeeSettings,
                  deviationSettings: DeviationsSettings,
                  marketStatus: Option[MarketStatus],
                  rateCache: RateCache)(order: Order): Result[Order] = {
    if (deviationSettings.enabled) {
      for {
        _ <- validatePriceDeviation(order, deviationSettings, marketStatus)
        _ <- validateFeeDeviation(order, deviationSettings, orderFeeSettings, marketStatus, rateCache)
      } yield order
    } else lift(order)
  }

  def timeAware(time: Time)(order: Order): Result[Order] = {
    for {
      _ <- cond(order.expiration > time.correctedTime() + MinExpiration,
                (),
                error.WrongExpiration(time.correctedTime(), MinExpiration, order.expiration))
      _ <- order.isValid(time.correctedTime()).toEither.left.map(error.OrderCommonValidationFailed)
    } yield order
  }

  def accountStateAware(
      sender: Address,
      tradableBalance: Asset => Long,
      activeOrderCount: => Int,
      orderExists: ByteStr => Boolean,
  )(order: Order): Result[Order] =
    for {
      _ <- lift(order)
        .ensure(error.UnexpectedSender(order.sender.toAddress, sender))(_.sender.toAddress == sender)
        .ensure(error.ActiveOrdersLimitReached(MaxActiveOrders))(_ => activeOrderCount < MaxActiveOrders)
        .ensure(error.OrderDuplicate(order.id()))(o => !orderExists(o.id()))
      _ <- validateBalance(order, tradableBalance)
    } yield order

  private def lift[T](x: T): Result[T] = x.asRight[MatcherError]
  private def success: Result[Unit]    = lift(())
}
