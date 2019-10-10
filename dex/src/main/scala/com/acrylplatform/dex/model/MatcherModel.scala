package com.acrylplatform.dex.model

import cats.implicits._
import cats.kernel.Monoid
import com.acrylplatform.account.Address
import com.acrylplatform.dex.error
import com.acrylplatform.dex.error.MatcherError
import com.acrylplatform.dex.model.MatcherModel.Price
import com.acrylplatform.state.{Blockchain, Portfolio}
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.assets.exchange._
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.math.BigDecimal.RoundingMode

object MatcherModel {

  type Price = Long

  def getAssetDecimals(asset: Asset, blockchain: Blockchain): Either[MatcherError, Int] =
    asset.fold[Either[MatcherError, Int]](Right(8)) { issuedAsset =>
      blockchain
        .assetDescription(issuedAsset)
        .toRight[MatcherError](error.AssetNotFound(issuedAsset))
        .map(_.decimals)
    }

  def getPairDecimals(pair: AssetPair, blockchain: Blockchain): Either[MatcherError, (Int, Int)] =
    (getAssetDecimals(pair.amountAsset, blockchain), getAssetDecimals(pair.priceAsset, blockchain)).tupled

  object Normalization {

    def normalizeAmountAndFee(value: Double, amountAssetDecimals: Int): Long =
      (BigDecimal(value) * BigDecimal(10).pow(amountAssetDecimals)).toLong

    def normalizePrice(value: Double, amountAssetDecimals: Int, priceAssetDecimals: Int): Long =
      (BigDecimal(value) * BigDecimal(10).pow(8 + priceAssetDecimals - amountAssetDecimals).toLongExact).toLong

    def normalizePrice(value: Double, pair: AssetPair, decimals: (Int, Int)): Long = {
      val (amountAssetDecimals, priceAssetDecimals) = decimals
      normalizePrice(value, amountAssetDecimals, priceAssetDecimals)
    }
  }

  object Denormalization {

    def denormalizeAmountAndFee(value: Long, amountAssetDecimals: Int): Double =
      (BigDecimal(value) / BigDecimal(10).pow(amountAssetDecimals)).toDouble

    def denormalizeAmountAndFee(value: Price, pair: AssetPair, blockchain: Blockchain): Either[MatcherError, Double] =
      getAssetDecimals(pair.amountAsset, blockchain).map(denormalizeAmountAndFee(value, _))

    def denormalizePrice(value: Long, amountAssetDecimals: Int, priceAssetDecimals: Int): Double =
      (BigDecimal(value) / BigDecimal(10).pow(8 + priceAssetDecimals - amountAssetDecimals).toLongExact).toDouble

    def denormalizePrice(value: Price, pair: AssetPair, blockchain: Blockchain): Either[MatcherError, Double] =
      getPairDecimals(pair, blockchain).map(denormalizePrice(value, pair, _))

    def denormalizePrice(value: Price, pair: AssetPair, decimals: (Int, Int)): Double = {
      val (amountAssetDecimals, priceAssetDecimals) = decimals
      denormalizePrice(value, amountAssetDecimals, priceAssetDecimals)
    }
  }

  sealed trait DecimalsFormat
  final case object Denormalized extends DecimalsFormat
  final case object Normalized   extends DecimalsFormat
}

case class LevelAgg(amount: Long, price: Long)

sealed trait LimitOrder {
  def amount: Long // could be remaining or executed, see OrderExecuted
  def fee: Long    // same
  def order: Order

  def price: Price = order.price
  def partial(amount: Long, fee: Long): LimitOrder

  protected def rawSpendAmount: Long // Without correction
  protected def spendAmount: Long
  protected def receiveAmount: Long

  def spentAsset: Asset = order.getSpendAssetId
  def rcvAsset: Asset   = order.getReceiveAssetId
  val feeAsset: Asset   = order.matcherFeeAssetId

  def requiredBalance: Map[Asset, Long] = Monoid.combine(
    Map(spentAsset -> rawSpendAmount),
    Map(feeAsset   -> (if (feeAsset == rcvAsset) (fee - receiveAmount).max(0L) else fee))
  )

  def amountOfPriceAsset: Long                           = (BigDecimal(amount) * price / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
  def amountOfAmountAsset: Long                          = correctedAmountOfAmountAsset(amount, price)
  private def executionAmount(counterPrice: Price): Long = correctedAmountOfAmountAsset(amount, counterPrice)

  def isValid: Boolean = isValid(price)
  def isValid(counterPrice: Price): Boolean =
    amount > 0 && amount >= minimalAmountOfAmountAssetByPrice(counterPrice) && amount < Order.MaxAmount && spendAmount > 0 && receiveAmount > 0

  private def minimalAmountOfAmountAssetByPrice(p: Long): Long = (BigDecimal(Order.PriceConstant) / p).setScale(0, RoundingMode.CEILING).toLong
  private def correctedAmountOfAmountAsset(a: Long, p: Long): Long = {
    val settledTotal = (BigDecimal(p) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / p * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }
}

case class BuyLimitOrder(amount: Long, fee: Long, order: Order) extends LimitOrder {
  def partial(amount: Long, fee: Long): BuyLimitOrder = copy(amount = amount, fee = fee)
  def receiveAmount: Long                             = amountOfAmountAsset
  def spendAmount: Long                               = amountOfPriceAsset
  def rawSpendAmount: Long                            = amountOfPriceAsset
  override def toString                               = s"BuyLimitOrder($amount,$fee,${order.id()})"
}

case class SellLimitOrder(amount: Long, fee: Long, order: Order) extends LimitOrder {
  def partial(amount: Long, fee: Long): SellLimitOrder = copy(amount = amount, fee = fee)
  def receiveAmount: Long                              = amountOfPriceAsset
  def spendAmount: Long                                = amountOfAmountAsset
  def rawSpendAmount: Long                             = amount
  override def toString                                = s"SellLimitOrder($amount,$fee,${order.id()})"
}

object LimitOrder {
  def apply(o: Order): LimitOrder = {
    val pf = partialFee(o.matcherFee, o.amount, o.amount)
    o.orderType match {
      case OrderType.BUY  => BuyLimitOrder(o.amount, pf, o)
      case OrderType.SELL => SellLimitOrder(o.amount, pf, o)
    }
  }

  def partialFee(matcherFee: Long, totalAmount: Long, partialAmount: Long): Long = {
    // Should not round! It could lead to forks. See ExchangeTransactionDiff
    (BigInt(matcherFee) * partialAmount / totalAmount).toLong
  }

  def executedAmount(submitted: LimitOrder, counter: LimitOrder): Long =
    math.min(submitted.executionAmount(counter.price), counter.amountOfAmountAsset)
}

sealed trait OrderStatus {
  def name: String
  def json: JsValue

  def filledAmount: Long
  def filledFee: Long
}

object OrderStatus {
  sealed trait Final extends OrderStatus

  case object Accepted extends OrderStatus {
    val name           = "Accepted"
    def json: JsObject = Json.obj("status" -> name)

    override def filledAmount: Long = 0
    override def filledFee: Long = 0
  }
  case object NotFound extends Final {
    val name           = "NotFound"
    def json: JsObject = Json.obj("status" -> name, "message" -> "The limit order is not found")

    override def filledAmount: Long = 0
    override def filledFee: Long = 0
  }
  case class PartiallyFilled(filledAmount: Long, filledFee: Long) extends OrderStatus {
    val name           = "PartiallyFilled"
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filledAmount, "filledFee" -> filledFee)
  }
  case class Filled(filledAmount: Long, filledFee: Long) extends Final {
    val name           = "Filled"
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filledAmount, "filledFee" -> filledFee)
  }
  case class Cancelled(filledAmount: Long, filledFee: Long) extends Final {
    val name           = "Cancelled"
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filledAmount, "filledFee" -> filledFee)
  }

  def finalStatus(lo: LimitOrder, unmatchable: Boolean): Final = {
    val filledAmount = lo.order.amount - lo.amount
    val filledMatcherFee = lo.order.matcherFee - lo.fee
    if (unmatchable && filledAmount > 0) Filled(filledAmount, filledMatcherFee) else Cancelled(filledAmount, filledMatcherFee)
  }
}

object Events {

  sealed trait Event

  case class OrderExecuted(submitted: LimitOrder, counter: LimitOrder, timestamp: Long) extends Event {
    lazy val executedAmount: Long = LimitOrder.executedAmount(submitted, counter)

    def counterRemainingAmount: Long = math.max(counter.amount - executedAmount, 0)
    def counterExecutedFee: Long     = LimitOrder.partialFee(counter.order.matcherFee, counter.order.amount, executedAmount)
    def counterRemainingFee: Long    = math.max(counter.fee - counterExecutedFee, 0)
    def counterRemaining: LimitOrder = counter.partial(amount = counterRemainingAmount, fee = counterRemainingFee)

    def submittedRemainingAmount: Long = math.max(submitted.amount - executedAmount, 0)
    def submittedExecutedFee: Long     = LimitOrder.partialFee(submitted.order.matcherFee, submitted.order.amount, executedAmount)
    def submittedRemainingFee: Long    = math.max(submitted.fee - submittedExecutedFee, 0)
    def submittedRemaining: LimitOrder = submitted.partial(amount = submittedRemainingAmount, fee = submittedRemainingFee)
  }

  case class OrderAdded(order: LimitOrder, timestamp: Long) extends Event

  case class OrderCanceled(limitOrder: LimitOrder, unmatchable: Boolean, timestamp: Long) extends Event

  case class ExchangeTransactionCreated(tx: ExchangeTransaction)

  case class BalanceChanged(changes: Map[Address, BalanceChanged.Changes]) {
    def isEmpty: Boolean = changes.isEmpty
  }

  object BalanceChanged {
    val empty: BalanceChanged = BalanceChanged(Map.empty)
    case class Changes(updatedPortfolio: Portfolio, changedAssets: Set[Option[Asset]])
  }
}
