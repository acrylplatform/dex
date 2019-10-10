package com.acrylplatform.dex.model

import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.features.BlockchainFeatures
import com.acrylplatform.dex.MatcherTestData
import com.acrylplatform.state.Blockchain
import com.acrylplatform.state.diffs.produce
import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.transaction.Proofs
import com.acrylplatform.transaction.assets.exchange.{AssetPair, ExchangeTransactionV1, ExchangeTransactionV2}
import com.acrylplatform.{NoShrink, crypto}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ExchangeTransactionCreatorSpecification
    extends WordSpec
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks
    with NoShrink {

  private val pair = AssetPair(Acryl, mkAssetId("BTC"))

  "ExchangeTransactionCreator" when {
    "SmartAccountTrading hasn't been activated yet" should {
      "create an ExchangeTransactionV1" in {
        val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L))
        val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L))

        val bc = stub[Blockchain]
        (bc.activatedFeatures _).when().returns(Map.empty).anyNumberOfTimes()
        val tc = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)
        tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()).explicitGet() shouldBe a[ExchangeTransactionV1]
      }

      "return an error" when {
        List((1, 2), (2, 1), (2, 2)).foreach {
          case (buyVersion, sellVersion) =>
            s"buyV$buyVersion and sellV$sellVersion" in {
              val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L), version = buyVersion.toByte)
              val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L), version = sellVersion.toByte)

              val bc = stub[Blockchain]
              (bc.activatedFeatures _).when().returns(Map.empty).anyNumberOfTimes()
              val tc = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)
              tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()) should produce(
                "Smart Account Trading feature has not been activated yet")
            }
        }
      }
    }

    "SmartAccountTrading has been activated" should {
      "create an ExchangeTransactionV2" in {
        val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L), version = 2)
        val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L), version = 2)

        val bc = stub[Blockchain]
        (bc.activatedFeatures _).when().returns(Map(BlockchainFeatures.SmartAccountTrading.id -> 0)).anyNumberOfTimes()
        val tc = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)
        tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()).explicitGet() shouldBe a[ExchangeTransactionV2]
      }
    }
  }

  "ExchangeTransactionCreator" should {

    val bc = stub[Blockchain]

    (bc.activatedFeatures _)
      .when()
      .returns(Map(BlockchainFeatures.OrderV3.id -> 0, BlockchainFeatures.SmartAccountTrading.id -> 0))
      .anyNumberOfTimes()

    "calculate fees in exchange transaction which are equal to matcher fees in fully matched orders" in {

      val preconditions =
        for {
          ((_, buyOrder), (_, sellOrder)) <- orderV3PairGenerator
          orderSettings                   <- orderFeeSettingsGenerator(Some(buyOrder.matcherFeeAssetId))
        } yield (buyOrder, sellOrder, orderSettings)

      forAll(preconditions) {
        case (buyOrder, sellOrder, orderSettings) =>
          val tc = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)
          val tx = tc.createTransaction(LimitOrder(buyOrder), LimitOrder(sellOrder), System.currentTimeMillis).explicitGet()

          tx.buyMatcherFee shouldBe buyOrder.matcherFee
          tx.sellMatcherFee shouldBe sellOrder.matcherFee
      }
    }

    "create valid exchange transaction when orders are matched partially" in {

      import com.acrylplatform.transaction.assets.exchange.OrderOps._

      val preconditions =
        for {
          ((_, buyOrder), (senderSell, sellOrder)) <- orderV3PairGenerator
          orderSettings                            <- orderFeeSettingsGenerator(Some(buyOrder.matcherFeeAssetId))
        } yield {

          val sellOrderWithUpdatedAmount = sellOrder.updateAmount(sellOrder.amount / 2)
          val newSignature               = crypto.sign(senderSell, sellOrderWithUpdatedAmount.bodyBytes())
          val correctedSellOrder         = sellOrderWithUpdatedAmount.updateProofs(Proofs(Seq(ByteStr(newSignature))))

          (buyOrder, correctedSellOrder, orderSettings)
        }

      forAll(preconditions) {
        case (buyOrder, sellOrder, orderSettings) =>
          val tc = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)
          val tx = tc.createTransaction(LimitOrder(buyOrder), LimitOrder(sellOrder), System.currentTimeMillis)

          tx shouldBe 'right
      }
    }
  }
}
