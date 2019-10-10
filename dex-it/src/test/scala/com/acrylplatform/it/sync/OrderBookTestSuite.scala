package com.acrylplatform.it.sync

import com.acrylplatform.account.KeyPair
import com.acrylplatform.it.MatcherSuiteBase
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.api.SyncMatcherHttpApi
import com.acrylplatform.it.api.SyncMatcherHttpApi._
import com.acrylplatform.it.sync.config.MatcherPriceAssetConfig._
import com.acrylplatform.transaction.assets.exchange.Order.PriceConstant
import com.acrylplatform.transaction.assets.exchange.OrderType._

class OrderBookTestSuite extends MatcherSuiteBase {

  {
    val xs = Seq(IssueUsdTx, IssueWctTx).map(_.json()).map(node.broadcastRequest(_))
    xs.foreach(x => node.waitForTransaction(x.id))
    node.waitForHeight(node.height + 1)
  }

  case class ReservedBalances(wct: Long, usd: Long, acryl: Long)
  def reservedBalancesOf(pk: KeyPair): ReservedBalances = {
    val reservedBalances = node.reservedBalance(pk)
    ReservedBalances(
      reservedBalances.getOrElse(WctId.toString, 0),
      reservedBalances.getOrElse(UsdId.toString, 0),
      reservedBalances.getOrElse("ACRYL", 0)
    )
  }

  val (amount, price) = (1000L, PriceConstant)

  "When delete order book" - {
    val buyOrder        = node.placeOrder(alice, wctUsdPair, BUY, 2 * amount, price, matcherFee).message.id
    val anotherBuyOrder = node.placeOrder(alice, wctUsdPair, BUY, amount, price, matcherFee).message.id

    val submitted = node.placeOrder(bob, wctUsdPair, SELL, amount, price, matcherFee).message.id

    val sellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 2 * price, matcherFee).message.id

    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, submitted, "Filled")

    val (aliceRBForOnePair, bobRBForOnePair) = (reservedBalancesOf(alice), reservedBalancesOf(bob))

    val buyOrderForAnotherPair = node.placeOrder(alice, wctAcrylPair, BUY, amount, price, matcherFee).message.id
    val sellOrderForAnotherPair =
      node.placeOrder(bob, wctAcrylPair, SELL, amount, 2 * price, matcherFee).message.id

    node.waitOrderStatus(wctAcrylPair, buyOrderForAnotherPair, "Accepted")
    node.waitOrderStatus(wctAcrylPair, sellOrderForAnotherPair, "Accepted")

    val (aliceRBForBothPairs, bobRBForBothPairs) = (reservedBalancesOf(alice), reservedBalancesOf(bob))

    node.deleteOrderBook(wctUsdPair)

    "orders by the pair should be canceled" in {
      node.waitOrderStatus(wctUsdPair, buyOrder, "Cancelled")
      node.waitOrderStatus(wctUsdPair, anotherBuyOrder, "Cancelled")
      node.waitOrderStatus(wctUsdPair, sellOrder, "Cancelled")
    }

    "orderbook was deleted" in {
      withClue("orderBook") {
        val orderBook = node.orderBook(wctUsdPair)
        orderBook.bids shouldBe empty
        orderBook.asks shouldBe empty
      }

      withClue("tradingMarkets") {
        val tradingPairs = node.tradingMarkets().markets.map(x => s"${x.amountAsset}-${x.priceAsset}")
        tradingPairs shouldNot contain(wctUsdPair.key)
      }

      withClue("getAllSnapshotOffsets") {
        node.getAllSnapshotOffsets.keySet shouldNot contain(wctUsdPair.key)
      }
    }

    "reserved balances should be released for the pair" in {
      val (aliceReservedBalances, bobReservedBalances) = (reservedBalancesOf(alice), reservedBalancesOf(bob))
      aliceReservedBalances.usd shouldBe 0
      aliceReservedBalances.acryl shouldBe (aliceRBForBothPairs.acryl - aliceRBForOnePair.acryl)
      bobReservedBalances.wct shouldBe (bobRBForBothPairs.wct - bobRBForOnePair.wct)
      bobReservedBalances.acryl shouldBe (bobRBForBothPairs.acryl - bobRBForOnePair.acryl)
    }

    "it should not affect other pairs and their orders" in {
      node.orderStatus(buyOrderForAnotherPair, wctAcrylPair).status shouldBe "Accepted"
      node.orderStatus(sellOrderForAnotherPair, wctAcrylPair).status shouldBe "Accepted"
      node.placeOrder(alice, wctAcrylPair, BUY, amount, price, matcherFee)

      val orderBook = node.orderBook(wctAcrylPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks shouldNot be(empty)
    }

    "matcher can start after multiple delete events" in {
      import com.acrylplatform.it.api.AsyncMatcherHttpApi.{MatcherAsyncHttpApi => async}

      def deleteWctAcryl = async(node).deleteOrderBook(wctAcrylPair)
      val deleteMultipleTimes = deleteWctAcryl
        .zip(deleteWctAcryl)
        .map(_ => ())
        .recover { case _ => () } // It's ok: either this should fail, or restartNode should work

      SyncMatcherHttpApi.sync(deleteMultipleTimes)

      docker.restartNode(node)
    }
  }
}
