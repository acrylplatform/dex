package com.acrylplatform.it.sync

import com.acrylplatform.it.MatcherSuiteBase
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.api.SyncMatcherHttpApi._
import com.acrylplatform.it.sync.config.MatcherPriceAssetConfig._
import com.acrylplatform.it.util._
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.{AssetPair, OrderType}

import scala.concurrent.duration._

class CancelOrderTestSuite extends MatcherSuiteBase {
  private val acrylBtcPair = AssetPair(Acryl, IssuedAsset(BtcId))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val xs = Seq(IssueUsdTx, IssueBtcTx).map(_.json()).map(node.signedBroadcast(_))
    xs.foreach(tx => node.waitForTransaction(tx.id))
  }

  "Order can be canceled" - {
    "by sender" in {
      val orderId = node.placeOrder(bob, acrylUsdPair, OrderType.SELL, 100.acryl, 800, matcherFee).message.id
      node.waitOrderStatus(acrylUsdPair, orderId, "Accepted", 1.minute)

      node.cancelOrder(bob, acrylUsdPair, orderId)
      node.waitOrderStatus(acrylUsdPair, orderId, "Cancelled", 1.minute)

      node.orderHistoryByPair(bob, acrylUsdPair).collectFirst {
        case o if o.id == orderId => o.status shouldEqual "Cancelled"
      }
    }
    "with API key" in {
      val orderId = node.placeOrder(bob, acrylUsdPair, OrderType.SELL, 100.acryl, 800, matcherFee).message.id
      node.waitOrderStatus(acrylUsdPair, orderId, "Accepted", 1.minute)

      node.cancelOrderWithApiKey(orderId)
      node.waitOrderStatus(acrylUsdPair, orderId, "Cancelled", 1.minute)

      node.fullOrderHistory(bob).filter(_.id == orderId).head.status shouldBe "Cancelled"
      node.orderHistoryByPair(bob, acrylUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"

      val orderBook = node.orderBook(acrylUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Cancel is rejected" - {
    "when request sender is not the sender of and order" in {
      val orderId = node.placeOrder(bob, acrylUsdPair, OrderType.SELL, 100.acryl, 800, matcherFee).message.id
      node.waitOrderStatus(acrylUsdPair, orderId, "Accepted", 1.minute)

      node.expectCancelRejected(matcher, acrylUsdPair, orderId)

      // Cleanup
      node.cancelOrder(bob, acrylUsdPair, orderId)
      node.waitOrderStatus(acrylUsdPair, orderId, "Cancelled")
    }
  }

  "Batch cancel" - {
    "works for" - {
      "all orders placed by an address" in {
        node.fullOrderHistory(bob)

        val usdOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, acrylUsdPair, OrderType.SELL, 100.acryl + i, 400, matcherFee).message.id
        }

        node.assetBalance(bob.toAddress.stringRepr, BtcId.toString)

        val btcOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, acrylBtcPair, OrderType.BUY, 100.acryl + i, 400, matcherFee).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(acrylUsdPair, id, "Accepted"))

        node.cancelAllOrders(bob)

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(acrylUsdPair, id, "Cancelled"))
      }

      "a pair" in {
        val usdOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, acrylUsdPair, OrderType.SELL, 100.acryl + i, 400, matcherFee).message.id
        }

        val btcOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, acrylBtcPair, OrderType.BUY, 100.acryl + i, 400, matcherFee).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(acrylUsdPair, id, "Accepted"))

        node.cancelOrdersForPair(bob, acrylBtcPair)

        btcOrderIds.foreach(id => node.waitOrderStatus(acrylUsdPair, id, "Cancelled"))
        usdOrderIds.foreach(id => node.waitOrderStatus(acrylUsdPair, id, "Accepted"))
      }
    }
  }
}
