package com.acrylplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.it.MatcherSuiteBase
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.api.SyncMatcherHttpApi
import com.acrylplatform.it.api.SyncMatcherHttpApi._
import com.acrylplatform.it.sync.config.MatcherPriceAssetConfig._
import com.acrylplatform.it.util._
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.IssueTransactionV1
import com.acrylplatform.transaction.assets.exchange.{AssetPair, OrderType}

class MatcherTickerTestSuite extends MatcherSuiteBase {

  import MatcherTickerTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs.map(updatedMatcherConfig.withFallback)

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    val issueTx = node.broadcastRequest(IssueUsdTx.json())
    node.waitForTransaction(issueTx.id)
  }

  "matcher ticker validation" - {
    "get tickers for unavailable asset should produce error" in {
      SyncMatcherHttpApi.assertNotFoundAndMessage(node.marketStatus(wctAcrylPair), s"The asset ${IssueEightDigitAssetTx.id()} not found")
    }

    "status of empty orderbook" in {
//    TODO: add error message after fix of https://acrylplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(node.marketStatus(acrylUsdPair), s"")
    }

    "error of non-existed order" in {
      //TODO: add error message after fix of https://acrylplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(node.orderStatus(IssueUsdTx.id().toString, acrylUsdPair), s"")
    }

    "try to work with incorrect pair" in {
      val usdAcrylPair = AssetPair(UsdId, Acryl)

      assert(
        node
          .matcherGet(s"/matcher/orderbook/${usdAcrylPair.amountAssetStr}/${usdAcrylPair.priceAssetStr}/status", statusCode = 301)
          .getHeader("Location")
          .contains(s"ACRYL/${usdAcrylPair.amountAssetStr}"))

      //TODO: add error message after fix of https://acrylplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(node.placeOrder(node, usdAcrylPair, OrderType.BUY, 1.acryl, 200), "")
    }

    "issue tokens" in {
      val tx = node.broadcastRequest(IssueEightDigitAssetTx.json())
      node.waitForTransaction(tx.id)
    }

    val bidPrice  = 200
    val bidAmount = 1.acryl
    val askPrice  = 400
    val askAmount = bidAmount / 2

    "place bid order for first pair" in {
      node.placeOrder(alice, edUsdPair, OrderType.BUY, bidAmount, bidPrice, matcherFee)
      val aliceOrder = node.placeOrder(alice, edUsdPair, OrderType.BUY, bidAmount, bidPrice, matcherFee).message.id
      node.waitOrderStatus(edUsdPair, aliceOrder, "Accepted")

      val r = node.marketStatus(edUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe None
      r.askAmount shouldBe None
    }

    "place ask order for second pair" in {
      node.placeOrder(bob, wctAcrylPair, OrderType.SELL, askAmount, askPrice, matcherFee)
      val bobOrder = node.placeOrder(bob, wctAcrylPair, OrderType.SELL, askAmount, askPrice, matcherFee).message.id
      node.waitOrderStatus(wctAcrylPair, bobOrder, "Accepted")
      val r = node.marketStatus(wctAcrylPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe None
      r.bidAmount shouldBe None
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)
    }

    "place ask order for first pair" in {
      node.placeOrder(bob, edUsdPair, OrderType.SELL, askAmount, askPrice, matcherFee)
      val bobOrder = node.placeOrder(bob, edUsdPair, OrderType.SELL, askAmount, askPrice, matcherFee).message.id
      node.waitOrderStatus(edUsdPair, bobOrder, "Accepted")
      val r = node.marketStatus(edUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)
    }

    "match bid order for first pair" in {
      val bobOrder = node.placeOrder(bob, edUsdPair, OrderType.SELL, askAmount, bidPrice, matcherFee).message.id
      node.waitOrderStatus(edUsdPair, bobOrder, "Filled")
      val r = node.marketStatus(edUsdPair)
      r.lastPrice shouldBe Some(bidPrice)
      r.lastSide shouldBe Some("sell")
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount - askAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)

      val bobOrder1 = node.placeOrder(bob, edUsdPair, OrderType.SELL, 3 * askAmount, bidPrice, matcherFee).message.id
      node.waitOrderStatus(edUsdPair, bobOrder1, "Filled")
      val s = node.marketStatus(edUsdPair)
      s.lastPrice shouldBe Some(bidPrice)
      s.lastSide shouldBe Some("sell")
      s.bid shouldBe None
      s.bidAmount shouldBe None
      s.ask shouldBe Some(askPrice)
      s.askAmount shouldBe Some(2 * askAmount)
    }

    "match ask order for first pair" in {
      val aliceOrder = node.placeOrder(alice, edUsdPair, OrderType.BUY, bidAmount, askPrice, matcherFee).message.id
      node.waitOrderStatus(edUsdPair, aliceOrder, "Filled")
      val r = node.marketStatus(edUsdPair)
      r.lastPrice shouldBe Some(askPrice)
      r.lastSide shouldBe Some("buy")
      r.bid shouldBe None
      r.bidAmount shouldBe None
      r.ask shouldBe None
      r.askAmount shouldBe None
    }
  }

}

object MatcherTickerTestSuite {

  import ConfigFactory._

  val Decimals: Byte = 2

  val usdAssetName             = "USD-X"
  val eightDigitAssetAssetName = "Eight-X"
  val IssueUsdTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alice,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.acryl,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  val IssueEightDigitAssetTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = bob,
      name = eightDigitAssetAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      fee = 1.acryl,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  val UsdId: IssuedAsset             = IssuedAsset(IssueUsdTx.id())
  val EightDigitAssetId: IssuedAsset = IssuedAsset(IssueEightDigitAssetTx.id())

  val edUsdPair = AssetPair(
    amountAsset = EightDigitAssetId,
    priceAsset = UsdId
  )

  val wctAcrylPair = AssetPair(
    amountAsset = EightDigitAssetId,
    priceAsset = Acryl
  )

  val acrylUsdPair = AssetPair(
    amountAsset = Acryl,
    priceAsset = UsdId
  )

  private val updatedMatcherConfig = parseString(s"""acryl.dex.price-assets = ["${UsdId.id.toString}", "ACRYL"]""".stripMargin)
}
