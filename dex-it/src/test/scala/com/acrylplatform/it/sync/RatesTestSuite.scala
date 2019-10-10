package com.acrylplatform.it.sync

import akka.http.scaladsl.model.StatusCodes._
import com.typesafe.config.{Config, ConfigFactory}
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.it.MatcherSuiteBase
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.api.SyncMatcherHttpApi._
import com.acrylplatform.it.sync.config.MatcherPriceAssetConfig._
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.Order
import com.acrylplatform.transaction.assets.exchange.Order.PriceConstant
import com.acrylplatform.transaction.assets.exchange.OrderType.BUY
import com.acrylplatform.transaction.transfer.TransferTransactionV2

class RatesTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = {

    val orderFeeSettingsStr =
      s"""
         |acryl.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee {
         |    mode = dynamic
         |    dynamic {
         |      base-fee = 300000
         |    }
         |  }  
         |}
       """.stripMargin

    super.nodeConfigs.map(
      ConfigFactory
        .parseString(orderFeeSettingsStr)
        .withFallback
    )
  }

  val defaultRateMap: Map[Asset, Double] = Map(Acryl -> 1d)

  val wctRate        = 0.2
  val wctRateUpdated = 0.5

  val wctStr   = WctId.toString
  val wctAsset = IssuedAsset(WctId)

  val btcStr   = BtcId.toString
  val btcAsset = IssuedAsset(BtcId)

  val usdStr = UsdId.toString

  val (amount, price) = (1000L, PriceConstant)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val txIds = Seq(IssueUsdTx, IssueWctTx, IssueBtcTx).map(_.json()).map(node.broadcastRequest(_).id)
    txIds.foreach(node.waitForTransaction(_))

    val transferTxId = node
      .broadcastRequest(
        TransferTransactionV2
          .selfSigned(
            assetId = btcAsset,
            sender = bob,
            recipient = alice.toAddress,
            amount = matcherFee * 5,
            timestamp = System.currentTimeMillis(),
            feeAssetId = Acryl,
            feeAmount = 300000,
            attachment = Array.emptyByteArray
          )
          .explicitGet()
          .json())
      .id
    node.waitForTransaction(transferTxId)
  }

  def getOrder: Order = node.prepareOrder(alice, wctUsdPair, BUY, amount, price, fee = matcherFee, version = 3, matcherFeeAssetId = btcAsset)

  "Rates can be handled via REST" in {
    // default rates
    node.getRates shouldBe defaultRateMap

    // add rate for wct
    node.upsertRate(wctAsset, wctRate, expectedStatusCode = Created).message shouldBe s"Rate $wctRate for the asset $wctStr added"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRate)

    // update rate for wct
    node
      .upsertRate(wctAsset, wctRateUpdated, expectedStatusCode = OK)
      .message shouldBe s"Rate for the asset $wctStr updated, old value = $wctRate, new value = $wctRateUpdated"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRateUpdated)

    // update rate for Acryl is not allowed
    node.upsertRate(Acryl, wctRateUpdated, expectedStatusCode = BadRequest).message shouldBe "Rate for Acryl cannot be changed"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRateUpdated)

    // delete rate for wct
    node.deleteRate(wctAsset).message shouldBe s"Rate for the asset $wctStr deleted, old value = $wctRateUpdated"
    node.getRates shouldBe defaultRateMap
  }

  "Changing rates affects order validation" in {
    // set rate for btc
    node.upsertRate(btcAsset, 1, expectedStatusCode = Created)

    // place order with admissible fee (according to btc rate = 1)
    val placedOrderId1 = node.placeOrder(getOrder).message.id
    node.waitOrderStatus(wctUsdPair, placedOrderId1, "Accepted")

    // slightly increase rate for btc
    node.upsertRate(btcAsset, 1.1, expectedStatusCode = OK)

    // the same order now is rejected
    node.expectIncorrectOrderPlacement(
      getOrder,
      400,
      "OrderRejected",
      Some(s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr")
    )

    // return previous rate for btc
    node.upsertRate(btcAsset, 1, expectedStatusCode = OK)

    val placedOrderId2 = node.placeOrder(getOrder).message.id
    node.waitOrderStatus(wctUsdPair, placedOrderId2, "Accepted")

    node.deleteRate(btcAsset)
  }

  "Rates are restored from the DB after matcher's restart" in {
    // add high rate for btc
    node.upsertRate(btcAsset, 1.1, expectedStatusCode = Created)

    // order with low fee should be rejected
    node.expectIncorrectOrderPlacement(
      getOrder,
      400,
      "OrderRejected",
      Some(s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr")
    )

    // restart matcher
    docker.restartNode(node)

    // order with low fee should be rejected again
    node.expectIncorrectOrderPlacement(
      getOrder,
      400,
      "OrderRejected",
      Some(s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr")
    )
  }
}
