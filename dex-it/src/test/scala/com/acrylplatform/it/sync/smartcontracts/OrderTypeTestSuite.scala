package com.acrylplatform.it.sync.smartcontracts

import com.acrylplatform.api.http.ApiError.TransactionNotAllowedByAccountScript
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.it.MatcherSuiteBase
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.api.SyncMatcherHttpApi._
import com.acrylplatform.it.sync._
import com.acrylplatform.it.sync.config.MatcherPriceAssetConfig._
import com.acrylplatform.it.util._
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import play.api.libs.json.Json

import scala.concurrent.duration._

class OrderTypeTestSuite extends MatcherSuiteBase {
  private val aliceAsset =
    node
      .broadcastIssue(alice, "AliceCoinOrders", "AliceCoin for tests with order types", someAssetAmount, 0, reissuable = false, smartIssueFee, None)
      .id

  Seq(aliceAsset, node.broadcastRequest(IssueUsdTx.json()).id).map(node.waitForTransaction(_))

  private val predefAssetPair = acrylUsdPair
  private val aliceAcrylPair  = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceAsset).get), Acryl)

  "Order types verification with SmartContracts" - {
    val sco1 = s"""
                 |{-# STDLIB_VERSION 2 #-}
                 |match tx {
                 | case o : Order =>
                 |   o.orderType == Buy
                 | case s : SetScriptTransaction => true
                 | case other => throw()
                 | }
                 |""".stripMargin

    val sco2 = s"""
              |{-# STDLIB_VERSION 2 #-}
              |match tx {
              | case o : Order =>
              |    o.orderType == Sell
              |  case s : SetScriptTransaction => true
              |  case _ => throw()
              | }
      """.stripMargin

    val sco3 = s"""
                 |{-# STDLIB_VERSION 2 #-}
                 |match tx {
                 |  case o : Order =>
                 |        o.orderType == Buy || o.orderType == Sell
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    "scenarios of order placement" - {
      "set contracts with only BUY type and then place order" in {
        setContract(Some(sco1), alice)

        val aliceOrd1 = node
          .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.acryl * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

        assertBadRequest(
          node
            .placeOrder(alice, aliceAcrylPair, OrderType.SELL, 500, 2.acryl * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id)

        node.cancelOrder(alice, predefAssetPair, aliceOrd1).status should be("OrderCanceled")

        setContract(None, alice)
      }

      "set contracts with only SELL type and then place order" in {
        setContract(Some(sco2), alice)

        assertBadRequest(
          node
            .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.acryl * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id)

        val aliceOrd2 = node
          .placeOrder(alice, aliceAcrylPair, OrderType.SELL, 500, 2.acryl * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(aliceAcrylPair, aliceOrd2, "Accepted", 1.minute)

        node.cancelOrder(alice, aliceAcrylPair, aliceOrd2).status should be("OrderCanceled")

        setContract(None, alice)
      }

      "set contracts with both SELL/BUY types and then place order" in {
        setContract(Some(sco3), alice)

        val aliceOrd1 = node
          .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.acryl * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

        val aliceOrd2 = node
          .placeOrder(alice, aliceAcrylPair, OrderType.SELL, 500, 2.acryl * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(aliceAcrylPair, aliceOrd2, "Accepted", 1.minute)

        node.cancelOrder(alice, predefAssetPair, aliceOrd1).status should be("OrderCanceled")
        node.cancelOrder(alice, aliceAcrylPair, aliceOrd2).status should be("OrderCanceled")

        setContract(None, alice)
      }

      "place order and then set contract on BUY type" in {
        val aliceOrd1 = node
          .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.acryl * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

        val aliceOrd2 = node
          .placeOrder(alice, aliceAcrylPair, OrderType.SELL, 500, 2.acryl * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(aliceAcrylPair, aliceOrd2, "Accepted", 1.minute)

        setContract(Some(sco1), alice)

        val bobOrd1 = node
          .placeOrder(bob, predefAssetPair, OrderType.SELL, 500, 2.acryl * Order.PriceConstant, smartMatcherFee, version = 1, 10.minutes)
          .message
          .id
        val bobOrd2 = node
          .placeOrder(bob, aliceAcrylPair, OrderType.BUY, 500, 2.acryl * Order.PriceConstant, smartMatcherFee, version = 1, 10.minutes)
          .message
          .id

        node.waitOrderStatus(predefAssetPair, aliceOrd1, "Filled", 1.minute)
        node.waitOrderStatus(aliceAcrylPair, aliceOrd2, "Filled", 1.minute)
        node.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
        node.waitOrderStatus(aliceAcrylPair, bobOrd2, "Filled", 1.minute)

        node.waitOrderInBlockchain(bobOrd1)

        val txs = node.waitTransactionsByOrder(bobOrd2, 1)
        node.expectSignedBroadcastRejected(Json.toJson(txs.head)) shouldBe TransactionNotAllowedByAccountScript.ErrorCode
      }
    }
  }
}
