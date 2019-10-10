package com.acrylplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.it.MatcherSuiteBase
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.api.SyncMatcherHttpApi._
import com.acrylplatform.it.sync.config.MatcherPriceAssetConfig._
import com.acrylplatform.it.util._
import com.acrylplatform.dex.market.MatcherActor
import com.acrylplatform.dex.model.MatcherModel.Price
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.util.Random

class TradersTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = super.nodeConfigs.map(TradersTestSuite.matcherSettingsOrderV3Allowed.withFallback)

  private def orderVersion = (Random.nextInt(3) + 1).toByte

  "Verifications of tricky ordering cases" - {
    // Alice issues new asset
    val aliceAsset = node
      .broadcastIssue(alice,
                      "AliceCoin",
                      "AliceCoin for matcher's tests",
                      someAssetAmount,
                      0,
                      reissuable = false,
                      smartIssueFee,
                      None,
                      waitForTx = true)
      .id

    // Wait for balance on Alice's account
    node.assertAssetBalance(alice.address, aliceAsset, someAssetAmount)
    node.assertAssetBalance(matcher.address, aliceAsset, 0)
    node.assertAssetBalance(bob.address, aliceAsset, 0)

    // Bob issues a new asset
    val bobAssetQuantity = 10000
    val bobNewAsset =
      node.broadcastIssue(bob, "BobCoin3", "Bob's asset", bobAssetQuantity, 0, reissuable = false, smartIssueFee, None, waitForTx = true).id

    val bobAssetId   = IssuedAsset(ByteStr.decodeBase58(bobNewAsset).get)
    val aliceAssetId = IssuedAsset(ByteStr.decodeBase58(aliceAsset).get)

    val bobAcrylPair = AssetPair(
      amountAsset = bobAssetId,
      priceAsset = Acryl
    )

    val twoAssetsPair =
      if (MatcherActor.compare(Some(bobAssetId.id.arr), Some(aliceAssetId.id.arr)) < 0)
        AssetPair(
          amountAsset = aliceAssetId,
          priceAsset = bobAssetId
        )
      else
        AssetPair(
          amountAsset = bobAssetId,
          priceAsset = aliceAssetId
        )

    node.assertAssetBalance(bob.address, bobNewAsset, bobAssetQuantity)

    "AssetPair BOB/ACRYL vs BOB/NULL" in {
      val trickyBobAcrylPairWB58 = AssetPair(
        amountAsset = bobAssetId,
        priceAsset = IssuedAsset(ByteStr.decodeBase58("ACRYL").get)
      )

      trickyBobAcrylPairWB58.key shouldBe bobAcrylPair.key

      val trickyBobAcrylPairWS = AssetPair(
        priceAsset = IssuedAsset(ByteStr("ACRYL".getBytes())),
        amountAsset = bobAssetId
      )

      val trickyBobOrderWB58 = node.prepareOrder(bob, trickyBobAcrylPairWB58, OrderType.BUY, 1, 10.acryl * Order.PriceConstant)
      node.expectIncorrectOrderPlacement(trickyBobOrderWB58, 400, "OrderRejected")

      val trickyBobOrderWS = node.prepareOrder(bob, trickyBobAcrylPairWS, OrderType.BUY, 1, 10.acryl * Order.PriceConstant)
      node.expectIncorrectOrderPlacement(trickyBobOrderWS, 400, "OrderRejected")

      val correctBobOrder   = node.prepareOrder(bob, bobAcrylPair, OrderType.BUY, 1, 10.acryl * Order.PriceConstant)
      val correctBobOrderId = node.placeOrder(correctBobOrder).message.id
      node.waitOrderStatus(bobAcrylPair, correctBobOrderId, "Accepted")

      val markets = node.tradingMarkets().markets.map(x => s"${x.amountAsset}-${x.priceAsset}").toSet

      withClue("hasTrickyBobAcrylPairWB58Market") {
        markets.contains(trickyBobAcrylPairWB58.key) shouldBe true
      }

      withClue("hasTrickyBobAcrylPairWSMarket") {
        markets.contains(trickyBobAcrylPairWS.key) shouldBe false
      }

      withClue("bobAcrylPair") {
        markets.contains(bobAcrylPair.key) shouldBe true
      }

      node.orderBook(bobAcrylPair).bids shouldNot be(empty)
      node.cancelOrder(bob, bobAcrylPair, correctBobOrderId)
      node.waitOrderStatus(bobAcrylPair, correctBobOrderId, "Cancelled")
    }

    "owner moves assets/acryl to another account and order become an invalid" - {
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val oldestOrderId = bobPlacesAssetOrder(4000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(4000, twoAssetsPair, bobNewAsset)

          // 5000 acryl are rest
          node.broadcastTransfer(bob, alice.address, 5000, matcherFee, Some(bobNewAsset), None, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            node.waitOrderStatus(bobAcrylPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobAcrylPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, twoAssetsPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")
          node.broadcastTransfer(alice, bob.address, 5000, matcherFee, Some(bobNewAsset), None, waitForTx = true).id
        }

        "leased acryl, insufficient fee" in {
          val bobBalance    = node.accountBalances(bob.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val leaseAmount = bobBalance - matcherFee - matcherFee
          val leaseId     = node.broadcastLease(bob, alice.address, leaseAmount, matcherFee, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            node.waitOrderStatus(bobAcrylPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobAcrylPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, twoAssetsPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          node.broadcastCancelLease(bob, leaseId, matcherFee, waitForTx = true).id
        }

        "moved acryl, insufficient fee" in {
          val bobBalance    = node.accountBalances(bob.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val transferAmount = bobBalance - matcherFee - matcherFee
          node.broadcastTransfer(bob, alice.address, transferAmount, matcherFee, None, None, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            node.waitOrderStatus(bobAcrylPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobAcrylPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, twoAssetsPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")
          node.broadcastTransfer(alice, bob.address, transferAmount, matcherFee, None, None, waitForTx = true).id
        }
      }

      "order with acryl" - {
        "leased acryl, insufficient fee for one ExchangeTransaction" in {
          // Amount of acryl in order is smaller than fee
          val bobBalance = node.accountBalances(bob.address)._1

          val oldestOrderId = bobPlacesWaveOrder(bobAcrylPair, 1, 10.acryl * Order.PriceConstant)
          val newestOrderId = bobPlacesWaveOrder(bobAcrylPair, 1, 10.acryl * Order.PriceConstant)

          //      waitForOrderStatus(node, bobAssetIdRaw, id, "Accepted")
          val leaseAmount = bobBalance - matcherFee - 10.acryl - matcherFee
          val leaseId     = node.broadcastLease(bob, alice.address, leaseAmount, matcherFee, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' is Cancelled") {
            node.waitOrderStatus(bobAcrylPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobAcrylPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, bobAcrylPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          node.broadcastCancelLease(bob, leaseId, matcherFee, waitForTx = true)
        }

        "leased acryl, insufficient acryl" in {
          val bobBalance = node.accountBalances(bob.address)._1
          val price      = 1.acryl
          val order2     = bobPlacesWaveOrder(bobAcrylPair, 1, price * Order.PriceConstant)

          val leaseAmount = bobBalance - matcherFee - price / 2
          val leaseId     = node.broadcastLease(bob, alice.address, leaseAmount, matcherFee, waitForTx = true).id

          withClue(s"The order '$order2' was cancelled") {
            node.waitOrderStatus(bobAcrylPair, order2, "Cancelled")
          }

          // Cleanup
          node.broadcastCancelLease(bob, leaseId, matcherFee, waitForTx = true)
        }

        "moved acryl, insufficient fee" in {
          // Amount of acryl in order is smaller than fee
          val bobBalance = node.accountBalances(bob.address)._1
          val price      = matcherFee / 2
          val order3     = bobPlacesWaveOrder(bobAcrylPair, 1, price * Order.PriceConstant)

          val transferAmount = bobBalance - matcherFee - price
          node.broadcastTransfer(bob, alice.address, transferAmount, matcherFee, None, None, waitForTx = true).id

          withClue(s"The order '$order3' was cancelled") {
            node.waitOrderStatus(bobAcrylPair, order3, "Cancelled")
          }

          // Cleanup
          node.broadcastTransfer(alice, bob.address, transferAmount, matcherFee, None, None, waitForTx = true).id
        }
      }
    }
  }

  def bobPlacesWaveOrder(assetPair: AssetPair, amount: Long, price: Price): String = {
    val bobOrder = node.prepareOrder(bob, assetPair, OrderType.BUY, amount, price)
    val order    = node.placeOrder(bobOrder).message.id
    node.waitOrderStatus(assetPair, order, "Accepted")
    order
  }

  def bobPlacesAssetOrder(bobCoinAmount: Int, twoAssetsPair: AssetPair, assetId: String): String = {
    val decodedAsset = IssuedAsset(ByteStr.decodeBase58(assetId).get)
    val bobOrder = if (twoAssetsPair.amountAsset == decodedAsset) {
      node.prepareOrder(bob, twoAssetsPair, OrderType.SELL, bobCoinAmount, 1 * Order.PriceConstant, matcherFee, orderVersion)
    } else {
      node.prepareOrder(bob, twoAssetsPair, OrderType.BUY, 1, bobCoinAmount * Order.PriceConstant, matcherFee, orderVersion)
    }
    val order = node.placeOrder(bobOrder)
    node.waitOrderStatus(twoAssetsPair, order.message.id, "Accepted")
    order.message.id
  }

}

object TradersTestSuite {
  val matcherSettingsOrderV3Allowed: Config = ConfigFactory.parseString("acryl.dex { allowed-order-versions = [1, 2, 3] }")
}
