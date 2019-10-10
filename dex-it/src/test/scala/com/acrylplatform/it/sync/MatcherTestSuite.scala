package com.acrylplatform.it.sync

import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.Base58
import com.acrylplatform.it.MatcherSuiteBase
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.api.SyncMatcherHttpApi._
import com.acrylplatform.it.api.{AssetDecimalsInfo, LevelResponse}
import com.acrylplatform.it.sync.config.MatcherPriceAssetConfig._
import com.acrylplatform.it.util._
import com.acrylplatform.dex.db.OrderDB
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.OrderType._
import com.acrylplatform.transaction.assets.exchange._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration._
import scala.util.Random

class MatcherTestSuite extends MatcherSuiteBase with TableDrivenPropertyChecks {
  private val aliceSellAmount         = 500
  private val exTxFee                 = 300000
  private val amountAssetName         = "AliceCoin"
  private val AssetQuantity           = 1000
  private val aliceCoinDecimals: Byte = 0

  private def orderVersion = (Random.nextInt(2) + 1).toByte

  "Check cross ordering between Alice and Bob" - {
    // Alice issues new asset
    val aliceAsset = node
      .broadcastIssue(alice,
                      amountAssetName,
                      "AliceCoin for matcher's tests",
                      AssetQuantity,
                      aliceCoinDecimals,
                      reissuable = false,
                      smartIssueFee,
                      None)
      .id
    val bobAsset = node
      .broadcastIssue(bob, "BobCoin1", "Bob's asset", someAssetAmount, 5, false, smartIssueFee, None)
      .id
    val bobAsset2 = node
      .broadcastIssue(bob, "BobCoin2", "Bob's asset", someAssetAmount, 0, false, smartIssueFee, None)
      .id

    Seq(aliceAsset, bobAsset, bobAsset2).foreach(node.waitForTransaction(_))

    val aliceAcrylPair = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceAsset).get), Acryl)

    val order1         = node.prepareOrder(alice, aliceAcrylPair, SELL, aliceSellAmount, 2000.acryl, version = orderVersion, timeToLive = 2.minutes)
    val order1Response = node.placeOrder(order1)

    // Bob issues new asset
    val bobAcrylPair = AssetPair(
      amountAsset = IssuedAsset(ByteStr.decodeBase58(bobAsset2).get),
      priceAsset = Acryl
    )

    "assert addresses balances" in {
      node.assertAssetBalance(alice.address, aliceAsset, AssetQuantity)
      node.assertAssetBalance(matcher.address, aliceAsset, 0)
      node.assertAssetBalance(bob.address, aliceAsset, 0)
    }

    "matcher should respond with Public key" in {
      node.matcherGet("/matcher").getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe Base58.encode(matcher.publicKey)
    }

    "get opened trading markets" in {
      val openMarkets = node.tradingMarkets()
      openMarkets.markets.size shouldBe 1
      val markets = openMarkets.markets.head

      markets.amountAssetName shouldBe amountAssetName
      markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(aliceCoinDecimals))

      markets.priceAssetName shouldBe "ACRYL"
      markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(8))
    }

    "sell order could be placed correctly" - {
      "alice places sell order" in {
        order1Response.status shouldBe "OrderAccepted"

        // Alice checks that the order in order book
        node.waitOrderStatus(aliceAcrylPair, order1Response.message.id, "Accepted")

        // Alice check that order is correct
        val orders = node.orderBook(aliceAcrylPair)
        orders.asks.head.amount shouldBe aliceSellAmount
        orders.asks.head.price shouldBe 2000.acryl
      }

      "frozen amount should be listed via matcherBalance REST endpoint" in {
        node.reservedBalance(alice) shouldBe Map(aliceAsset -> aliceSellAmount)
        node.reservedBalance(bob) shouldBe Map()
      }

      "and should be listed by trader's publiс key via REST" in {
        node.fullOrderHistory(alice).map(_.id) should contain(order1Response.message.id)
      }

      "and should match with buy order" in {
        val bobBalance     = node.accountBalances(bob.address)._1
        val matcherBalance = node.accountBalances(matcher.address)._1
        val aliceBalance   = node.accountBalances(alice.address)._1

        // Bob places a buy order
        val order2 = node.placeOrder(bob, aliceAcrylPair, BUY, 200, 2.acryl * Order.PriceConstant, matcherFee, orderVersion)
        order2.status shouldBe "OrderAccepted"

        node.waitOrderStatus(aliceAcrylPair, order1Response.message.id, "PartiallyFilled")
        node.waitOrderStatus(aliceAcrylPair, order2.message.id, "Filled")

        node.orderHistoryByPair(bob, aliceAcrylPair).map(_.id) should contain(order2.message.id)
        node.fullOrderHistory(bob).map(_.id) should contain(order2.message.id)

        node.waitOrderInBlockchain(order2.message.id)

        // Bob checks that asset on his balance
        node.assertAssetBalance(bob.address, aliceAsset, 200)

        // Alice checks that part of her order still in the order book
        val orders = node.orderBook(aliceAcrylPair)
        orders.asks.head.amount shouldBe 300
        orders.asks.head.price shouldBe 2000.acryl

        // Alice checks that she sold some assets
        node.assertAssetBalance(alice.address, aliceAsset, 800)

        // Bob checks that he spent some Acryl
        val updatedBobBalance = node.accountBalances(bob.address)._1
        updatedBobBalance shouldBe (bobBalance - 2000 * 200 - matcherFee)

        // Alice checks that she received some Acryl
        val updatedAliceBalance = node.accountBalances(alice.address)._1
        updatedAliceBalance shouldBe (aliceBalance + 2000 * 200 - (matcherFee * 200.0 / 500.0).toLong)

        // Matcher checks that it earn fees
        val updatedMatcherBalance = node.accountBalances(matcher.address)._1
        updatedMatcherBalance shouldBe (matcherBalance + matcherFee + (matcherFee * 200.0 / 500.0).toLong - exTxFee)
      }

      "request activeOnly orders" in {
        val aliceOrders = node.activeOrderHistory(alice)
        aliceOrders.map(_.id) shouldBe Seq(order1Response.message.id)
        val bobOrders = node.activeOrderHistory(bob)
        bobOrders.map(_.id) shouldBe Seq()
      }

      "submitting sell orders should check availability of asset" in {
        // Bob trying to place order on more assets than he has - order rejected
        val badOrder = node.prepareOrder(bob, aliceAcrylPair, SELL, 300, 1900.acryl, orderVersion)
        node.expectIncorrectOrderPlacement(badOrder, 400, "OrderRejected") should be(true)

        // Bob places order on available amount of assets - order accepted
        val order3 = node.placeOrder(bob, aliceAcrylPair, SELL, 150, 1900.acryl, matcherFee, orderVersion)
        node.waitOrderStatus(aliceAcrylPair, order3.message.id, "Accepted")

        // Bob checks that the order in the order book
        val orders = node.orderBook(aliceAcrylPair)
        orders.asks should contain(LevelResponse(150, 1900.acryl))
      }

      "buy order should match on few price levels" in {
        val matcherBalance = node.accountBalances(matcher.address)._1
        val aliceBalance   = node.accountBalances(alice.address)._1
        val bobBalance     = node.accountBalances(bob.address)._1

        // Alice places a buy order
        val order4 =
          node.placeOrder(alice, aliceAcrylPair, BUY, 350, (21.acryl / 10.0 * Order.PriceConstant).toLong, matcherFee, orderVersion)
        order4.status should be("OrderAccepted")

        // Where were 2 sells that should fulfill placed order
        node.waitOrderStatus(aliceAcrylPair, order4.message.id, "Filled")

        // Check balances
        node.waitOrderInBlockchain(order4.message.id)
        node.assertAssetBalance(alice.address, aliceAsset, 950)
        node.assertAssetBalance(bob.address, aliceAsset, 50)

        val updatedMatcherBalance = node.accountBalances(matcher.address)._1
        updatedMatcherBalance should be(
          matcherBalance - 2 * exTxFee + matcherFee + (matcherFee * 150.0 / 350.0).toLong + (matcherFee * 200.0 / 350.0).toLong + (matcherFee * 200.0 / 500.0).toLong)

        val updatedBobBalance = node.accountBalances(bob.address)._1
        updatedBobBalance should be(bobBalance - matcherFee + 150 * 1900)

        val updatedAliceBalance = node.accountBalances(alice.address)._1
        updatedAliceBalance should be(
          aliceBalance - (matcherFee * 200.0 / 350.0).toLong - (matcherFee * 150.0 / 350.0).toLong - (matcherFee * 200.0 / 500.0).toLong - 1900 * 150)
      }

      "order could be canceled and resubmitted again" in {
        // Alice cancels the very first order (100 left)
        val status1 = node.cancelOrder(alice, aliceAcrylPair, order1Response.message.id)
        status1.status should be("OrderCanceled")

        // Alice checks that the order book is empty
        val orders1 = node.orderBook(aliceAcrylPair)
        orders1.asks.size should be(0)
        orders1.bids.size should be(0)

        // Alice places a new sell order on 100
        val order4 = node.placeOrder(alice, aliceAcrylPair, SELL, 100, 2000.acryl, matcherFee, orderVersion)
        order4.status should be("OrderAccepted")

        // Alice checks that the order is in the order book
        val orders2 = node.orderBook(aliceAcrylPair)
        orders2.asks should contain(LevelResponse(100, 2000.acryl))
      }

      "buy order should execute all open orders and put remaining in order book" in {
        val matcherBalance = node.accountBalances(matcher.address)._1
        val aliceBalance   = node.accountBalances(alice.address)._1
        val bobBalance     = node.accountBalances(bob.address)._1

        // Bob places buy order on amount bigger then left in sell orders
        val order5 = node.placeOrder(bob, aliceAcrylPair, BUY, 130, 2000.acryl, matcherFee, orderVersion)

        // Check that the order is partially filled
        node.waitOrderStatus(aliceAcrylPair, order5.message.id, "PartiallyFilled")

        // Check that remaining part of the order is in the order book
        val orders = node.orderBook(aliceAcrylPair)
        orders.bids should contain(LevelResponse(30, 2000.acryl))

        // Check balances
        node.waitOrderInBlockchain(order5.message.id)
        node.assertAssetBalance(alice.address, aliceAsset, 850)
        node.assertAssetBalance(bob.address, aliceAsset, 150)

        val updatedMatcherBalance = node.accountBalances(matcher.address)._1
        updatedMatcherBalance should be(matcherBalance - exTxFee + matcherFee + (matcherFee * 100.0 / 130.0).toLong)

        val updatedBobBalance = node.accountBalances(bob.address)._1
        updatedBobBalance should be(bobBalance - (matcherFee * 100.0 / 130.0).toLong - 100 * 2000)

        val updatedAliceBalance = node.accountBalances(alice.address)._1
        updatedAliceBalance should be(aliceBalance - matcherFee + 2000 * 100)
      }

      "request order book for blacklisted pair" in {
        val f = node.matcherGetStatusCode(s"/matcher/orderbook/$ForbiddenAssetId/ACRYL", 404)
        f.message shouldBe s"The asset $ForbiddenAssetId not found"
      }

      "should consider UTX pool when checking the balance" in {

        node.assertAssetBalance(alice.address, bobAsset, 0)
        node.assertAssetBalance(matcher.address, bobAsset, 0)
        node.assertAssetBalance(bob.address, bobAsset, someAssetAmount)
        val bobAcrylPair = AssetPair(IssuedAsset(ByteStr.decodeBase58(bobAsset).get), Acryl)

        def bobOrder = node.prepareOrder(bob, bobAcrylPair, SELL, someAssetAmount, 0.005.acryl, matcherFee, orderVersion)

        val order6 = node.placeOrder(bobOrder)
        node.waitOrderStatus(bobAcrylPair, order6.message.id, "Accepted")

        // Alice wants to buy all Bob's assets for 1 Wave
        val order7 = node.placeOrder(alice, bobAcrylPair, BUY, someAssetAmount, 0.005.acryl, matcherFee, orderVersion)
        node.waitOrderStatus(bobAcrylPair, order7.message.id, "Filled")

        node.waitOrderInBlockchain(order7.message.id)
        // Bob tries to do the same operation, but at now he have no assets
        node.expectIncorrectOrderPlacement(bobOrder, 400, "OrderRejected")
      }

      "trader can buy acryl for assets with order without having acryl" in {
        val bobBalance = node.accountBalances(bob.address)._1
        node.assertAssetBalance(alice.address, bobAsset2, 0)
        node.assertAssetBalance(matcher.address, bobAsset2, 0)
        node.assertAssetBalance(bob.address, bobAsset2, someAssetAmount)

        // Bob wants to sell all own assets for 1 Wave
        def bobOrder =
          node.prepareOrder(bob, bobAcrylPair, SELL, someAssetAmount, 1.acryl, matcherFee, orderVersion)

        val order8 = node.placeOrder(bobOrder)
        node.waitOrderStatus(bobAcrylPair, order8.message.id, "Accepted")
        node.reservedBalance(bob)

        // Bob moves all acryl to Alice
        val transferAmount = bobBalance - minFee
        node.broadcastTransfer(bob, alice.address, transferAmount, minFee, None, None, waitForTx = true).id
        node.reservedBalance(bob)

        node.accountBalances(bob.address)._1 shouldBe 0

        // Order should stay accepted
        node.waitOrderStatus(bobAcrylPair, order8.message.id, "Accepted")

        // Cleanup
        node.cancelOrder(bob, bobAcrylPair, order8.message.id).status should be("OrderCanceled")
        node.broadcastTransfer(alice, bob.address, transferAmount, minFee, None, None, waitForTx = true)
      }

      "market status" in {
        val ask       = 5.acryl
        val askAmount = 5000000

        val bid       = 10.acryl
        val bidAmount = 10000000

        node.placeOrder(bob, bobAcrylPair, SELL, askAmount, ask, matcherFee, orderVersion)

        val resp1 = node.marketStatus(bobAcrylPair)
        resp1.lastPrice shouldBe None
        resp1.lastSide shouldBe None
        resp1.bid shouldBe None
        resp1.bidAmount shouldBe None
        resp1.ask shouldBe Some(ask)
        resp1.askAmount shouldBe Some(askAmount)

        node.placeOrder(alice, bobAcrylPair, BUY, bidAmount, bid, matcherFee, orderVersion)

        val resp2 = node.marketStatus(bobAcrylPair)
        resp2.lastPrice shouldBe Some(ask)
        resp2.lastSide shouldBe Some(OrderType.BUY.toString)
        resp2.bid shouldBe Some(bid)
        resp2.bidAmount shouldBe Some(bidAmount - askAmount)
        resp2.ask shouldBe None
        resp2.askAmount shouldBe None
      }
    }
  }

  "Max 8 price decimals allowed to be non zero" - {
    val ap28 = issueAssetPair(alice, 2, 8)
    val ap34 = issueAssetPair(alice, 3, 4)
    val ap08 = issueAssetPair(alice, 0, 8)

    {
      val xs = Seq(ap28._1, ap28._2, ap34._1, ap34._2, ap08._1, ap08._2).map(_.json()).map(node.broadcastRequest(_))
      xs.foreach(x => node.waitForTransaction(x.id))
    }

    val assets =
      Table(
        ("pair", "amountDecimals", "priceDecimals"),
        (ap28._3, 2, 8),
        (ap34._3, 3, 4),
        (ap08._3, 0, 8),
      )

    forAll(assets) { (pair: AssetPair, amountDecimals: Int, priceDecimals: Int) =>
      s"Not able to place order, amount decimals =  $amountDecimals, price decimals =  $priceDecimals " in {
        val amount     = BigDecimal(10).pow(amountDecimals).toLong
        val valid      = BigDecimal(10).pow(8 + priceDecimals - amountDecimals).longValue()
        val minInvalid = valid + BigDecimal(10).pow(priceDecimals - amountDecimals + 1).longValue() + 1
        val maxInvalid = valid + BigDecimal(10).pow(priceDecimals - amountDecimals + 1).longValue() - 1
        val o1         = node.prepareOrder(alice, pair, SELL, amount, minInvalid)
        val o2         = node.prepareOrder(alice, pair, SELL, amount, maxInvalid)

        node.expectIncorrectOrderPlacement(o1, 400, "OrderRejected", Some(s"Invalid price, last ${priceDecimals - amountDecimals} digits must be 0"))
        node.expectIncorrectOrderPlacement(o2, 400, "OrderRejected", Some(s"Invalid price, last ${priceDecimals - amountDecimals} digits must be 0"))
      }
    }

    forAll(assets) { (pair: AssetPair, amountDecimals: Int, priceDecimals: Int) =>
      s"Able to place order, amount decimals =  $amountDecimals, price decimals =  $priceDecimals " in {
        val amount            = BigDecimal(10).pow(amountDecimals + 8).toLong //big amount, because low price
        val minNonZeroInvalid = BigDecimal(10).pow(priceDecimals - amountDecimals + 1).longValue()
        val o1                = node.placeOrder(alice, pair, BUY, amount, minNonZeroInvalid, matcherFee)
        o1.status shouldBe "OrderAccepted"
      }
    }
  }

  "Order statuses for old orders" in {
    val (amountAssetTx, priceAssetTx, pair) = issueAssetPair(alice, 2, 8)

    def placeOrder(i: Int, tpe: OrderType) = node.placeOrder(alice, pair, tpe, 100L + i, Order.PriceConstant, matcherFee)

    val txIds = List(amountAssetTx, priceAssetTx).map(_.json()).map(node.broadcastRequest(_)).map(_.id)
    txIds.foreach(node.waitForTransaction(_))

    val ids = (1 to (OrderDB.OldestOrderIndexOffset + 5)).flatMap { i =>
      List(
        placeOrder(i, OrderType.BUY).message.id,
        placeOrder(i, OrderType.SELL).message.id
      )
    }

    ids.foreach { id =>
      val status = node.orderStatus(id, pair, waitForStatus = false).status
      withClue(id)(status should not be "NotFound")
    }
  }

  "Debug information was updated" in {
    val currentOffset = node.getCurrentOffset
    currentOffset should be > 0L

    val oldestSnapshotOffset = node.getOldestSnapshotOffset
    oldestSnapshotOffset should be <= currentOffset

    val snapshotOffsets = node.getAllSnapshotOffsets
    snapshotOffsets.foreach {
      case (assetPair, offset) =>
        withClue(assetPair) {
          offset should be <= currentOffset
        }
    }
  }
}
