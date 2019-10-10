package com.acrylplatform.it.sync

import com.acrylplatform.it.MatcherSuiteBase
import com.acrylplatform.it.api.AssetDecimalsInfo
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.api.SyncMatcherHttpApi._
import com.acrylplatform.it.sync.config.MatcherPriceAssetConfig._
import com.acrylplatform.it.util._
import com.acrylplatform.dex.model.LimitOrder
import com.acrylplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.acrylplatform.transaction.assets.exchange.{Order, OrderType}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode

class TradeBalanceAndRoundingTestSuite extends MatcherSuiteBase {
  {
    val xs = Seq(IssueUsdTx, IssueEthTx, IssueWctTx).map(_.json()).map(node.broadcastRequest(_))
    xs.foreach(x => node.waitForTransaction(x.id))
  }

  "Alice and Bob trade ACRYL-USD" - {
    val aliceAcrylBalanceBefore = node.accountBalances(alice.address)._1
    val bobAcrylBalanceBefore   = node.accountBalances(bob.address)._1

    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 3100000000L

    val correctedSellAmount = correctAmount(sellOrderAmount, price)

    val adjustedAmount = receiveAmount(OrderType.BUY, buyOrderAmount, price)
    val adjustedTotal  = receiveAmount(OrderType.SELL, buyOrderAmount, price)

    log.debug(s"correctedSellAmount: $correctedSellAmount, adjustedAmount: $adjustedAmount, adjustedTotal: $adjustedTotal")

    "place usd-acryl order" in {
      // Alice wants to sell USD for Acryl
      val bobOrder1   = node.prepareOrder(bob, acrylUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder1Id = node.placeOrder(bobOrder1).message.id
      node.waitOrderStatus(acrylUsdPair, bobOrder1Id, "Accepted", 1.minute)
      node.reservedBalance(bob)("ACRYL") shouldBe sellOrderAmount + matcherFee
      node.tradableBalance(bob, acrylUsdPair)("ACRYL") shouldBe bobAcrylBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder   = node.prepareOrder(alice, acrylUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrderId = node.placeOrder(aliceOrder).message.id
      node.waitOrderStatusAndAmount(acrylUsdPair, aliceOrderId, "Filled", Some(420169L), 1.minute)

      // Bob wants to buy some USD
      node.waitOrderStatusAndAmount(acrylUsdPair, bobOrder1Id, "PartiallyFilled", Some(420169L), 1.minute)

      // Each side get fair amount of assets
      node.waitOrderInBlockchain(aliceOrder.idStr())
    }

    "get opened trading markets. USD price-asset" in {
      val openMarkets = node.tradingMarkets()
      openMarkets.markets.size shouldBe 1
      val markets = openMarkets.markets.head

      markets.amountAssetName shouldBe "ACRYL"
      markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(8))

      markets.priceAssetName shouldBe usdAssetName
      markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
    }

    "check usd and acryl balance after fill" in {
      val aliceAcrylBalanceAfter = node.accountBalances(alice.address)._1
      val aliceUsdBalance = node.assetBalance(alice.address, UsdId.toString).balance

      val bobAcrylBalanceAfter = node.accountBalances(bob.address)._1
      val bobUsdBalance = node.assetBalance(bob.address, UsdId.toString).balance

      (aliceAcrylBalanceAfter - aliceAcrylBalanceBefore) should be(
        adjustedAmount - (BigInt(matcherFee) * adjustedAmount / buyOrderAmount).bigInteger.longValue())

      aliceUsdBalance - defaultAssetQuantity should be(-adjustedTotal)
      bobAcrylBalanceAfter - bobAcrylBalanceBefore should be(
        -adjustedAmount - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount).bigInteger.longValue())
      bobUsdBalance should be(adjustedTotal)
    }

    "check filled amount and tradable balance" in {
      val bobsOrderId  = node.fullOrderHistory(bob).head.id
      val filledAmount = node.orderStatus(bobsOrderId, acrylUsdPair).filledAmount.getOrElse(0L)

      filledAmount shouldBe adjustedAmount
    }

    "check reserved balance" in {
      val reservedFee = BigInt(matcherFee) - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount)
      log.debug(s"reservedFee: $reservedFee")
      val expectedBobReservedBalance = correctedSellAmount - adjustedAmount + reservedFee
      node.reservedBalance(bob)("ACRYL") shouldBe expectedBobReservedBalance

      node.reservedBalance(alice) shouldBe empty
    }

    "check acryl-usd tradable balance" in {
      val orderHistory = node.fullOrderHistory(bob)
      orderHistory.size should be(1)

      val expectedBobTradableBalance = bobAcrylBalanceBefore - (correctedSellAmount + matcherFee)
      node.tradableBalance(bob, acrylUsdPair)("ACRYL") shouldBe expectedBobTradableBalance
      node.tradableBalance(alice, acrylUsdPair)("ACRYL") shouldBe node.accountBalances(alice.address)._1

      val orderId = orderHistory.head.id
      node.cancelOrder(bob, acrylUsdPair, orderId)
      node.waitOrderStatus(acrylUsdPair, orderId, "Cancelled", 1.minute)
      node.tradableBalance(bob, acrylUsdPair)("ACRYL") shouldBe node.accountBalances(bob.address)._1
    }
  }

  "Alice and Bob trade ACRYL-USD check CELLING" - {
    val price2           = 289
    val buyOrderAmount2  = 0.07.acryl
    val sellOrderAmount2 = 3.acryl

    val correctedSellAmount2 = correctAmount(sellOrderAmount2, price2)

    "place usd-acryl order" in {
      // Alice wants to sell USD for Acryl
      val bobAcrylBalanceBefore = node.accountBalances(bob.address)._1
      node.tradableBalance(bob, acrylUsdPair)("ACRYL")
      val bobOrder1   = node.prepareOrder(bob, acrylUsdPair, OrderType.SELL, sellOrderAmount2, price2)
      val bobOrder1Id = node.placeOrder(bobOrder1).message.id
      node.waitOrderStatus(acrylUsdPair, bobOrder1Id, "Accepted", 1.minute)

      node.reservedBalance(bob)("ACRYL") shouldBe correctedSellAmount2 + matcherFee
      node.tradableBalance(bob, acrylUsdPair)("ACRYL") shouldBe bobAcrylBalanceBefore - (correctedSellAmount2 + matcherFee)

      val aliceOrder   = node.prepareOrder(alice, acrylUsdPair, OrderType.BUY, buyOrderAmount2, price2)
      val aliceOrderId = node.placeOrder(aliceOrder).message.id
      node.waitOrderStatus(acrylUsdPair, aliceOrderId, "Filled", 1.minute)

      // Bob wants to buy some USD
      node.waitOrderStatus(acrylUsdPair, bobOrder1Id, "PartiallyFilled", 1.minute)

      // Each side get fair amount of assets
      node.waitOrderInBlockchain(aliceOrder.idStr())
      node.cancelOrder(bob, acrylUsdPair, bobOrder1Id)
    }

  }

  "Alice and Bob trade WCT-USD sell price less than buy price" - {
    "place wcd-usd order corrected by new price sell amount less then initial one" in {
      val buyPrice   = 247700
      val sellPrice  = 135600
      val buyAmount  = 46978
      val sellAmount = 56978

      val bobOrderId = node.placeOrder(bob, wctUsdPair, SELL, sellAmount, sellPrice, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, bobOrderId, "Accepted", 1.minute)
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, buyAmount, buyPrice, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, aliceOrderId, "Filled", 1.minute)

      node.waitOrderInBlockchain(aliceOrderId)
      node.cancelOrder(bob, wctUsdPair, bobOrderId)

      node.waitOrderStatus(wctUsdPair, bobOrderId, "Cancelled", 1.minute)

      node.reservedBalance(bob) shouldBe empty
      node.reservedBalance(alice) shouldBe empty
    }
  }

  "Alice and Bob trade WCT-USD 1" - {
    val wctUsdSellAmount = 347
    val wctUsdBuyAmount  = 146
    val wctUsdPrice      = 12739213

    "place wct-usd order" in {
      val aliceUsdBalance = node.assetBalance(alice.address, UsdId.toString).balance
      val bobUsdBalance = node.assetBalance(bob.address, UsdId.toString).balance
      val bobWctInitBalance = node.assetBalance(bob.address, WctId.toString).balance

      val bobOrderId =
        node.placeOrder(bob, wctUsdPair, SELL, wctUsdSellAmount, wctUsdPrice, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, bobOrderId, "Accepted", 1.minute)

      val aliceOrderId =
        node.placeOrder(alice, wctUsdPair, BUY, wctUsdBuyAmount, wctUsdPrice, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, aliceOrderId, "Filled", 1.minute)

      node.waitOrderInBlockchain(aliceOrderId)

      val executedAmount         = correctAmount(wctUsdBuyAmount, wctUsdPrice) // 142
      val bobReceiveUsdAmount    = receiveAmount(SELL, wctUsdBuyAmount, wctUsdPrice)
      val expectedReservedBobWct = wctUsdSellAmount - executedAmount // 205 = 347 - 142

      node.reservedBalance(bob)(s"$WctId") shouldBe expectedReservedBobWct
      // 999999999652 = 999999999999 - 142 - 205
      node.tradableBalance(bob, wctUsdPair)(s"$WctId") shouldBe bobWctInitBalance - executedAmount - expectedReservedBobWct
      node.tradableBalance(bob, wctUsdPair)(s"$UsdId") shouldBe bobUsdBalance + bobReceiveUsdAmount

      node.reservedBalance(alice) shouldBe empty
      node.tradableBalance(alice, wctUsdPair)(s"$UsdId") shouldBe aliceUsdBalance - bobReceiveUsdAmount

      val expectedReservedAcryl = matcherFee - LimitOrder.partialFee(matcherFee, wctUsdSellAmount, executedAmount)
      node.reservedBalance(bob)("ACRYL") shouldBe expectedReservedAcryl

      node.cancelOrder(bob, wctUsdPair, node.fullOrderHistory(bob).head.id)
    }

    "reserved balance is empty after the total execution" in {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, 5000000, 100000, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, aliceOrderId, "Accepted", 1.minute)

      val bobOrderId = node.placeOrder(bob, wctUsdPair, SELL, 5000000, 99908, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, bobOrderId, "Filled", 1.minute)
      node.waitOrderStatus(wctUsdPair, aliceOrderId, "Filled", 1.minute)

      node.waitOrderInBlockchain(bobOrderId)
      node.reservedBalance(alice) shouldBe empty
      node.reservedBalance(bob) shouldBe empty
    }

  }

  "get opened trading markets. Check WCT-USD" in {
    val openMarkets = node.tradingMarkets()
    val markets     = openMarkets.markets.last

    markets.amountAssetName shouldBe wctAssetName
    markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))

    markets.priceAssetName shouldBe usdAssetName
    markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
  }

  "Alice and Bob trade WCT-ACRYL on not enough fee when place order" - {
    val wctAcrylSellAmount = 2
    val wctAcrylPrice      = 11234560000000L

    "bob lease all acryl exact half matcher fee" in {
      val leasingAmount = node.accountBalances(bob.address)._1 - leasingFee - matcherFee / 2
      val leaseTxId     = node.broadcastLease(bob, matcher.address, leasingAmount, leasingFee, waitForTx = true).id
      val bobOrderId =
        node.placeOrder(bob, wctAcrylPair, SELL, wctAcrylSellAmount, wctAcrylPrice, matcherFee).message.id
      node.waitOrderStatus(wctAcrylPair, bobOrderId, "Accepted", 1.minute)

      node.tradableBalance(bob, wctAcrylPair)("ACRYL") shouldBe matcherFee / 2 + receiveAmount(SELL, wctAcrylSellAmount, wctAcrylPrice) - matcherFee
      node.cancelOrder(bob, wctAcrylPair, bobOrderId)

      assertBadRequestAndResponse(
        node.placeOrder(bob, wctAcrylPair, SELL, wctAcrylSellAmount / 2, wctAcrylPrice, matcherFee),
        "Not enough tradable balance"
      )

      node.broadcastCancelLease(bob, leaseTxId, leasingFee, waitForTx = true)
    }
  }

  "Alice and Bob trade ETH-ACRYL" - {
    "reserved balance is empty after the total execution" in {
      val counterId1 = node.placeOrder(alice, ethAcrylPair, SELL, 2864310, 300000, matcherFee).message.id
      node.waitOrderStatus(ethAcrylPair, counterId1, "Accepted", 1.minute)

      val counterId2 = node.placeOrder(alice, ethAcrylPair, SELL, 7237977, 300000, matcherFee).message.id
      node.waitOrderStatus(ethAcrylPair, counterId2, "Accepted", 1.minute)

      val submittedId = node.placeOrder(bob, ethAcrylPair, BUY, 4373667, 300000, matcherFee).message.id

      node.waitOrderStatus(ethAcrylPair, counterId1, "Filled", 1.minute)
      node.waitOrderStatus(ethAcrylPair, counterId2, "PartiallyFilled", 1.minute)
      node.waitOrderStatus(ethAcrylPair, submittedId, "Filled", 1.minute)

      node.waitOrderInBlockchain(submittedId)
      node.reservedBalance(bob) shouldBe empty
      node.cancelOrder(alice, ethAcrylPair, counterId2)
    }
  }

  "Submitted order Canceled during match" in {
    val bobOrder   = node.prepareOrder(matcher, acrylUsdPair, OrderType.SELL, 10000000L, 10L)
    val bobOrderId = node.placeOrder(bobOrder).message.id
    node.waitOrderStatus(acrylUsdPair, bobOrderId, "Accepted", 1.minute)

    val aliceOrder   = node.prepareOrder(alice, acrylUsdPair, OrderType.BUY, 100000L, 1000L)
    val aliceOrderId = node.placeOrder(aliceOrder).message.id

    node.waitOrderStatusAndAmount(acrylUsdPair, aliceOrderId, "Cancelled", Some(0), 1.minute)

    withClue("Alice's reserved balance:") {
      node.reservedBalance(alice) shouldBe empty
    }

    val aliceOrders = node.ordersByAddress(alice, activeOnly = false, 1.minute)
    aliceOrders should not be empty

    val order = aliceOrders.find(_.id == aliceOrderId).getOrElse(throw new IllegalStateException(s"Alice should have the $aliceOrderId order"))
    order.status shouldBe "Cancelled"

    node.cancelOrder(matcher, acrylUsdPair, bobOrderId)
  }

  def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def receiveAmount(ot: OrderType, matchAmount: Long, matchPrice: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}
