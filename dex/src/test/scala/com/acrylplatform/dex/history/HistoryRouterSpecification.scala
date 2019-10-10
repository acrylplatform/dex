package com.acrylplatform.dex.history

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.acrylplatform.NTPTime
import com.acrylplatform.account.{KeyPair, PublicKey}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.dex.MatcherTestData
import com.acrylplatform.dex.history.HistoryRouter.{SaveEvent, SaveOrder}
import com.acrylplatform.dex.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.acrylplatform.dex.model.LimitOrder
import com.acrylplatform.dex.model.MatcherModel.Denormalization
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV1}
import com.acrylplatform.wallet.Wallet
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class HistoryRouterSpecification
    extends TestKit(ActorSystem("AddressActorSpecification"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with NTPTime
    with MatcherTestData {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  def privateKey(seed: String): KeyPair = Wallet.generateNewAccount(seed.getBytes(), 0)

  val assetId    = ByteStr("asset".getBytes)
  val matcherFee = 30000L

  val assetDecimals: Byte = 8
  val acrylDecimals: Byte = 8

  val sender0Seed = "test"
  val sender1Seed = "test1"
  val sender2Seed = "test2"
  val sender3Seed = "test3"

  val buyAcrylOrder   = getOrder(sender0Seed, OrderType.BUY, 300L, 1L)
  val sellAcrylOrder1 = getOrder(sender1Seed, OrderType.SELL, 100L, 2L)
  val sellAcrylOrder2 = getOrder(sender2Seed, OrderType.SELL, 100L, 3L)
  val sellAcrylOrder3 = getOrder(sender3Seed, OrderType.SELL, 100L, 4L)

  val buyAcrylOrderCancelled = getOrder(sender0Seed, OrderType.BUY, 300L, 5L)

  val buyAcrylOrderFilledAndCancelled = getOrder(sender0Seed, OrderType.BUY, 300L, 6L)
  val sellAcrylOrder4                 = getOrder(sender1Seed, OrderType.SELL, 100L, 7L)

  val sellAcrylOrderFilling           = getOrder(sender1Seed, OrderType.SELL, 100L, 7L)
  val buyAcrylOrderFilledAfterPlacing = getOrder(sender0Seed, OrderType.BUY, 100L, 8L)

  def getOrder(senderSeed: String, orderType: OrderType, amount: Long, timestamp: Long): LimitOrder = {
    LimitOrder(
      OrderV1(
        sender = privateKey(senderSeed),
        matcher = PublicKey("matcher".getBytes()),
        pair = AssetPair(Acryl, IssuedAsset(assetId)),
        orderType = orderType,
        price = Order.PriceConstant,
        amount = amount * Order.PriceConstant,
        timestamp = timestamp,
        expiration = 1000L,
        matcherFee = matcherFee
      )
    )
  }

  def orderAdded(submitted: LimitOrder): OrderAdded                            = OrderAdded(submitted, ntpTime.getTimestamp())
  def orderExecuted(submitted: LimitOrder, counter: LimitOrder): OrderExecuted = OrderExecuted(submitted, counter, ntpTime.getTimestamp())
  def orderCancelled(submitted: LimitOrder): OrderCanceled                     = OrderCanceled(submitted, false, ntpTime.getTimestamp())

  // don't need to use blockchain in order to find out asset decimals, therefore pair parameter isn't used
  def denormalizeAmountAndFee(value: Long, pair: AssetPair): Double = Denormalization.denormalizeAmountAndFee(value, acrylDecimals)
  def denormalizePrice(value: Long, pair: AssetPair): Double        = Denormalization.denormalizePrice(value, acrylDecimals, assetDecimals)

  implicit class LimitOrderOps(limitOrder: LimitOrder) {
    def orderId: String = limitOrder.order.id().toString
    def senderPublicKey: String = limitOrder.order.senderPublicKey.toString
  }

  case class OrderShortenedInfo(id: String, senderPublicKey: String, side: Byte, price: Double, amount: Double)
  case class EventShortenedInfo(orderId: String, eventType: Byte, filled: Double, totalFilled: Double, status: Byte)

  def getOrderInfo(orderAddedEvent: OrderAdded): OrderShortenedInfo = {
    SaveOrder(orderAddedEvent.order, orderAddedEvent.timestamp)
      .createRecords(denormalizeAmountAndFee, denormalizePrice)
      .map(r => OrderShortenedInfo(r.id, r.senderPublicKey, r.side, r.price, r.amount))
      .head
  }

  def getEventsInfo(event: Event): Set[EventShortenedInfo] = {
    SaveEvent(event)
      .createRecords(denormalizeAmountAndFee, denormalizePrice)
      .map(r => EventShortenedInfo(r.orderId, r.eventType, r.filled, r.totalFilled, r.status))
  }

  "HistoryRouter" should {
    "correctly convert events to records" in {

      import HistoryRouter._

      // place big buy order
      getOrderInfo(orderAdded(buyAcrylOrder)) shouldBe
        OrderShortenedInfo(buyAcrylOrder.orderId, buyAcrylOrder.senderPublicKey, buySide, price = 1, amount = 300)

      // place small sell order 1
      getOrderInfo(orderAdded(sellAcrylOrder1)) shouldBe
        OrderShortenedInfo(sellAcrylOrder1.orderId, sellAcrylOrder1.senderPublicKey, sellSide, price = 1, amount = 100)

      // big buy order executed first time
      val orderExecutedEvent1 = orderExecuted(buyAcrylOrder, sellAcrylOrder1)
      getEventsInfo(orderExecutedEvent1) shouldBe Set(
        EventShortenedInfo(buyAcrylOrder.orderId, eventTrade, filled = 100, totalFilled = 100, statusPartiallyFilled),
        EventShortenedInfo(sellAcrylOrder1.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // place small sell order 2
      getOrderInfo(orderAdded(sellAcrylOrder2)) shouldBe
        OrderShortenedInfo(sellAcrylOrder2.orderId, sellAcrylOrder2.senderPublicKey, sellSide, price = 1, amount = 100)

      // big buy order executed second time
      val orderExecutedEvent2 = orderExecuted(orderExecutedEvent1.submittedRemaining, sellAcrylOrder2)
      getEventsInfo(orderExecutedEvent2) shouldBe Set(
        EventShortenedInfo(buyAcrylOrder.orderId, eventTrade, filled = 100, totalFilled = 200, statusPartiallyFilled),
        EventShortenedInfo(sellAcrylOrder2.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // place small sell order 3
      getOrderInfo(orderAdded(sellAcrylOrder3)) shouldBe
        OrderShortenedInfo(sellAcrylOrder3.orderId, sellAcrylOrder3.senderPublicKey, sellSide, price = 1, amount = 100)

      // big buy order executed third time and filled
      val orderExecutedEvent3 = orderExecuted(orderExecutedEvent2.submittedRemaining, sellAcrylOrder3)
      getEventsInfo(orderExecutedEvent3) shouldBe Set(
        EventShortenedInfo(buyAcrylOrder.orderId, eventTrade, filled = 100, totalFilled = 300, statusFilled),
        EventShortenedInfo(sellAcrylOrder3.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // place order and then cancel
      getOrderInfo(orderAdded(buyAcrylOrderCancelled)) shouldBe
        OrderShortenedInfo(buyAcrylOrderCancelled.orderId, buyAcrylOrderCancelled.senderPublicKey, buySide, price = 1, amount = 300)

      getEventsInfo(orderCancelled(buyAcrylOrderCancelled)) shouldBe Set(
        EventShortenedInfo(buyAcrylOrderCancelled.orderId, eventCancel, filled = 0, totalFilled = 0, statusCancelled),
      )

      // place buy order
      getOrderInfo(orderAdded(buyAcrylOrderFilledAndCancelled)) shouldBe
        OrderShortenedInfo(buyAcrylOrderFilledAndCancelled.orderId, buyAcrylOrderFilledAndCancelled.senderPublicKey, buySide, price = 1, amount = 300)

      // place sell order
      getOrderInfo(orderAdded(sellAcrylOrder4)) shouldBe
        OrderShortenedInfo(sellAcrylOrder4.orderId, sellAcrylOrder4.senderPublicKey, sellSide, price = 1, amount = 100)

      // buy order partially filled
      val cancellingOrderExecutedEvent = orderExecuted(buyAcrylOrderFilledAndCancelled, sellAcrylOrder4)
      getEventsInfo(cancellingOrderExecutedEvent) shouldBe Set(
        EventShortenedInfo(buyAcrylOrderFilledAndCancelled.orderId, eventTrade, filled = 100, totalFilled = 100, statusPartiallyFilled),
        EventShortenedInfo(sellAcrylOrder4.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // buy order cancelled
      getEventsInfo(orderCancelled(cancellingOrderExecutedEvent.submittedRemaining)) shouldBe Set(
        EventShortenedInfo(buyAcrylOrderFilledAndCancelled.orderId, eventCancel, filled = 0, totalFilled = 100, statusCancelled),
      )
    }
  }
}
