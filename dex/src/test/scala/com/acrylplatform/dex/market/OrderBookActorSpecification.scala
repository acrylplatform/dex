package com.acrylplatform.dex.market

import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorRef
import akka.testkit.{ImplicitSender, TestActorRef, TestProbe}
import cats.data.NonEmptyList
import com.acrylplatform.NTPTime
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.dex.MatcherTestData
import com.acrylplatform.dex.api.AlreadyProcessed
import com.acrylplatform.dex.db.OrderBookSnapshotDB
import com.acrylplatform.dex.fixtures.RestartableActor
import com.acrylplatform.dex.fixtures.RestartableActor.RestartActor
import com.acrylplatform.dex.market.MatcherActor.SaveSnapshot
import com.acrylplatform.dex.market.OrderBookActor._
import com.acrylplatform.dex.model.Events.{OrderAdded, OrderCanceled}
import com.acrylplatform.dex.model._
import com.acrylplatform.dex.queue.QueueEvent.Canceled
import com.acrylplatform.dex.settings.{MatchingRules, RawMatchingRules}
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.OrderOps._
import com.acrylplatform.transaction.assets.exchange.{AssetPair, Order}
import com.acrylplatform.utils.EmptyBlockchain
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration._
import scala.util.Random

class OrderBookActorSpecification
    extends MatcherSpec("OrderBookActor")
    with NTPTime
    with ImplicitSender
    with MatcherTestData
    with PathMockFactory
    with Eventually {

  private val txFactory = new ExchangeTransactionCreator(EmptyBlockchain, MatcherAccount, matcherSettings).createTransaction _
  private val obc       = new ConcurrentHashMap[AssetPair, OrderBook.AggregatedSnapshot]
  private val md        = new ConcurrentHashMap[AssetPair, MarketStatus]

  private def update(ap: AssetPair)(snapshot: OrderBook.AggregatedSnapshot): Unit = obc.put(ap, snapshot)

  private def obcTest(f: (AssetPair, TestActorRef[OrderBookActor with RestartableActor], TestProbe) => Unit): Unit =
    obcTestWithPrepare((_, _) => ()) { (pair, actor, probe) =>
      probe.expectMsg(OrderBookRecovered(pair, None))
      f(pair, actor, probe)
    }

  private def obcTestWithTickSize(tickSize: Double)(f: (AssetPair, ActorRef, TestProbe) => Unit): Unit =
    obcTestWithPrepare((_, _) => (), NonEmptyList(RawMatchingRules(0L, tickSize), List.empty)) { (pair, actor, probe) =>
      probe.expectMsg(OrderBookRecovered(pair, None))
      f(pair, actor, probe)
    }

  private def obcTestWithMatchingRules(matchingRules: NonEmptyList[RawMatchingRules])(f: (AssetPair, ActorRef, TestProbe) => Unit): Unit =
    obcTestWithPrepare((_, _) => (), matchingRules) { (pair, actor, probe) =>
      probe.expectMsg(OrderBookRecovered(pair, None))
      f(pair, actor, probe)
    }

  private def obcTestWithPrepare(prepare: (OrderBookSnapshotDB, AssetPair) => Unit,
                                 matchingRules: NonEmptyList[RawMatchingRules] = NonEmptyList.one(RawMatchingRules(0, 0.00000001)))(
      f: (AssetPair, TestActorRef[OrderBookActor with RestartableActor], TestProbe) => Unit): Unit = {
    obc.clear()
    md.clear()
    val b = ByteStr(new Array[Byte](32))
    Random.nextBytes(b.arr)

    val tp    = TestProbe()
    val pair  = AssetPair(IssuedAsset(b), Acryl)
    val obsdb = OrderBookSnapshotDB.inMem
    prepare(obsdb, pair)
    val actor = TestActorRef(
      new OrderBookActor(
        tp.ref,
        tp.ref,
        system.actorOf(OrderBookSnapshotStoreActor.props(obsdb)),
        pair,
        update(pair),
        p => Option(md.get(p)),
        _ => (),
        raw => MatchingRules(raw.startOffset, (BigDecimal(raw.tickSize) * BigDecimal(10).pow(8)).toLongExact),
        txFactory,
        ntpTime,
        matchingRules
      ) with RestartableActor)

    f(pair, actor, tp)
    system.stop(actor)
  }

  "OrderBookActor" should {
    "recover from snapshot - 1" in obcTestWithPrepare((_, _) => ()) { (pair, _, tp) =>
      tp.expectMsg(OrderBookRecovered(pair, None))
    }

    "recover from snapshot - 2" in obcTestWithPrepare { (obsdb, p) =>
      obsdb.update(p, 50, Some(OrderBook.empty.snapshot))
    } { (pair, _, tp) =>
      tp.expectMsg(OrderBookRecovered(pair, Some(50)))
    }

    "recovery - notify address actor about orders" in obcTestWithPrepare(
      { (obsdb, p) =>
        val ord = buy(p, 10 * Order.PriceConstant, 100)
        val ob  = OrderBook.empty
        ob.add(ord, ord.timestamp)
        obsdb.update(p, 50, Some(ob.snapshot))
      }
    ) { (pair, _, tp) =>
      tp.expectMsgType[OrderAdded]
      tp.expectMsg(OrderBookRecovered(pair, Some(50)))
    }

    "place buy and sell order to the order book and preserve it after restart" in obcTest { (pair, orderBook, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 150)

      orderBook ! wrap(ord1)
      orderBook ! wrap(ord2)
      tp.receiveN(2)

      orderBook ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      orderBook ! RestartActor

      tp.receiveN(2) shouldEqual Seq(ord1, ord2).map(o => OrderAdded(LimitOrder(o), o.timestamp))
      tp.expectMsgType[OrderBookRecovered]
    }

    "execute partial market orders and preserve remaining after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 100)

      actor ! wrap(ord1)
      actor ! wrap(ord2)

      tp.receiveN(3)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]

      actor ! RestartActor
      tp.expectMsg(
        OrderAdded(SellLimitOrder(
                     ord2.amount - ord1.amount,
                     ord2.matcherFee - LimitOrder.partialFee(ord2.matcherFee, ord2.amount, ord1.amount),
                     ord2
                   ),
                   ord2.timestamp)
      )
      tp.expectMsgType[OrderBookRecovered]
    }

    "execute one order fully and other partially and restore after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = buy(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 12 * Order.PriceConstant, 100)

      actor ! wrap(ord1)
      actor ! wrap(ord2)
      actor ! wrap(ord3)
      tp.receiveN(4)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount - ord3.amount
      tp.expectMsg(
        OrderAdded(BuyLimitOrder(
                     restAmount,
                     ord2.matcherFee - LimitOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
                     ord2
                   ),
                   ord2.timestamp)
      )
      tp.expectMsgType[OrderBookRecovered]
    }

    "match multiple best orders at once and restore after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 5 * Order.PriceConstant, 90)
      val ord4 = buy(pair, 19 * Order.PriceConstant, 100)

      actor ! wrap(ord1)
      actor ! wrap(ord2)
      actor ! wrap(ord3)
      actor ! wrap(ord4)
      tp.receiveN(6)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount + ord3.amount - ord4.amount
      tp.expectMsg(
        OrderAdded(
          SellLimitOrder(
            restAmount,
            ord2.matcherFee - LimitOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
            ord2
          ),
          ord2.timestamp
        ))
      tp.expectMsgType[OrderBookRecovered]
    }

    "place orders and restart without waiting for response" in obcTest { (pair, orderBook, tp) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ts   = System.currentTimeMillis()

      (1 to 100) foreach { i =>
        orderBook ! wrap(ord1.updateTimestamp(ts + i))
      }

      within(10.seconds) {
        tp.receiveN(100)
      }

      orderBook ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      orderBook ! RestartActor

      within(10.seconds) {
        tp.receiveN(100)
      }
      tp.expectMsgType[OrderBookRecovered]
    }

    "ignore outdated requests" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      all(receiveN(10)) shouldBe AlreadyProcessed
    }

    "respond on SaveSnapshotCommand" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      tp.expectMsg(OrderBookSnapshotUpdateCompleted(pair, Some(10)))

      (11 to 20).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(20)
      tp.expectMsg(OrderBookSnapshotUpdateCompleted(pair, Some(20)))
    }

    "don't do a snapshot if there is no changes" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      actor ! SaveSnapshot(10)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      tp.expectNoMessage(200.millis)
    }

    "restore its state at start" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
    }

    "cancel order in merge small prices mode" in obcTestWithTickSize(100) { (pair, orderBook, tp) =>
      val buyOrder = buy(pair, 100000000, 0.0000041)

      orderBook ! wrap(1, buyOrder)
      tp.expectMsgType[OrderAdded]

      orderBook ! wrap(2, Canceled(buyOrder.assetPair, buyOrder.id()))
      tp.expectMsgType[OrderCanceled]
    }

    val switchRulesTest = NonEmptyList(
      RawMatchingRules(0, 0.00000001),
      List(
        RawMatchingRules(4, 0.000001),
        RawMatchingRules(10, 0.000003)
      )
    )

    "rules are switched" in obcTestWithMatchingRules(switchRulesTest) { (pair, orderBook, tp) =>
      val buyOrder = buy(pair, 100000000, 0.0000041)
      (0 to 17).foreach { i =>
        orderBook ! wrap(i, buyOrder)
        tp.expectMsgType[OrderAdded]
      }

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 3

        val level41 = bids.head
        level41.price shouldBe buyOrder.price
        level41.amount shouldBe buyOrder.amount * 4

        val level40 = bids(1)
        level40.price shouldBe (0.000004 * Order.PriceConstant)
        level40.amount shouldBe buyOrder.amount * 6

        val level30 = bids(2)
        level30.price shouldBe (0.000003 * Order.PriceConstant)
        level30.amount shouldBe buyOrder.amount * 8
      }
    }

    val disableRulesTest = NonEmptyList(
      RawMatchingRules(0, 0.000001),
      List(
        RawMatchingRules(3, 0.00000001)
      )
    )

    "rules can be disabled" in obcTestWithMatchingRules(disableRulesTest) { (pair, orderBook, tp) =>
      val buyOrder = buy(pair, 100000000, 0.0000041)
      (0 to 10).foreach { i =>
        orderBook ! wrap(i, buyOrder)
        tp.expectMsgType[OrderAdded]
      }

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 2

        val level41 = bids.head
        level41.price shouldBe buyOrder.price
        level41.amount shouldBe buyOrder.amount * 8

        val level40 = bids(1)
        level40.price shouldBe (0.000004 * Order.PriceConstant)
        level40.amount shouldBe buyOrder.amount * 3
      }
    }

    val matchingRulesForCancelTest = NonEmptyList(
      RawMatchingRules(0, 0.00000001),
      List(
        RawMatchingRules(0, 0.00000001),
        RawMatchingRules(0, 0.000001)
      )
    )

    "correctly cancel order when rules are switched" in obcTestWithMatchingRules(matchingRulesForCancelTest) { (pair, orderBook, tp) =>
      val buyOrder1, buyOrder2 = buy(pair, 100000000, 0.0000041)

      orderBook ! wrap(0, buyOrder1) // order book places order to the price level 41
      tp.expectMsgType[OrderAdded]

      orderBook ! wrap(1, buyOrder2) // now order book places the same order to the price level 40
      tp.expectMsgType[OrderAdded]

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 2
        bids.head.price shouldBe buyOrder1.price
        bids.last.price shouldBe 0.0000040 * Order.PriceConstant
      }

      orderBook ! wrap(2, Canceled(buyOrder1.assetPair, buyOrder1.id())) // order book is looking for the price level of buyOrder1 correctly (41 but not 40)
      tp.expectMsgType[OrderCanceled]

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 1
        bids.head.price shouldBe 0.000004 * Order.PriceConstant
      }
    }
  }
}
