package com.acrylplatform.dex

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import cats.kernel.Monoid
import com.acrylplatform.NTPTime
import com.acrylplatform.account.{KeyPair, PublicKey, Address}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.dex.AddressActor.{BalanceUpdated, PlaceOrder}
import com.acrylplatform.dex.db.EmptyOrderDB
import com.acrylplatform.dex.model.LimitOrder
import com.acrylplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.acrylplatform.state.{LeaseBalance, Portfolio}
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV1}
import com.acrylplatform.wallet.Wallet
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class AddressActorSpecification
    extends TestKit(ActorSystem("AddressActorSpecification"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ImplicitSender
    with NTPTime {

  private val assetId    = ByteStr("asset".getBytes("utf-8"))
  private val matcherFee = 30000L

  private val sellTokenOrder1 = OrderV1(
    sender = privateKey("test"),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Acryl, IssuedAsset(assetId)),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = 1L,
    expiration = 1000L,
    matcherFee = matcherFee
  )

  private val sellToken1Portfolio = requiredPortfolio(sellTokenOrder1)

  private val sellTokenOrder2 = OrderV1(
    sender = privateKey("test"),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Acryl, IssuedAsset(assetId)),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = 2L,
    expiration = 1000L,
    matcherFee = matcherFee
  )

  private val sellToken2Portfolio = requiredPortfolio(sellTokenOrder2)

  private val sellAcrylOrder = OrderV1(
    sender = privateKey("test"),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Acryl, IssuedAsset(assetId)),
    orderType = OrderType.SELL,
    price = 100000000L,
    amount = 100L,
    timestamp = 3L,
    expiration = 1000L,
    matcherFee = matcherFee
  )

  private val sellAcrylPortfolio = requiredPortfolio(sellAcrylOrder)

  "AddressActorSpecification" should {
    "cancel orders" when {
      "asset balance changed" in test { (ref, eventsProbe, updatePortfolio) =>
        val initPortfolio = sellToken1Portfolio
        updatePortfolio(initPortfolio, false)

        ref ! PlaceOrder(sellTokenOrder1)
        eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder1))

        updatePortfolio(initPortfolio.copy(assets = Map.empty), true)
        eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id()))
      }

      "acryl balance changed" when {
        "there are acryl for fee" in acrylBalanceTest(restAcryl = matcherFee)
        "there are no acryl at all" in acrylBalanceTest(restAcryl = 0L)

        def acrylBalanceTest(restAcryl: Long): Unit = test { (ref, eventsProbe, updatePortfolio) =>
          val initPortfolio = sellAcrylPortfolio
          updatePortfolio(initPortfolio, false)

          ref ! PlaceOrder(sellAcrylOrder)
          eventsProbe.expectMsg(QueueEvent.Placed(sellAcrylOrder))

          updatePortfolio(initPortfolio.copy(balance = restAcryl), true)
          eventsProbe.expectMsg(QueueEvent.Canceled(sellAcrylOrder.assetPair, sellAcrylOrder.id()))
        }
      }

      "acryl were leased" when {
        "there are acryl for fee" in leaseTest(_ => matcherFee)
        "there are no acryl at all" in leaseTest(_.spendableBalance)

        def leaseTest(leasedAcryl: Portfolio => Long): Unit = test { (ref, eventsProbe, updatePortfolio) =>
          val initPortfolio = sellAcrylPortfolio
          updatePortfolio(initPortfolio, false)

          ref ! PlaceOrder(sellAcrylOrder)
          eventsProbe.expectMsg(QueueEvent.Placed(sellAcrylOrder))

          updatePortfolio(initPortfolio.copy(lease = LeaseBalance(0, leasedAcryl(initPortfolio))), true)
          eventsProbe.expectMsg(QueueEvent.Canceled(sellAcrylOrder.assetPair, sellAcrylOrder.id()))
        }
      }
    }

    "track canceled orders and don't cancel more on same BalanceUpdated message" in test { (ref, eventsProbe, updatePortfolio) =>
      val initPortfolio = Monoid.combine(sellToken1Portfolio, sellToken2Portfolio)
      updatePortfolio(initPortfolio, false)

      ref ! PlaceOrder(sellTokenOrder1)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder1))

      ref ! PlaceOrder(sellTokenOrder2)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder2))

      updatePortfolio(sellToken1Portfolio, true)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id()))

      updatePortfolio(sellToken1Portfolio, true) // same event
      eventsProbe.expectNoMessage()
    }

    "cancel multiple orders" in test { (ref, eventsProbe, updatePortfolio) =>
      val initPortfolio = Monoid.combineAll(Seq(sellToken1Portfolio, sellToken2Portfolio, sellAcrylPortfolio))
      updatePortfolio(initPortfolio, false)

      ref ! PlaceOrder(sellTokenOrder1)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder1))

      ref ! PlaceOrder(sellTokenOrder2)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder2))

      updatePortfolio(sellAcrylPortfolio, true)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id()))
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id()))
    }

    "cancel only orders, those aren't fit" in test { (ref, eventsProbe, updatePortfolio) =>
      val initPortfolio = Monoid.combineAll(Seq(sellToken1Portfolio, sellToken2Portfolio, sellAcrylPortfolio))
      updatePortfolio(initPortfolio, false)

      ref ! PlaceOrder(sellTokenOrder1)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder1))

      ref ! PlaceOrder(sellAcrylOrder)
      eventsProbe.expectMsg(QueueEvent.Placed(sellAcrylOrder))

      ref ! PlaceOrder(sellTokenOrder2)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder2))

      updatePortfolio(sellAcrylPortfolio, true)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id()))
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id()))
    }

    "schedule expired order cancellation" in {
      pending
    }
  }

  /**
    * (updatedPortfolio: Portfolio, sendBalanceChanged: Boolean) => Unit
    */
  private def test(f: (ActorRef, TestProbe, (Portfolio, Boolean) => Unit) => Unit): Unit = {
    val eventsProbe      = TestProbe()
    val currentPortfolio = new AtomicReference[Portfolio]()
    val address          = addr("test")
    val addressActor = system.actorOf(
      Props(
        new AddressActor(
          address,
          x => currentPortfolio.get().spendableBalanceOf(x),
          1.day,
          ntpTime,
          EmptyOrderDB,
          _ => false,
          event => {
            eventsProbe.ref ! event
            Future.successful(Some(QueueEventWithMeta(0, 0, event)))
          },
          false
        )))
    f(
      addressActor,
      eventsProbe,
      (updatedPortfolio, notify) => {
        val prevPortfolio = currentPortfolio.getAndSet(updatedPortfolio)
        if (notify) addressActor ! BalanceUpdated(prevPortfolio.changedAssetIds(updatedPortfolio))
      }
    )
    addressActor ! PoisonPill
  }

  private def requiredPortfolio(order: Order): Portfolio = {
    val b = LimitOrder(order).requiredBalance
    Portfolio(b.getOrElse(Acryl, 0L), LeaseBalance.empty, b.collect { case (id @ IssuedAsset(_), v) => id -> v })
  }

  private def addr(seed: String): Address              = privateKey(seed).toAddress
  private def privateKey(seed: String): KeyPair = Wallet.generateNewAccount(seed.getBytes("utf-8"), 0)

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }
}
