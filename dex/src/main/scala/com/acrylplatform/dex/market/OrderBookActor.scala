package com.acrylplatform.dex.market

import akka.actor.{Actor, ActorRef, Props}
import cats.data.NonEmptyList
import cats.instances.option.catsStdInstancesForOption
import cats.syntax.apply._
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.dex.api._
import com.acrylplatform.dex.market.MatcherActor.{ForceStartOrderBook, OrderBookCreated, SaveSnapshot}
import com.acrylplatform.dex.market.OrderBookActor._
import com.acrylplatform.dex.model.Events.{Event, ExchangeTransactionCreated, OrderAdded}
import com.acrylplatform.dex.model.ExchangeTransactionCreator.CreateTransaction
import com.acrylplatform.dex.model.OrderBook.LastTrade
import com.acrylplatform.dex.model._
import com.acrylplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.acrylplatform.dex.settings.{MatcherSettings, MatchingRules, RawMatchingRules}
import com.acrylplatform.dex.util.WorkingStash
import com.acrylplatform.metrics.TimerExt
import com.acrylplatform.transaction.assets.exchange._
import com.acrylplatform.utils.{LoggerFacade, ScorexLogging, Time}
import kamon.Kamon
import org.slf4j.LoggerFactory
import play.api.libs.json._

class OrderBookActor(owner: ActorRef,
                     addressActor: ActorRef,
                     snapshotStore: ActorRef,
                     assetPair: AssetPair,
                     updateSnapshot: OrderBook.AggregatedSnapshot => Unit,
                     updateMarketStatus: MarketStatus => Unit,
                     updateMatchingRules: RawMatchingRules => Unit,
                     normalize: RawMatchingRules => MatchingRules,
                     createTransaction: CreateTransaction,
                     time: Time,
                     var matchingRules: NonEmptyList[RawMatchingRules])
    extends Actor
    with WorkingStash
    with ScorexLogging {

  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"OrderBookActor[$assetPair]"))

  private var savingSnapshot          = Option.empty[QueueEventWithMeta.Offset]
  private var lastSavedSnapshotOffset = Option.empty[QueueEventWithMeta.Offset]
  private var lastProcessedOffset     = Option.empty[QueueEventWithMeta.Offset]

  private val addTimer    = Kamon.timer("matcher.orderbook.add").refine("pair" -> assetPair.toString)
  private val cancelTimer = Kamon.timer("matcher.orderbook.cancel").refine("pair" -> assetPair.toString)
  private var orderBook   = OrderBook.empty

  private var actualRules: MatchingRules = normalize(matchingRules.head)
  private def update(updated: NonEmptyList[RawMatchingRules]): Unit = if (matchingRules.head != updated.head) {
    matchingRules = updated
    updateMatchingRules(matchingRules.head)
    actualRules = normalize(matchingRules.head)
  }

  override def receive: Receive = recovering

  private def recovering: Receive = {
    case OrderBookSnapshotStoreActor.Response.GetSnapshot(result) =>
      result.foreach { case (_, snapshot) => orderBook = OrderBook(snapshot) }

      lastSavedSnapshotOffset = result.map(_._1)
      lastProcessedOffset = lastSavedSnapshotOffset

      log.debug(lastSavedSnapshotOffset match {
        case None    => "Recovery completed"
        case Some(x) => s"Recovery completed at $x: $orderBook"
      })

      lastProcessedOffset.foreach { x =>
        update(RawMatchingRules.skipOutdated(x, matchingRules))
      }

      updateMarketStatus(MarketStatus(orderBook))
      updateSnapshot(orderBook.aggregatedSnapshot)
      processEvents(orderBook.allOrders.map { case (_, lo) => OrderAdded(lo, lo.order.timestamp) })

      owner ! OrderBookRecovered(assetPair, lastSavedSnapshotOffset)
      context.become(working)
      unstashAll()

    case x => stash(x)
  }

  private def working: Receive = {
    case request: QueueEventWithMeta =>
      update(RawMatchingRules.skipOutdated(request.offset, matchingRules))
      lastProcessedOffset match {
        case Some(lastProcessed) if request.offset <= lastProcessed => sender() ! AlreadyProcessed
        case _ =>
          lastProcessedOffset = Some(request.offset)
          request.event match {
            case x: QueueEvent.Placed   => onAddOrder(request, x.order)
            case x: QueueEvent.Canceled => onCancelOrder(request, x.orderId)
            case _: QueueEvent.OrderBookDeleted =>
              updateSnapshot(OrderBook.AggregatedSnapshot())
              processEvents(orderBook.cancelAll(request.timestamp))
              // We don't delete the snapshot, because it could be required after restart
              // snapshotStore ! OrderBookSnapshotStoreActor.Message.Delete(assetPair)
              context.stop(self)
          }
      }

    case MatcherActor.Ping => sender() ! MatcherActor.Pong

    case ForceStartOrderBook(p) if p == assetPair =>
      sender() ! OrderBookCreated(assetPair)

    case OrderBookSnapshotStoreActor.Response.Updated(offset) =>
      log.info(s"Snapshot has been saved at offset $offset")
      lastSavedSnapshotOffset = Some(offset)
      owner ! OrderBookSnapshotUpdateCompleted(assetPair, lastSavedSnapshotOffset)
      savingSnapshot = None

    case SaveSnapshot(globalEventNr) =>
      if (savingSnapshot.isEmpty && lastSavedSnapshotOffset.getOrElse(-1L) < globalEventNr) {
        saveSnapshotAt(globalEventNr)
        savingSnapshot = Some(globalEventNr)
      }
  }

  private def processEvents(events: Iterable[Event]): Unit = {
    updateMarketStatus(MarketStatus(orderBook))
    updateSnapshot(orderBook.aggregatedSnapshot)

    events.foreach { e =>
      e match {
        case Events.OrderAdded(order, _) =>
          log.info(s"OrderAdded(${order.order.id()}, amount=${order.amount})")
        case x @ Events.OrderExecuted(submitted, counter, timestamp) =>
          log.info(s"OrderExecuted(s=${submitted.order.idStr()}, c=${counter.order.idStr()}, amount=${x.executedAmount})")
          createTransaction(submitted, counter, timestamp) match {
            case Right(tx) => context.system.eventStream.publish(ExchangeTransactionCreated(tx))
            case Left(ex) =>
              log.warn(s"""Can't create tx: $ex
                   |o1: (amount=${submitted.amount}, fee=${submitted.fee}): ${Json.prettyPrint(submitted.order.json())}
                   |o2: (amount=${counter.amount}, fee=${counter.fee}): ${Json.prettyPrint(counter.order.json())}""".stripMargin)
          }
        case Events.OrderCanceled(order, unmatchable, _) =>
          log.info(s"OrderCanceled(${order.order.idStr()}, system=$unmatchable)")
      }

      addressActor ! e
    }
  }

  private def onCancelOrder(event: QueueEventWithMeta, orderIdToCancel: ByteStr): Unit =
    cancelTimer.measure(orderBook.cancel(orderIdToCancel, event.timestamp) match {
      case Some(cancelEvent) =>
        processEvents(List(cancelEvent))
      case None =>
        log.warn(s"Error applying $event: order not found")
    })

  private def onAddOrder(eventWithMeta: QueueEventWithMeta, order: Order): Unit = addTimer.measure {
    log.trace(s"Applied $eventWithMeta, trying to match ...")
    processEvents(orderBook.add(order, eventWithMeta.timestamp, actualRules.normalizedTickSize))
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.warn(s"Restarting actor because of $message", reason)
    super.preRestart(reason, message)
  }

  private def saveSnapshotAt(globalEventNr: QueueEventWithMeta.Offset): Unit = {
    val saveSnapshot = (lastSavedSnapshotOffset, lastProcessedOffset).tupled.forall { case (saved, processed) => saved < processed }
    val toSave       = if (saveSnapshot) Some(orderBook.snapshot) else None

    if (saveSnapshot) {
      log.trace(s"About to save snapshot $orderBook")
      log.debug(
        s"Saving both offset and snapshot. Global seqNr=$globalEventNr, local seqNr=$lastProcessedOffset, current offset = $lastSavedSnapshotOffset")
    } else {
      log.debug(s"Saving offset only. Global seqNr=$globalEventNr, local seqNr=$lastProcessedOffset, current offset = $lastSavedSnapshotOffset")
    }

    snapshotStore ! OrderBookSnapshotStoreActor.Message.Update(assetPair, globalEventNr, toSave)
  }

  snapshotStore ! OrderBookSnapshotStoreActor.Message.GetSnapshot(assetPair)
}

object OrderBookActor {
  def props(parent: ActorRef,
            addressActor: ActorRef,
            snapshotStore: ActorRef,
            assetPair: AssetPair,
            updateSnapshot: OrderBook.AggregatedSnapshot => Unit,
            updateMarketStatus: MarketStatus => Unit,
            updateMatchingRules: RawMatchingRules => Unit,
            normalize: RawMatchingRules => MatchingRules,
            settings: MatcherSettings,
            createTransaction: CreateTransaction,
            time: Time,
            matchingRules: NonEmptyList[RawMatchingRules]): Props =
    Props(
      new OrderBookActor(
        parent,
        addressActor,
        snapshotStore,
        assetPair,
        updateSnapshot,
        updateMarketStatus,
        updateMatchingRules,
        normalize,
        createTransaction,
        time,
        matchingRules
      ))

  def name(assetPair: AssetPair): String = assetPair.toString

  case class MarketStatus(
      lastTrade: Option[LastTrade],
      bestBid: Option[LevelAgg],
      bestAsk: Option[LevelAgg],
  )

  object MarketStatus {
    implicit val fmt: Writes[MarketStatus] = { ms =>
      Json.obj(
        "lastPrice"  -> ms.lastTrade.map(_.price),
        "lastAmount" -> ms.lastTrade.map(_.amount),
        "lastSide"   -> ms.lastTrade.map(_.side.toString),
        "bid"        -> ms.bestBid.map(_.price),
        "bidAmount"  -> ms.bestBid.map(_.amount),
        "ask"        -> ms.bestAsk.map(_.price),
        "askAmount"  -> ms.bestAsk.map(_.amount)
      )
    }

    def apply(ob: OrderBook): MarketStatus = MarketStatus(ob.getLastTrade, ob.bestBid, ob.bestAsk)
  }

  case class Snapshot(eventNr: Option[Long], orderBook: OrderBook.Snapshot)

  // Internal messages
  case class OrderBookRecovered(assetPair: AssetPair, eventNr: Option[Long])
  case class OrderBookSnapshotUpdateCompleted(assetPair: AssetPair, currentOffset: Option[Long])
}
