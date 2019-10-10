package com.acrylplatform.dex.model

import akka.actor.{ActorRef, ActorSystem, Props}
import com.acrylplatform.account.Address
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.dex.queue.QueueEventWithMeta
import com.acrylplatform.dex.AddressActor
import com.acrylplatform.dex.db.TestOrderDB
import com.acrylplatform.utils.Time

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

class OrderHistoryStub(system: ActorSystem, time: Time) {
  private val refs   = mutable.AnyRefMap.empty[Address, ActorRef]
  private val orders = mutable.AnyRefMap.empty[ByteStr, Address]

  private def actorFor(lo: LimitOrder): ActorRef =
    refs.getOrElseUpdate(
      lo.order.sender,
      system.actorOf(
        Props(
          new AddressActor(
            lo.order.sender,
            _ => 0L,
            5.seconds,
            time,
            new TestOrderDB(100),
            _ => false,
            e => Future.successful(Some(QueueEventWithMeta(0, 0, e))),
            true
          )))
    )

  def ref(sender: Address): ActorRef  = refs(sender)
  def ref(orderId: ByteStr): ActorRef = refs(orders(orderId))

  def process(event: Events.Event): Unit = event match {
    case oa: Events.OrderAdded =>
      orders += oa.order.order.id() -> oa.order.order.sender
      actorFor(oa.order) ! oa

    case ox: Events.OrderExecuted =>
      orders += ox.submitted.order.id() -> ox.submitted.order.sender
      orders += ox.counter.order.id()   -> ox.counter.order.sender
      actorFor(ox.counter) ! ox
      actorFor(ox.submitted) ! ox

    case oc: Events.OrderCanceled =>
      actorFor(oc.limitOrder) ! oc
  }

  def processAll(events: Events.Event*): Unit = events.foreach(process)
}
