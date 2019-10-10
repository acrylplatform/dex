package com.acrylplatform.dex

import akka.actor.{Actor, ActorRef, Cancellable, Props, Terminated}
import com.acrylplatform.dex.WatchDistributedCompletionActor.TimedOut
import com.acrylplatform.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

class WatchDistributedCompletionActor(workers: Set[ActorRef],
                                      completionReceiver: ActorRef,
                                      startWorkCommand: Any,
                                      workCompleted: Any,
                                      timeout: FiniteDuration)
    extends Actor
    with ScorexLogging {

  import context.dispatcher

  if (workers.isEmpty) stop(Cancellable.alreadyCancelled)
  else
    workers.foreach { x =>
      context.watch(x)
      x ! startWorkCommand
    }

  override def receive: Receive = state(workers, context.system.scheduler.scheduleOnce(timeout, self, TimedOut))

  private def state(rest: Set[ActorRef], timer: Cancellable): Receive = {
    case `workCompleted` =>
      switchTo(rest - sender(), timer)
      context.unwatch(sender())

    case Terminated(ref) =>
      switchTo(rest - ref, timer)

    case TimedOut =>
      val workerPairs = workers.iterator.map(_.path.name).mkString(", ")
      log.error(s"PingAll is timed out! Pairs those didn't respond: $workerPairs")
      stop(timer)
  }

  private def switchTo(updatedRest: Set[ActorRef], timer: Cancellable): Unit =
    if (updatedRest.isEmpty) stop(timer) else context.become(state(updatedRest, timer))

  private def stop(timer: Cancellable): Unit = {
    timer.cancel()
    completionReceiver ! workCompleted
    context.stop(self)
  }
}

object WatchDistributedCompletionActor {
  def props(workers: Set[ActorRef], completionReceiver: ActorRef, startWorkCommand: Any, workCompleted: Any, timeout: FiniteDuration): Props =
    Props(new WatchDistributedCompletionActor(workers, completionReceiver, startWorkCommand, workCompleted, timeout))

  private object TimedOut
}
