package org.ergoplatform

import akka.actor.{Actor, ActorRef, Props, Terminated}
import scorex.util.ScorexLogging

/**
  * Watches a set of critical actors. If any of them terminates, triggers a
  * coordinated shutdown of the whole node so storage and network are closed cleanly
  * rather than leaving the node running in a broken state.
  *
  * Additional actors can be added at runtime via [[CriticalActorsWatcher.Watch]],
  * which is useful for refs that are only available after another actor has been
  * constructed (e.g. ErgoNodeViewSynchronizer, which is created lazily inside
  * NetworkController's message handler closure).
  */
class CriticalActorsWatcher(initial: Seq[ActorRef]) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    initial.foreach(context.watch)
    log.info(
      s"CriticalActorsWatcher watching ${initial.size} actors: " +
        initial.map(_.path.name).mkString(", ")
    )
  }

  override def receive: Receive = {
    case CriticalActorsWatcher.Watch(ref) =>
      context.watch(ref)
      log.info(s"CriticalActorsWatcher now also watching ${ref.path.name}")

    case Terminated(ref) =>
      log.error(s"Critical actor terminated: ${ref.path} - initiating coordinated shutdown")
      ErgoApp.shutdownSystem()(context.system)
  }
}

object CriticalActorsWatcher {

  /** Add another actor to the watched set. */
  final case class Watch(ref: ActorRef)

  def props(initial: Seq[ActorRef]): Props =
    Props(new CriticalActorsWatcher(initial))
}
