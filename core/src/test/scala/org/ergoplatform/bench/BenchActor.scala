package org.ergoplatform.bench

import akka.actor.{Actor, ActorRef}
import org.ergoplatform.bench.BenchActor.{Start, Sub}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.core.NodeViewHolder.ReceivableMessages._
import scorex.core.NodeViewHolder._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.core.utils.ScorexLogging

class BenchActor(threshold: Int) extends Actor with ScorexLogging {

  var counter = 0
  var start = 0L
  var finish = 0L

  val fileName = "target/bench/result"

  override def receive: Receive = {
    case Sub(ref) =>
      log.info("Subscribing")
      ref ! Subscribe(Seq(EventType.SuccessfulSemanticallyValidModifier,
        EventType.SuccessfulSyntacticallyValidModifier,
        EventType.SyntacticallyFailedPersistentModifier,
        EventType.SemanticallyFailedPersistentModifier)
      )
    case Start(ref, modifiers) =>
      start = System.currentTimeMillis()
      log.info(s"start is $start")
      modifiers.foreach(m => ref ! m)
    case _: SemanticallySuccessfulModifier[ErgoPersistentModifier] =>
      log.info("SUCC SEM")
      self ! "increase"
    case _: SyntacticallyFailedModification[ErgoPersistentModifier] =>
      log.info("SYN FAIL")
      self ! "increase"
    case _: SemanticallyFailedModification[ErgoPersistentModifier] =>
      log.info("SEM FAIL")
      self ! "increase"
    case "increase" =>
      counter += 1
      if (counter % 100 == 0 ) {log.error(s"counter is $counter")}
      if (counter >= threshold) {
        finish = System.currentTimeMillis()
        val seconds = (finish - start) / 1000
        log.info(s"start is $start")
        log.info(s"finish is $finish")
        log.info(s"FINISHED APPLYING $threshold MODIFIERS in $seconds seconds.")
        FileWriter.writeToFile(s"$fileName$threshold.csv", Result(finish, seconds))
        System.exit(0)
      }
  }
}

object BenchActor {
  case class Sub(actorRef: ActorRef)
  case class Start(actorRef: ActorRef, modifiers: Vector[ModifiersFromRemote])
}
