package org.ergoplatform.bench

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.bench.protocol.{Start, SubTo}
import scorex.core.NodeViewHolder.ReceivableMessages.Subscribe
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.ModificationOutcome
import scorex.core.utils.ScorexLogging

class BenchActor(threshold: Int) extends Actor with ScorexLogging {

  var counter = 0
  var start = 0L
  var finish = 0L

  val fileName = "target/bench/result"

  override def receive: Receive = {
    case SubTo(ref, events) =>
      log.trace(s"Bench actor is subscribig to ${events.mkString(" ")}")
      ref ! Subscribe(events)
    case Start =>
      start = System.currentTimeMillis()
      log.info(s"start is $start")
    case _: ModificationOutcome => self ! "increase"
    case "increase" =>
      counter += 1
      if (counter % 100 == 0 ) {log.error(s"counter is $counter")}
      if (counter >= threshold) {
        finish = System.currentTimeMillis()
        val seconds = (finish - start) / 1000
        log.info(s"start is $start")
        log.info(s"finish is $finish")
        log.info(s"FINISHED APPLYING $threshold MODIFIERS in $seconds seconds.")
        ResultWriter.writeToFile(s"$fileName$threshold.csv", Result(finish, seconds))
        System.exit(0)
      }
  }
}

object BenchActor {
  def apply(threshold: Int)(implicit ac: ActorSystem): ActorRef =
    ac.actorOf(Props.apply(classOf[BenchActor], threshold))
}

