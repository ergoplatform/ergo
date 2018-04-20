package org.ergoplatform.bench

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.bench.protocol.Start
import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class BenchActor(threshold: Int) extends Actor with ScorexLogging {

  implicit val ec: ExecutionContext = context.dispatcher

  var counter = 0
  var start = 0L
  var finish = 0L

  val timeout = 2 hours

  val fileName = "target/bench/result"

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[ErgoPersistentModifier]])
    context.system.scheduler.scheduleOnce(timeout, self, BenchActor.Timeout)

  }

  override def receive: Receive = {
    case Start =>
      start = System.currentTimeMillis()
      log.info(s"start is $start")
    case _: SemanticallySuccessfulModifier[ErgoPersistentModifier] => self ! "increase"
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
    case BenchActor.Timeout =>
      log.error("Bench is taking too long. Shutting down on timeout.")
      System.exit(1)
  }
}

object BenchActor {
  def apply(threshold: Int)(implicit ac: ActorSystem): ActorRef =
    ac.actorOf(Props.apply(classOf[BenchActor], threshold))

  case object Timeout
}

