package org.ergoplatform.bench

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.bench.misc.CrawlerConfig
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.FullBlockApplied
import scorex.util.ScorexLogging

class CrawlerActor(c: CrawlerConfig) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[FullBlockApplied])
    ()
  }

  override def receive: Receive = {
    case FullBlockApplied(header) =>
      val height = header.height
      if (height % 100 == 0) logger.info(s"Got $height modifiers")
      if (header.height >= c.threshold) {
        log.error("Got enough modifiers.")
        log.warn("Exiting benchmark..")
        System.exit(0)
      }
  }

}

object CrawlerActor {

  def apply(cc: CrawlerConfig)(implicit ac: ActorSystem): ActorRef = {
    ac.actorOf(Props.apply(classOf[CrawlerActor], cc))
  }

}
