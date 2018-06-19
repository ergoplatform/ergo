package org.ergoplatform.bench

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.bench.misc.CrawlerConfig
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.utils.ScorexLogging

class CrawlerActor(c: CrawlerConfig) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[ErgoPersistentModifier]])
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock) =>
      val height = mod.header.height
      if (height % 100 == 0) {
        logger.error(s"GOT ${height} modifiers")
      }
      if (mod.header.height >= c.threshold) {
        log.error("Got enough modifiers.")
        log.warn("Shutting Down")
        System.exit(0)
      }
  }
}

object CrawlerActor {
  def apply(cc: CrawlerConfig)(implicit ac: ActorSystem): ActorRef = ac.actorOf(Props.apply(classOf[CrawlerActor], cc))
}
