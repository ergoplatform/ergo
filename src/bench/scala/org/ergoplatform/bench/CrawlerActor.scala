package org.ergoplatform.bench

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.bench.misc.CrawlerConfig
import org.ergoplatform.bench.protocol.SubTo
import org.ergoplatform.modifiers.ErgoFullBlock
import scorex.core.NodeViewHolder.ReceivableMessages.Subscribe
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.utils.ScorexLogging

class CrawlerActor(c: CrawlerConfig) extends Actor with ScorexLogging {
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
    case SubTo(ref, events) =>
      ref ! Subscribe(events)
  }
}

object CrawlerActor {
  def apply(cc: CrawlerConfig)(implicit ac: ActorSystem): ActorRef = ac.actorOf(Props.apply(classOf[CrawlerActor], cc))
}
