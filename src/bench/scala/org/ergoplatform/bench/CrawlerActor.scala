package org.ergoplatform.bench

import java.io.FileOutputStream

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.bench.misc.{CrawlerConfig, ModifierWriter}
import org.ergoplatform.bench.protocol.SubTo
import scorex.core.NodeViewHolder.{Subscribe, SyntacticallySuccessfulModifier}
import scorex.core.utils.ScorexLogging

class CrawlerActor(c: CrawlerConfig) extends Actor with ScorexLogging {

  var counter = 0
  val writer = new FileOutputStream(c.file)

  override def receive: Receive = {
    case SyntacticallySuccessfulModifier(mod) =>
      if (counter >= c.threshold) {
        log.warn("Shutting Down")
        writer.flush()
        writer.close()
        System.exit(0)
      } else {
        counter += 1
        if ( counter % 100 == 0 ) { log.info(s"GOT ${counter} modifiers") }
        ModifierWriter.write(mod)(writer)
      }
    case SubTo(ref, events) =>
      ref ! Subscribe(events)
  }
}

object CrawlerActor {
  def apply(cc: CrawlerConfig)(implicit ac: ActorSystem): ActorRef = ac.actorOf(Props.apply(classOf[CrawlerActor], cc))
}
