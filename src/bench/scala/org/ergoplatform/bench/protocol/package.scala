package org.ergoplatform.bench

import akka.actor.ActorRef
import scorex.core.NodeViewHolder._

package object protocol {

  case class SubTo(actorRef: ActorRef, events: Seq[EventType.Value])

  case object Start

}
