package org.ergoplatform.network.message

import akka.actor.DeadLetterSuppression
import scorex.core.network.ConnectedPeer

/**
  * Wrapper for a network message, whether come from external peer or generated locally
  *
  * @param spec   - message specification
  * @param input  - message being wrapped, whether in byte-array form (if from outside),
  *               or structured data (if formed locally)
  * @param source - source peer, if the message is from outside
  * @tparam Content - message data type
  */

case class Message[Content](
  spec: MessageSpec[Content],
  input: Either[Array[Byte], Content],
  source: Option[ConnectedPeer]
) extends MessageBase[Content]
  with DeadLetterSuppression
