package org.ergoplatform.network.message

import java.nio.ByteBuffer
import org.ergoplatform.network.Version
import org.ergoplatform.network.message.MessageConstants._
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.serialization.VLQByteBufferReader

import scala.util.Try

/**
  * Base trait for app p2p messages in the network
  */
trait MessageSpec[Content] extends ErgoSerializer[Content] {

  /**
    * The p2p protocol version in which this message type first appeared
    */
  val protocolVersion: Version

  /**
    * Code which identifies what message type is contained in the payload
    */

  val messageCode: MessageCode

  /**
    * Name of this message type. For debug purposes only.
    */
  val messageName: String

  /**
    * Whether this framed payload intentionally accepts extension bytes that its
    * current parser does not understand.
    */
  protected def allowTrailingPayloadBytes: Boolean = false

  final def parsePayloadBytesTry(bytes: Array[Byte]): Try[Content] = Try {
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
    val result = parse(reader)
    if (!allowTrailingPayloadBytes) {
      require(reader.remaining == 0, s"Unexpected trailing bytes in $messageName message")
    }
    result
  }

  override def toString: String = s"MessageSpec($messageCode: $messageName)"
}

/**
  * P2p messages, that where implemented since the beginning.
  */
trait MessageSpecV1[Content] extends MessageSpec[Content] {

  override val protocolVersion: Version = Version.initial

}
