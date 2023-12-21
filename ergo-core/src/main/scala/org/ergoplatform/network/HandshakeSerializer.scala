package org.ergoplatform.network

import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}

/**
 * The `Handshake` message provides information about the transmitting node
 * to the receiving node at the beginning of a connection. Until both peers
 * have exchanged `Handshake` messages, no other messages will be accepted.
 */
object HandshakeSerializer extends MessageSpecV1[Handshake] {
  override val messageCode: MessageCode = 75: Byte
  override val messageName: String = "Handshake"

  val maxHandshakeSize: Int = 8096

  /**
   * Serializing handshake into a byte writer.
   *
   * @param hs - handshake instance
   * @param w  - writer to write bytes to
   */
  override def serialize(hs: Handshake, w: Writer): Unit = {
    // first writes down handshake time, then peer specification of our node
    w.putULong(hs.time)
    PeerSpecSerializer.serialize(hs.peerSpec, w)
  }

  override def parse(r: Reader): Handshake = {
    require(r.remaining <= maxHandshakeSize, s"Too big handshake. Size ${r.remaining} exceeds $maxHandshakeSize limit")
    val time = r.getULong()
    val data = PeerSpecSerializer.parse(r)
    Handshake(data, time)
  }

}
