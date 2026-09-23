package org.ergoplatform.network.message

import java.nio.ByteOrder
import akka.util.ByteString
import org.ergoplatform.network.message.MessageConstants.MaxMessageSize
import org.ergoplatform.network.message.MessageSerializer.UnknownMessageCodeException
import scorex.core.network.{ConnectedPeer, MaliciousBehaviorException}
import scorex.crypto.hash.Blake2b256

import scala.util.Try
import scala.util.control.NoStackTrace

class MessageSerializer(specs: Seq[MessageSpec[_]], magicBytes: Array[Byte]) {

  import MessageConstants.{ChecksumLength, HeaderLength, MagicLength}

  import scala.language.existentials

  private implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN

  private val specsMap = Map(specs.map(s => s.messageCode -> s): _*)
    .ensuring(m => m.size == specs.size, "Duplicate message codes")

  def serialize[A <: MessageBase[_]](obj: A): ByteString = {
    val builder = ByteString.createBuilder
      .putBytes(magicBytes)
      .putByte(obj.spec.messageCode)
      .putInt(obj.dataLength)

    if (obj.dataLength > 0) {
      val checksum = Blake2b256.hash(obj.dataBytes).take(ChecksumLength)
      builder.putBytes(checksum).putBytes(obj.dataBytes)
    }

    builder.result()
  }

  //MAGIC ++ Array(spec.messageCode) ++ Ints.toByteArray(dataLength) ++ dataWithChecksum
  def deserialize(
    byteString: ByteString,
    sourceOpt: Option[ConnectedPeer]
  ): Try[Option[Message[_]]] = Try {
    if (byteString.length < HeaderLength) {
      None
    } else {
      val it      = byteString.iterator
      val magic   = it.getBytes(MagicLength)
      val msgCode = it.getByte
      val length  = it.getInt

      //peer is trying to cause buffer overflow or breaking the parsing
      if (length < 0) {
        throw MaliciousBehaviorException("Data length is negative!")
      }
      // Reject messages larger than the hard network-wide cap documented in MessageConstants.MaxMessageSize
      if (length > MaxMessageSize) {
        throw MaliciousBehaviorException("Data length is above limit!")
      }

      if (length != 0 && byteString.length < length + HeaderLength + ChecksumLength) {
        None
      } else {
        //peer is from another network
        if (!java.util.Arrays.equals(magic, magicBytes)) {
          throw MaliciousBehaviorException(
            s"Wrong magic bytes, expected ${magicBytes.mkString}, got ${magic.mkString} in : ${byteString.utf8String}"
          )
        }
        val msgData = if (length > 0) {
          val checksum = it.getBytes(ChecksumLength)
          val data     = it.getBytes(length)
          val digest   = Blake2b256.hash(data).take(ChecksumLength)

          //peer reported incorrect checksum
          if (!java.util.Arrays.equals(checksum, digest)) {
            throw MaliciousBehaviorException(
              s"Wrong checksum, expected ${digest.mkString}, got ${checksum.mkString}"
            )
          }
          data
        } else {
          Array.empty[Byte]
        }

        val messageLength = HeaderLength + (if (length > 0) ChecksumLength + length else 0)
        val spec = specsMap.getOrElse(msgCode, throw UnknownMessageCodeException(msgCode, messageLength))
        Some(Message(spec, Left(msgData), sourceOpt))
      }
    }
  }

}

object MessageSerializer {
  /** A complete, valid envelope that this node cannot decode. Its bytes can be skipped safely. */
  final case class UnknownMessageCodeException(messageCode: Byte, messageLength: Int)
    extends Exception(s"No message handler found for $messageCode") with NoStackTrace
}
