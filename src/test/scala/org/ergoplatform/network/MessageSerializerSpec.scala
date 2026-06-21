package org.ergoplatform.network

import akka.util.ByteString
import org.ergoplatform.network.message.{InvSpec, MessageConstants, MessageSerializer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.network.MaliciousBehaviorException

import java.nio.ByteOrder

class MessageSerializerSpec extends AnyFlatSpec with Matchers {

  private implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN

  private val magic = Array(1: Byte, 0: Byte, 2: Byte, 4: Byte)
  private val serializer = new MessageSerializer(Seq(InvSpec), magic)

  private def messageHeader(length: Int): ByteString =
    ByteString.createBuilder
      .putBytes(magic)
      .putByte(InvSpec.messageCode)
      .putInt(length)
      .result()

  it should "reject oversized payload lengths before buffering message data" in {
    val result = serializer.deserialize(messageHeader(MessageConstants.MaxPayloadLength + 1), sourceOpt = None)

    result.failed.get shouldBe a[MaliciousBehaviorException]
  }

  it should "reject overflowing payload lengths before computing total frame length" in {
    val result = serializer.deserialize(messageHeader(Int.MaxValue), sourceOpt = None)

    result.failed.get shouldBe a[MaliciousBehaviorException]
  }

}
