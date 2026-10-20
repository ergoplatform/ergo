package org.ergoplatform.network

import akka.util.ByteString
import org.ergoplatform.network.message.MessageConstants.{HeaderLength, MaxMessageSize}
import org.ergoplatform.network.message.{MessageSerializer, ModifiersSpec}
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.core.network.MaliciousBehaviorException

import java.nio.ByteOrder

class MessageSerializerSpecification extends ErgoCorePropertyTest {

  private implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN

  private val magic = Array(1: Byte, 0: Byte, 2: Byte, 4: Byte)
  private val serializer = new MessageSerializer(Seq(ModifiersSpec), magic)

  private def headerWithLength(length: Int): ByteString = {
    ByteString.createBuilder
      .putBytes(magic)
      .putByte(ModifiersSpec.messageCode)
      .putInt(length)
      .result()
  }

  property("message serializer rejects negative payload length") {
    val result = serializer.deserialize(headerWithLength(-1), None)

    result.isFailure shouldBe true
    result.failed.get shouldBe a[MaliciousBehaviorException]
    result.failed.get.getMessage should include("negative")
  }

  property("message serializer accepts max payload length as an incomplete message") {
    val result = serializer.deserialize(headerWithLength(MaxMessageSize), None)

    result.get shouldBe None
  }

  property("message serializer rejects payload length above max") {
    val result = serializer.deserialize(headerWithLength(MaxMessageSize + 1), None)

    result.isFailure shouldBe true
    result.failed.get shouldBe a[MaliciousBehaviorException]
    result.failed.get.getMessage should include("above limit")
  }

  property("message serializer waits for header before checking payload length") {
    serializer.deserialize(ByteString(Array.fill(HeaderLength - 1)(0.toByte)), None).get shouldBe None
  }
}
