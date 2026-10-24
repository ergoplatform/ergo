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

  property("wrong magic failure does not echo bytes after the declared frame") {
    val wrongMagic = Array(9: Byte, 0: Byte, 2: Byte, 4: Byte)
    val wrongMagicFrame = ByteString.createBuilder
      .putBytes(wrongMagic)
      .putByte(ModifiersSpec.messageCode)
      .putInt(0)
      .result()
    wrongMagicFrame.length shouldBe HeaderLength

    // TCP may deliver a second frame in the same read. Its bytes are not part of this frame.
    val nextFrameBytes = ByteString("\nREMOTE_FORGED_LOG_LINE")
    val result = serializer.deserialize(wrongMagicFrame ++ nextFrameBytes, None)

    result.isFailure shouldBe true
    val failure = result.failed.get
    failure shouldBe a[MaliciousBehaviorException]
    failure.getMessage shouldBe s"Wrong magic bytes, expected ${magic.mkString}, got ${wrongMagic.mkString}"
    failure.getMessage should not include "REMOTE_FORGED_LOG_LINE"
  }
}
