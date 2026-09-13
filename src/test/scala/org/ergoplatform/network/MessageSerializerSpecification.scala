package org.ergoplatform.network

import akka.actor.ActorRef
import akka.util.ByteString
import org.ergoplatform.network.message.MessageConstants.{HeaderLength, MaxMessageSize}
import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
import org.ergoplatform.network.message.{Message, MessageSerializer, ModifiersSpec, RequestModifierSpec}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.core.network.{ConnectedPeer, ConnectionId, Incoming, MaliciousBehaviorException}

import java.net.InetSocketAddress
import java.nio.ByteOrder

class MessageSerializerSpecification extends ErgoCorePropertyTest {

  private implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  private val magic = Array(1: Byte, 0: Byte, 2: Byte, 4: Byte)
  private val serializer = new MessageSerializer(Seq(ModifiersSpec), magic)

  private def peer(version: Version): ConnectedPeer = {
    val address = new InetSocketAddress("127.0.0.1", 9000 + version.thirdDigit.toInt)
    val connectionId = ConnectionId(address, address, Incoming)
    val peerSpec = PeerSpec("ergo-test", version, "ergo-test", None, Seq.empty)
    ConnectedPeer(connectionId, ActorRef.noSender, Some(PeerInfo(peerSpec, System.currentTimeMillis())))
  }

  property("MessageSerializer rejects input-block messages from peers below SubblocksVersion") {
    val serializer = new MessageSerializer(Seq(OrderingBlockAnnouncementMessageSpec), magic)
    val message = Message(OrderingBlockAnnouncementMessageSpec, Left(Array.emptyByteArray), None)
    val bytes = serializer.serialize(message)

    val result = serializer.deserialize(bytes, Some(peer(Version.Eip37ForkVersion)))

    result.isFailure shouldBe true
    result.failed.get.isInstanceOf[MaliciousBehaviorException] shouldBe true
    result.failed.get.getMessage should include ("below required")
  }

  property("MessageSerializer accepts input-block messages from SubblocksVersion peers") {
    val serializer = new MessageSerializer(Seq(OrderingBlockAnnouncementMessageSpec), magic)
    val message = Message(OrderingBlockAnnouncementMessageSpec, Left(Array.emptyByteArray), None)
    val bytes = serializer.serialize(message)

    val result = serializer.deserialize(bytes, Some(peer(Version.SubblocksVersion)))

    result.get.map(_.spec) shouldBe Some(OrderingBlockAnnouncementMessageSpec)
  }

  property("MessageSerializer keeps initial protocol messages available to legacy peers") {
    val serializer = new MessageSerializer(Seq(RequestModifierSpec), magic)
    val message = Message(RequestModifierSpec, Left(Array.emptyByteArray), None)
    val bytes = serializer.serialize(message)

    val result = serializer.deserialize(bytes, Some(peer(Version.Eip37ForkVersion)))

    result.get.map(_.spec) shouldBe Some(RequestModifierSpec)
  }

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
