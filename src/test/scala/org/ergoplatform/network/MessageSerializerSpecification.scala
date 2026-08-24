package org.ergoplatform.network

import akka.actor.ActorRef
import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
import org.ergoplatform.network.message.{Message, MessageSerializer, RequestModifierSpec}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.core.network.{ConnectedPeer, ConnectionId, Incoming, MaliciousBehaviorException}

import java.net.InetSocketAddress

class MessageSerializerSpecification extends ErgoCorePropertyTest {

  private val magic = Array(1: Byte, 0: Byte, 2: Byte, 4: Byte)

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

}
