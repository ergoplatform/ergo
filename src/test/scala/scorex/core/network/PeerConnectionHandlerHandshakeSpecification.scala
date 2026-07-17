package scorex.core.network

import akka.io.Tcp
import akka.testkit.TestProbe
import akka.util.ByteString
import org.ergoplatform.network.{Handshake, HandshakeSerializer}
import org.ergoplatform.network.message.{
  GetPeersSpec,
  Message,
  MessageSerializer,
  UtxoSnapshotChunkSpec
}
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.core.app.ScorexContext
import scorex.testkit.utils.AkkaFixture

import java.net.InetSocketAddress
import scala.concurrent.Await
import scala.concurrent.duration._

class PeerConnectionHandlerHandshakeSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  property("accept a valid handshake split across TCP chunks") {
    val fixture = new AkkaFixture
    try {
      implicit val system = fixture.system
      implicit val ec = system.dispatcher
      val connection = TestProbe("connection")
      val controller = TestProbe("controller")
      val localAddress = new InetSocketAddress("127.0.0.1", 9053)
      val remoteAddress = new InetSocketAddress("127.0.0.1", 9054)
      val description = ConnectionDescription(
        connection.ref,
        ConnectionId(remoteAddress, localAddress, Incoming),
        Some(localAddress),
        Seq.empty
      )
      val handler = system.actorOf(
        PeerConnectionHandlerRef.props(
          settings.scorexSettings,
          controller.ref,
          ScorexContext(Seq.empty, None, None),
          description
        )
      )
      val bytes =
        HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis()))
      val splitAt = bytes.length / 2

      connection.send(handler, Tcp.Received(ByteString(bytes.take(splitAt))))
      connection.send(handler, Tcp.Received(ByteString(bytes.drop(splitAt))))

      controller.expectMsgType[NetworkController.ReceivableMessages.Handshaked](2.seconds)
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  property("preserve a framed message coalesced with the handshake") {
    val fixture = new AkkaFixture
    try {
      implicit val system = fixture.system
      implicit val ec = system.dispatcher
      val connection = TestProbe("connection")
      val controller = TestProbe("controller")
      val localAddress = new InetSocketAddress("127.0.0.1", 9063)
      val remoteAddress = new InetSocketAddress("127.0.0.1", 9064)
      val description = ConnectionDescription(
        connection.ref,
        ConnectionId(remoteAddress, localAddress, Incoming),
        Some(localAddress),
        Seq.empty
      )
      val scorexContext = ScorexContext(Seq(GetPeersSpec), None, None)
      val handler = system.actorOf(
        PeerConnectionHandlerRef.props(
          settings.scorexSettings,
          controller.ref,
          scorexContext,
          description
        )
      )
      val baseHandshake =
        HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis()))
      val handshake = ByteString(baseHandshake.dropRight(1)) ++
        ByteString(Array[Byte](1, 127, 3, 1, 2, 3))
      val framedMessage =
        new MessageSerializer(Seq(GetPeersSpec), settings.scorexSettings.network.magicBytes)
          .serialize(Message(GetPeersSpec, Right(()), None))

      connection.send(handler, Tcp.Received(handshake ++ framedMessage))

      controller.expectMsgType[NetworkController.ReceivableMessages.Handshaked](2.seconds)
      val received = controller.expectMsgType[Message[_]](2.seconds)
      received.spec shouldBe GetPeersSpec
      controller.expectNoMessage(200.millis)
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  property("apply the handshake size limit only to the handshake prefix") {
    val fixture = new AkkaFixture
    try {
      implicit val system = fixture.system
      implicit val ec = system.dispatcher
      val connection = TestProbe("connection")
      val controller = TestProbe("controller")
      val localAddress = new InetSocketAddress("127.0.0.1", 9073)
      val remoteAddress = new InetSocketAddress("127.0.0.1", 9074)
      val description = ConnectionDescription(
        connection.ref,
        ConnectionId(remoteAddress, localAddress, Incoming),
        Some(localAddress),
        Seq.empty
      )
      val scorexContext = ScorexContext(Seq(UtxoSnapshotChunkSpec), None, None)
      val handler = system.actorOf(
        PeerConnectionHandlerRef.props(
          settings.scorexSettings,
          controller.ref,
          scorexContext,
          description
        )
      )
      val handshake =
        HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis()))
      val framedMessage = new MessageSerializer(
        Seq(UtxoSnapshotChunkSpec),
        settings.scorexSettings.network.magicBytes
      ).serialize(Message(UtxoSnapshotChunkSpec, Right(Array.fill[Byte](9000)(1)), None))

      (handshake.length + framedMessage.length) should be > HandshakeSerializer.maxHandshakeSize
      connection.send(handler, Tcp.Received(ByteString(handshake) ++ framedMessage))

      controller.expectMsgType[NetworkController.ReceivableMessages.Handshaked](2.seconds)
      val received = controller.expectMsgType[Message[_]](2.seconds)
      received.spec shouldBe UtxoSnapshotChunkSpec
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  property("close when an incomplete handshake reaches the size limit") {
    val fixture = new AkkaFixture
    try {
      implicit val system = fixture.system
      implicit val ec = system.dispatcher
      val connection = TestProbe("connection")
      val controller = TestProbe("controller")
      val localAddress = new InetSocketAddress("127.0.0.1", 9083)
      val remoteAddress = new InetSocketAddress("127.0.0.1", 9084)
      val description = ConnectionDescription(
        connection.ref,
        ConnectionId(remoteAddress, localAddress, Incoming),
        Some(localAddress),
        Seq.empty
      )
      val handler = system.actorOf(
        PeerConnectionHandlerRef.props(
          settings.scorexSettings,
          controller.ref,
          ScorexContext(Seq.empty, None, None),
          description
        )
      )

      connection.expectMsgType[Tcp.Register]
      connection.expectMsg(Tcp.ResumeReading)
      connection.expectMsgType[Tcp.Write]
      val header = ByteString(
        Array[Byte](0, 1, 'a'.toByte, 4, 0, 100, 0, 0, 1, 127, 0xa0.toByte, 0x3f)
      )
      val incompleteHandshake =
        header ++ ByteString(
          Array.fill[Byte](HandshakeSerializer.maxHandshakeSize - header.length)(0)
        )

      connection.send(handler, Tcp.Received(incompleteHandshake))
      val validHandshake =
        HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis()))
      connection.send(handler, Tcp.Received(ByteString(validHandshake)))

      connection.expectMsg(Tcp.Abort)
      controller.expectNoMessage(200.millis)
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }
}
