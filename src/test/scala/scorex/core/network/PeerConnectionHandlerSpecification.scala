package scorex.core.network

import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import org.ergoplatform.network.message.{
  GetPeersSpec,
  Message,
  MessageSerializer,
  MessageSpec,
  UtxoSnapshotChunkSpec
}
import org.ergoplatform.network.{Handshake, HandshakeSerializer}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.{defaultPeerSpec, settings}
import scorex.core.app.ScorexContext
import scorex.testkit.utils.AkkaFixture

import java.net.InetSocketAddress
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

class PeerConnectionHandlerSpecification extends ErgoCorePropertyTest {
  private final class ConnectedHandler(val connection: TestProbe,
                                       val watcher: TestProbe,
                                       val handler: TestActorRef[PeerConnectionHandler])

  private def withHandler(
    messageSpecs: Seq[MessageSpec[_]],
    localPort: Int
  )(test: ConnectedHandler => Unit): Unit = {
    val fixture = new AkkaFixture
    try {
      implicit val system = fixture.system
      implicit val ec = system.dispatcher
      val connection = TestProbe("connection")
      val controller = TestProbe("controller")
      val localAddress = new InetSocketAddress("127.0.0.1", localPort)
      val remoteAddress = new InetSocketAddress("127.0.0.1", localPort + 1)
      val description = ConnectionDescription(
        connection.ref,
        ConnectionId(remoteAddress, localAddress, Incoming),
        Some(localAddress),
        Seq.empty
      )
      val handler = TestActorRef(new PeerConnectionHandler(
        settings.scorexSettings,
        controller.ref,
        ScorexContext(messageSpecs, None, None),
        description
      ))

      connection.expectMsgType[Tcp.Register]
      connection.expectMsg(Tcp.ResumeReading)
      connection.expectMsgType[Tcp.Write]

      test(new ConnectedHandler(connection, controller, handler))
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  private def withConnectedHandler(
    messageSpecs: Seq[MessageSpec[_]],
    localPort: Int
  )(test: ConnectedHandler => Unit): Unit =
    withHandler(messageSpecs, localPort) { fixture =>
      val handshake = HandshakeSerializer.toBytes(
        Handshake(defaultPeerSpec, System.currentTimeMillis())
      )
      fixture.connection.send(fixture.handler, Tcp.Received(ByteString(handshake)))
      fixture.watcher.expectMsgType[NetworkController.ReceivableMessages.Handshaked]
      fixture.connection.expectMsg(Tcp.ResumeReading)
      fixture.watcher.watch(fixture.handler)

      test(fixture)
    }

  property("abort before a fifth maximum snapshot frame is retained") {
    withConnectedHandler(Seq(UtxoSnapshotChunkSpec), localPort = 9083) { fixture =>
      val chunkMessage = Message(
        UtxoSnapshotChunkSpec,
        Right(Array.fill[Byte](3999996)(1)),
        None
      )
      fixture.handler ! chunkMessage
      val failedWrite = fixture.connection.expectMsgType[Tcp.Write]
      failedWrite.data.length shouldEqual 4000013
      fixture.connection.send(fixture.handler, Tcp.CommandFailed(failedWrite))
      fixture.connection.expectMsg(Tcp.ResumeWriting)

      (2 to 4).foreach { id =>
        val write = Tcp.Write(
          failedWrite.data,
          PeerConnectionHandler.ReceivableMessages.Ack(id)
        )
        fixture.connection.send(fixture.handler, Tcp.CommandFailed(write))
        fixture.connection.expectMsg(Tcp.ResumeWriting)
      }
      fixture.connection.expectNoMessage(200.millis)

      val overLimitWrite = Tcp.Write(
        failedWrite.data,
        PeerConnectionHandler.ReceivableMessages.Ack(5)
      )
      fixture.connection.send(
        fixture.handler,
        Tcp.CommandFailed(overLimitWrite)
      )
      fixture.connection.expectMsg(Tcp.ResumeWriting)
      fixture.connection.expectMsg(1.second, Tcp.Abort)
      fixture.watcher.expectTerminated(fixture.handler)
    }
  }

  property("abort before more than 64 outbound messages are buffered") {
    withConnectedHandler(Seq(GetPeersSpec), localPort = 9093) { fixture =>
      val getPeersMessage = Message(GetPeersSpec, Right(()), None)
      fixture.handler ! getPeersMessage
      val failedWrite = fixture.connection.expectMsgType[Tcp.Write]
      fixture.connection.send(fixture.handler, Tcp.CommandFailed(failedWrite))
      fixture.connection.expectMsg(Tcp.ResumeWriting)

      (1 until PeerConnectionHandler.MaxBufferedOutboundMessages)
        .foreach(_ => fixture.handler ! getPeersMessage)
      fixture.connection.expectNoMessage(200.millis)

      fixture.handler ! getPeersMessage
      fixture.connection.expectMsg(1.second, Tcp.Abort)
      fixture.watcher.expectTerminated(fixture.handler)
    }
  }

  property("account retried and acknowledged writes exactly") {
    withConnectedHandler(
      Seq(UtxoSnapshotChunkSpec),
      localPort = 9103
    ) { fixture =>
      val chunkMessage = Message(
        UtxoSnapshotChunkSpec,
        Right(Array.fill[Byte](3999996)(1)),
        None
      )
      fixture.handler ! chunkMessage
      val failedWrite = fixture.connection.expectMsgType[Tcp.Write]
      failedWrite.data.length shouldEqual 4000013
      failedWrite.ack shouldEqual PeerConnectionHandler.ReceivableMessages.Ack(1)

      fixture.connection.send(fixture.handler, Tcp.CommandFailed(failedWrite))
      fixture.connection.expectMsg(Tcp.ResumeWriting)

      (2 to 4).foreach(_ => fixture.handler ! chunkMessage)
      fixture.connection.expectNoMessage(200.millis)

      fixture.connection.send(fixture.handler, Tcp.WritingResumed)
      val retriedWrite = fixture.connection.expectMsgType[Tcp.Write]
      retriedWrite.data shouldEqual failedWrite.data
      retriedWrite.ack shouldEqual PeerConnectionHandler.ReceivableMessages.Ack(1)
      fixture.connection.send(fixture.handler, Tcp.CommandFailed(retriedWrite))
      fixture.connection.expectMsg(Tcp.ResumeWriting)
      fixture.connection.expectNoMessage(200.millis)

      fixture.connection.send(fixture.handler, Tcp.WritingResumed)
      val finalRetry = fixture.connection.expectMsgType[Tcp.Write]
      finalRetry.data shouldEqual failedWrite.data
      finalRetry.ack shouldEqual PeerConnectionHandler.ReceivableMessages.Ack(1)
      fixture.connection.send(
        fixture.handler,
        PeerConnectionHandler.ReceivableMessages.Ack(1)
      )

      val nextWrite = fixture.connection.expectMsgType[Tcp.Write]
      nextWrite.ack shouldEqual PeerConnectionHandler.ReceivableMessages.Ack(2)
    }
  }

  property("deliver a whole message received in the same TCP read as the handshake") {
    val specs: Seq[MessageSpec[_]] = Seq(GetPeersSpec)
    withHandler(specs, localPort = 9113) { fixture =>
      val serializer = new MessageSerializer(specs, settings.scorexSettings.network.magicBytes)
      val handshakeBytes = ByteString(HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis())))
      val messageBytes = serializer.serialize(Message(GetPeersSpec, Right(()), None))
      fixture.connection.send(fixture.handler, Tcp.Received(handshakeBytes ++ messageBytes))

      fixture.watcher.expectMsgType[NetworkController.ReceivableMessages.Handshaked]
      val delivered = fixture.watcher.expectMsgType[Message[_]]
      delivered.spec shouldEqual GetPeersSpec
      delivered.source shouldBe defined
      fixture.connection.expectMsg(Tcp.ResumeReading)
    }
  }

  property("reassemble a message split across the handshake TCP read and the next TCP read") {
    val specs: Seq[MessageSpec[_]] = Seq(UtxoSnapshotChunkSpec)
    val serializer = new MessageSerializer(specs, settings.scorexSettings.network.magicBytes)
    val handshakeBytes = ByteString(HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis())))
    val body = Array[Byte](1, 2, 3, 4, 5, 6, 7, 8)
    val messageBytes = serializer.serialize(Message(UtxoSnapshotChunkSpec, Right(body), None))

    (1 until messageBytes.length).foreach { cut =>
      withHandler(specs, localPort = 9120 + cut * 2) { fixture =>
        val (head, tail) = messageBytes.splitAt(cut)
        fixture.connection.send(fixture.handler, Tcp.Received(handshakeBytes ++ head))
        fixture.watcher.expectMsgType[NetworkController.ReceivableMessages.Handshaked]
        fixture.connection.expectMsg(Tcp.ResumeReading)

        fixture.connection.send(fixture.handler, Tcp.Received(tail))
        val delivered = fixture.watcher.expectMsgType[Message[_]]
        delivered.spec shouldEqual UtxoSnapshotChunkSpec
        delivered.source shouldBe defined
        fixture.connection.expectMsg(Tcp.ResumeReading)
        fixture.watcher.expectNoMessage(25.millis)
      }
    }
  }

  property("accept a valid handshake split across TCP chunks") {
    withHandler(Seq.empty, localPort = 9053) { fixture =>
      val bytes =
        HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis()))
      val splitAt = bytes.length / 2

      fixture.connection.send(fixture.handler, Tcp.Received(ByteString(bytes.take(splitAt))))
      fixture.connection.expectMsg(Tcp.ResumeReading)
      fixture.connection.send(fixture.handler, Tcp.Received(ByteString(bytes.drop(splitAt))))

      fixture.watcher.expectMsgType[NetworkController.ReceivableMessages.Handshaked](2.seconds)
      fixture.connection.expectMsg(Tcp.ResumeReading)
    }
  }

  property("preserve a framed message coalesced with a handshake carrying peer features") {
    val specs: Seq[MessageSpec[_]] = Seq(GetPeersSpec)
    withHandler(specs, localPort = 9063) { fixture =>
      val baseHandshake =
        HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis()))
      val handshake = ByteString(baseHandshake.dropRight(1)) ++
        ByteString(Array[Byte](1, 127, 3, 1, 2, 3))
      val framedMessage =
        new MessageSerializer(specs, settings.scorexSettings.network.magicBytes)
          .serialize(Message(GetPeersSpec, Right(()), None))

      fixture.connection.send(fixture.handler, Tcp.Received(handshake ++ framedMessage))

      fixture.watcher.expectMsgType[NetworkController.ReceivableMessages.Handshaked](2.seconds)
      val received = fixture.watcher.expectMsgType[Message[_]](2.seconds)
      received.spec shouldBe GetPeersSpec
      fixture.watcher.expectNoMessage(200.millis)
    }
  }

  property("apply the handshake size limit only to the handshake prefix") {
    val specs: Seq[MessageSpec[_]] = Seq(UtxoSnapshotChunkSpec)
    withHandler(specs, localPort = 9073) { fixture =>
      val handshake =
        HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis()))
      val framedMessage = new MessageSerializer(specs, settings.scorexSettings.network.magicBytes)
        .serialize(Message(UtxoSnapshotChunkSpec, Right(Array.fill[Byte](9000)(1)), None))

      (handshake.length + framedMessage.length) should be > HandshakeSerializer.maxHandshakeSize
      fixture.connection.send(fixture.handler, Tcp.Received(ByteString(handshake) ++ framedMessage))

      fixture.watcher.expectMsgType[NetworkController.ReceivableMessages.Handshaked](2.seconds)
      val received = fixture.watcher.expectMsgType[Message[_]](2.seconds)
      received.spec shouldBe UtxoSnapshotChunkSpec
    }
  }

  property("close when an incomplete handshake reaches the size limit") {
    withHandler(Seq.empty, localPort = 9183) { fixture =>
      val header = ByteString(
        Array[Byte](0, 1, 'a'.toByte, 4, 0, 100, 0, 0, 1, 127, 0xa0.toByte, 0x3f)
      )
      val incompleteHandshake =
        header ++ ByteString(
          Array.fill[Byte](HandshakeSerializer.maxHandshakeSize - header.length)(0)
        )

      fixture.connection.send(fixture.handler, Tcp.Received(incompleteHandshake))
      val validHandshake =
        HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis()))
      fixture.connection.send(fixture.handler, Tcp.Received(ByteString(validHandshake)))

      fixture.connection.expectMsg(Tcp.Abort)
      fixture.watcher.expectNoMessage(200.millis)
    }
  }
}
