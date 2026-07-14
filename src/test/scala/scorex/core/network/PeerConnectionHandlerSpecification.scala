package scorex.core.network

import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import org.ergoplatform.network.message.{
  GetPeersSpec,
  Message,
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

  private def withConnectedHandler(
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

      val handshake = HandshakeSerializer.toBytes(
        Handshake(defaultPeerSpec, System.currentTimeMillis())
      )
      connection.send(handler, Tcp.Received(ByteString(handshake)))
      controller.expectMsgType[NetworkController.ReceivableMessages.Handshaked]
      connection.expectMsg(Tcp.ResumeReading)
      controller.watch(handler)

      test(new ConnectedHandler(connection, controller, handler))
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
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
    withConnectedHandler(Seq(GetPeersSpec), localPort = 9103) { fixture =>
      val maxSizedWrite = Tcp.Write(
        ByteString(new Array[Byte](
          PeerConnectionHandler.MaxBufferedOutboundBytes.toInt
        )),
        PeerConnectionHandler.ReceivableMessages.Ack(1)
      )
      fixture.connection.send(fixture.handler, Tcp.CommandFailed(maxSizedWrite))
      fixture.connection.expectMsg(Tcp.ResumeWriting)
      fixture.connection.expectNoMessage(200.millis)

      val replacementWrite = Tcp.Write(
        ByteString(new Array[Byte](9)),
        PeerConnectionHandler.ReceivableMessages.Ack(1)
      )
      fixture.connection.send(
        fixture.handler,
        Tcp.CommandFailed(replacementWrite)
      )
      fixture.connection.expectMsg(Tcp.ResumeWriting)
      fixture.connection.expectNoMessage(200.millis)

      fixture.connection.send(fixture.handler, Tcp.WritingResumed)
      val retriedWrite = fixture.connection.expectMsgType[Tcp.Write]
      retriedWrite.data.length shouldEqual 9
      retriedWrite.ack shouldEqual PeerConnectionHandler.ReceivableMessages.Ack(1)
      fixture.connection.send(
        fixture.handler,
        PeerConnectionHandler.ReceivableMessages.Ack(1)
      )

      fixture.handler ! Message(GetPeersSpec, Right(()), None)
      val nextWrite = fixture.connection.expectMsgType[Tcp.Write]
      fixture.connection.send(fixture.handler, Tcp.CommandFailed(nextWrite))
      fixture.connection.expectMsg(Tcp.ResumeWriting)
      fixture.connection.expectNoMessage(200.millis)
    }
  }
}
