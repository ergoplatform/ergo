package scorex.core.network

import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import org.ergoplatform.network.message.MessageConstants.MaxMessageSize
import org.ergoplatform.network.message.{GetPeersSpec, Message}
import org.ergoplatform.network.peer.PenaltyType
import org.ergoplatform.network.{Handshake, HandshakeSerializer}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.{defaultPeerSpec, settings}
import scorex.core.app.ScorexContext
import scorex.crypto.hash.Blake2b256
import scorex.testkit.utils.AkkaFixture

import java.net.InetSocketAddress
import java.nio.ByteOrder
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

class UnknownMessageCodeReceptionSpec extends ErgoCorePropertyTest {
  private val UnknownCode: Byte = 127.toByte

  private final class ConnectedHandler(val connection: TestProbe,
                                       val controller: TestProbe,
                                       val handler: TestActorRef[PeerConnectionHandler])

  private def withConnectedHandler(localPort: Int)(test: ConnectedHandler => Unit): Unit = {
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
        ScorexContext(Seq(GetPeersSpec), None, None),
        description
      ))

      connection.expectMsgType[Tcp.Register]
      connection.expectMsg(Tcp.ResumeReading)
      connection.expectMsgType[Tcp.Write]
      val handshake = HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis()))
      connection.send(handler, Tcp.Received(ByteString(handshake)))
      controller.expectMsgType[NetworkController.ReceivableMessages.Handshaked]
      connection.expectMsg(Tcp.ResumeReading)

      test(new ConnectedHandler(connection, controller, handler))
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  private def frame(code: Byte,
                    payload: Array[Byte] = Array.emptyByteArray,
                    magic: Array[Byte] = settings.scorexSettings.network.magicBytes,
                    declaredLength: Int = Int.MinValue,
                    checksum: Option[Array[Byte]] = None): ByteString = {
    val length = if (declaredLength == Int.MinValue) payload.length else declaredLength
    val builder = ByteString.createBuilder.putBytes(magic).putByte(code).putInt(length)(ByteOrder.BIG_ENDIAN)
    if (length > 0 && declaredLength == Int.MinValue) {
      builder.putBytes(checksum.getOrElse(Blake2b256.hash(payload).take(4))).putBytes(payload)
    }
    builder.result()
  }

  private def receive(fixture: ConnectedHandler, bytes: ByteString): Unit =
    fixture.connection.send(fixture.handler, Tcp.Received(bytes))

  private def expectGetPeers(fixture: ConnectedHandler): Unit =
    fixture.controller.expectMsgPF(1.second) {
      case message: Message[_] if message.spec == GetPeersSpec => message
    }

  private def expectNoPenalty(fixture: ConnectedHandler): Unit =
    fixture.controller.expectNoMessage(100.millis)

  private def expectPermanentPenalty(fixture: ConnectedHandler): Unit =
    fixture.controller.expectMsgPF(1.second) {
      case NetworkController.ReceivableMessages.PenalizePeer(_, PenaltyType.PermanentPenalty) =>
    }

  private def wrongMagic: Array[Byte] = {
    val magic = settings.scorexSettings.network.magicBytes.clone()
    magic(0) = (magic(0) ^ 1).toByte
    magic
  }

  property("complete zero-length unknown frames leave following known frames usable") {
    withConnectedHandler(localPort = 9171) { fixture =>
      val known = frame(GetPeersSpec.messageCode)

      receive(fixture, frame(UnknownCode) ++ known)
      expectGetPeers(fixture)
      expectNoPenalty(fixture)

      receive(fixture, known)
      expectGetPeers(fixture)
      expectNoPenalty(fixture)
    }
  }

  property("complete non-empty unknown frames leave following known frames usable") {
    withConnectedHandler(localPort = 9172) { fixture =>
      val known = frame(GetPeersSpec.messageCode)

      receive(fixture, known)
      expectGetPeers(fixture)
      expectNoPenalty(fixture)

      receive(fixture, frame(UnknownCode, Array[Byte](1, 2, 3)) ++ known)
      expectGetPeers(fixture)
      expectNoPenalty(fixture)
    }
  }

  property("incomplete unknown headers and payloads retain bytes until a complete following frame arrives") {
    withConnectedHandler(localPort = 9173) { fixture =>
      val unknown = frame(UnknownCode, Array.fill[Byte](8)(7))
      val known = frame(GetPeersSpec.messageCode)

      receive(fixture, unknown.take(5))
      expectNoPenalty(fixture)
      receive(fixture, unknown.slice(5, 15))
      expectNoPenalty(fixture)
      receive(fixture, unknown.drop(15) ++ known)

      expectGetPeers(fixture)
      expectNoPenalty(fixture)
      receive(fixture, known)
      expectGetPeers(fixture)
      expectNoPenalty(fixture)
    }
  }

  property("concatenated unknown and known frames preserve every known control message") {
    withConnectedHandler(localPort = 9175) { fixture =>
      val known = frame(GetPeersSpec.messageCode)
      val bytes = frame(UnknownCode) ++ known ++
        frame(UnknownCode, Array[Byte](4, 5, 6, 7)) ++ known ++ frame(UnknownCode)

      receive(fixture, bytes)
      expectGetPeers(fixture)
      expectGetPeers(fixture)
      expectNoPenalty(fixture)
      receive(fixture, known)
      expectGetPeers(fixture)
      expectNoPenalty(fixture)
    }
  }

  property("a complete unknown frame does not discard a partial following known header") {
    withConnectedHandler(localPort = 9176) { fixture =>
      val known = frame(GetPeersSpec.messageCode)

      receive(fixture, frame(UnknownCode, Array[Byte](9, 8, 7)) ++ known.take(5))
      expectNoPenalty(fixture)
      receive(fixture, known.drop(5))
      expectGetPeers(fixture)
      expectNoPenalty(fixture)
      receive(fixture, known)
      expectGetPeers(fixture)
      expectNoPenalty(fixture)
    }
  }

  property("known control frames still reach the controller") {
    withConnectedHandler(localPort = 9177) { fixture =>
      receive(fixture, frame(GetPeersSpec.messageCode))
      expectGetPeers(fixture)
      expectNoPenalty(fixture)
    }
  }

  property("malformed unknown envelopes retain permanent-penalty handling") {
    val malformed = Seq(
      frame(UnknownCode, magic = wrongMagic),
      frame(UnknownCode, Array[Byte](1), checksum = Some(Array[Byte](0, 0, 0, 0))),
      frame(UnknownCode, declaredLength = -2),
      frame(UnknownCode, declaredLength = MaxMessageSize + 1)
    )

    malformed.zipWithIndex.foreach { case (bytes, index) =>
      withConnectedHandler(localPort = 9180 + index * 2) { fixture =>
        receive(fixture, bytes)
        expectPermanentPenalty(fixture)
      }
    }
  }
}
