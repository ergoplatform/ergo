package scorex.core.network

import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import org.ergoplatform.network.message.{GetPeersSpec, Message, MessageSerializer, MessageSpec}
import org.ergoplatform.network.{Handshake, HandshakeSerializer}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.{defaultPeerSpec, settings}
import scorex.core.app.ScorexContext
import scorex.testkit.utils.AkkaFixture

import java.net.InetSocketAddress
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

/**
  * Inbound forwarding with a stalled downstream, actor level, no network.
  *
  * A TestProbe stands in for the TCP connection and a second TestProbe stands in for the
  * NetworkController; the second never replies. After the handshake, one Tcp.Received carrying
  * 64 well-formed GetPeers frames is delivered. The spec counts how many decoded messages reach the
  * controller and which read-control commands reach the connection.
  *
  * Uses only API present on both trees compared, so the same bytes compile on each.
  * Claim: with a per-connection inbound bound, exactly 16 messages are forwarded and reading is
  * paused (SuspendReading on an incoming connection; no ResumeReading on an outgoing pull-mode one).
  * Against it: any other count (more: no bound; fewer: decoding stopped early), or reading resumed
  * while messages are outstanding.
  * Caps: 64 frames (about 0.6 KB), at most 5 s per collection.
  */
class InboundForwardBoundSpec extends ErgoCorePropertyTest {

  private val Frames = 64
  private val Bound = 16
  private val specs: Seq[MessageSpec[_]] = Seq(GetPeersSpec)

  private def run(direction: ConnectionDirection, localPort: Int): Unit = {
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
        ConnectionId(remoteAddress, localAddress, direction),
        Some(localAddress),
        Seq.empty
      )
      val handler = TestActorRef(new PeerConnectionHandler(
        settings.scorexSettings,
        controller.ref,
        ScorexContext(specs, None, None),
        description
      ))

      connection.expectMsgType[Tcp.Register]
      connection.expectMsg(Tcp.ResumeReading)
      connection.expectMsgType[Tcp.Write]

      val handshake = HandshakeSerializer.toBytes(Handshake(defaultPeerSpec, System.currentTimeMillis()))
      connection.send(handler, Tcp.Received(ByteString(handshake)))
      controller.expectMsgType[NetworkController.ReceivableMessages.Handshaked]
      connection.expectMsg(Tcp.ResumeReading)

      val serializer = new MessageSerializer(specs, settings.scorexSettings.network.magicBytes)
      val frame = serializer.serialize(Message(GetPeersSpec, Right(()), None))
      val burst = (1 to Frames).map(_ => frame).reduce(_ ++ _)
      connection.send(handler, Tcp.Received(burst))

      val atController = controller.receiveWhile(max = 5.seconds, idle = 500.millis, messages = Frames + 1) {
        case m => m
      }
      val forwarded = atController.count(_.isInstanceOf[Message[_]])
      val atConnection = connection.receiveWhile(max = 5.seconds, idle = 500.millis, messages = 8) {
        case m => m
      }
      val suspend = atConnection.count(_ == Tcp.SuspendReading)
      val resume = atConnection.count(_ == Tcp.ResumeReading)
      // scalastyle:off println
      println(s"E1RESULT direction=$direction frames=$Frames frameBytes=${frame.length} " +
        s"forwarded=$forwarded suspendReading=$suspend resumeReading=$resume " +
        s"connectionMsgs=${atConnection.mkString("[", ",", "]")}")
      // scalastyle:on println

      forwarded shouldBe Bound
      resume shouldBe 0
      if (direction.isIncoming) suspend shouldBe 1 else suspend shouldBe 0
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  property("incoming: a stalled controller sees exactly 16 messages and reading is suspended") {
    run(Incoming, localPort = 9611)
  }

  property("outgoing: a stalled controller sees exactly 16 messages and reading is not resumed") {
    run(Outgoing, localPort = 9621)
  }
}
