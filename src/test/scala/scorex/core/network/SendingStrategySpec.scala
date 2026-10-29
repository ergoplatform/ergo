package scorex.core.network

import akka.actor.ActorRef
import akka.testkit.TestProbe
import org.ergoplatform.network.Version
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.defaultPeerSpec
import scorex.testkit.utils.AkkaFixture

import java.net.InetSocketAddress
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class SendingStrategySpec extends ErgoCorePropertyTest {

  private def withFixture(testCode: AkkaFixture => Unit): Unit = {
    val fixture = new AkkaFixture
    try testCode(fixture)
    finally Await.result(fixture.system.terminate(), Duration.Inf)
  }

  private def peer(address: InetSocketAddress,
                   handler: ActorRef,
                   version: Version,
                   lastHandshake: Long): ConnectedPeer = {
    val info = PeerInfo(
      defaultPeerSpec.copy(protocolVersion = version, declaredAddress = Some(address)),
      lastHandshake
    )
    ConnectedPeer(
      ConnectionId(address, new InetSocketAddress("127.0.0.1", 9050), Incoming),
      handler,
      Some(info)
    )
  }

  property("a targeted older peer is not selected after version filtering") {
    withFixture { fixture =>
      implicit val system = fixture.system
      val older = peer(new InetSocketAddress("192.0.2.1", 9051), TestProbe().ref,
        Version(5, 0, 0), 1L)
      val eligible = peer(new InetSocketAddress("192.0.2.2", 9052), TestProbe().ref,
        Version(99, 0, 0), 2L)

      val connections = Seq(older, eligible)
      NetworkController.selectEligibleConnections(SendToPeer(older), Version(99, 0, 0),
        connections) shouldBe empty
      NetworkController.selectEligibleConnections(SendToPeer(eligible), Version(99, 0, 0),
        connections) shouldBe Seq(eligible)
    }
  }

  property("a targeted list keeps only eligible current connections in requested order") {
    withFixture { fixture =>
      implicit val system = fixture.system
      val first = peer(new InetSocketAddress("192.0.2.3", 9053), TestProbe().ref,
        Version(99, 0, 0), 3L)
      val second = peer(new InetSocketAddress("192.0.2.4", 9054), TestProbe().ref,
        Version(99, 0, 0), 4L)
      val older = peer(new InetSocketAddress("192.0.2.5", 9055), TestProbe().ref,
        Version(5, 0, 0), 5L)
      val connections = Seq(first, second, older)
      val chosen = Seq(second.copy(peerInfo = None), older, first.copy(peerInfo = None),
        second.copy(peerInfo = None))

      val selected = NetworkController.selectEligibleConnections(SendToPeers(chosen),
        Version(99, 0, 0), connections)
      selected shouldBe Seq(second, first, second)
      selected.map(_.peerInfo) shouldBe Seq(second.peerInfo, first.peerInfo, second.peerInfo)
    }
  }

  property("a replaced handler at the same address is not an eligible target") {
    withFixture { fixture =>
      implicit val system = fixture.system
      val current = peer(new InetSocketAddress("192.0.2.6", 9056), TestProbe().ref,
        Version(99, 0, 0), 6L)
      val stale = current.copy(handlerRef = TestProbe().ref)
      stale shouldBe current // ConnectedPeer equality checks only the remote address.

      NetworkController.selectEligibleConnections(SendToPeer(stale), Version(99, 0, 0),
        Seq(current)) shouldBe empty
      NetworkController.selectEligibleConnections(SendToPeers(Seq(stale)), Version(99, 0, 0),
        Seq(current)) shouldBe empty
    }
  }

  property("broadcast and random selection keep their existing eligible-set behavior") {
    withFixture { fixture =>
      implicit val system = fixture.system
      val current = peer(new InetSocketAddress("192.0.2.7", 9057), TestProbe().ref,
        Version(99, 0, 0), 7L)

      val older = peer(new InetSocketAddress("192.0.2.8", 9058), TestProbe().ref,
        Version(5, 0, 0), 8L)
      val connections = Seq(older, current)

      NetworkController.selectEligibleConnections(Broadcast, Version(99, 0, 0),
        connections) shouldBe Seq(current)
      NetworkController.selectEligibleConnections(SendToRandom, Version(99, 0, 0),
        connections) shouldBe Seq(current)
      NetworkController.selectEligibleConnections(SendToRandom, Version(99, 0, 0),
        Nil) shouldBe empty
    }
  }
}
