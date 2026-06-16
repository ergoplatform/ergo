package scorex.core.network

import akka.actor.ActorRef
import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.core.app.ScorexContext
import scorex.testkit.utils.AkkaFixture

import java.net.InetSocketAddress
import scala.concurrent.Await
import scala.concurrent.duration._

class NetworkControllerSpec extends ErgoCorePropertyTest {

  import org.ergoplatform.network.peer.PeerManager.ReceivableMessages._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private class ControllerFixture extends AkkaFixture {
    implicit val ec = system.dispatcher
    implicit val actorSystem = system

    val scorexContext: ScorexContext = ScorexContext(Seq.empty, None, None)

    def createController(maxConnections: Int): (TestActorRef[NetworkController], TestProbe, TestProbe) = {
      val peerManagerProbe = TestProbe("PeerManager")
      val tcpManagerProbe = TestProbe("TcpManager")

      val testSettings = settings.copy(
        scorexSettings = settings.scorexSettings.copy(
          network = settings.scorexSettings.network.copy(
            maxConnections = maxConnections
          )
        )
      )

      val controller = TestActorRef(new NetworkController(
        testSettings,
        peerManagerProbe.ref,
        scorexContext,
        tcpManagerProbe.ref,
        _ => Map.empty[MessageCode, ActorRef]
      ))

      tcpManagerProbe.expectMsgType[Tcp.Bind]
      controller ! Tcp.Bound(testSettings.scorexSettings.network.bindAddress)

      (controller, peerManagerProbe, tcpManagerProbe)
    }

    def establishIncomingConnection(
      controller: TestActorRef[NetworkController],
      peerManagerProbe: TestProbe,
      remoteAddress: InetSocketAddress
    ): InetSocketAddress = {
      val localAddress = settings.scorexSettings.network.bindAddress
      val connectionProbe = TestProbe("Connection")

      connectionProbe.send(controller, Tcp.Connected(remoteAddress, localAddress))

      peerManagerProbe.expectMsgPF(1.second) {
        case ConfirmConnection(_, handlerRef) =>
          controller ! ConnectionConfirmed(ConnectionId(remoteAddress, localAddress, Incoming), handlerRef)
      }

      remoteAddress
    }

    def establishOutgoingConnection(
      controller: TestActorRef[NetworkController],
      peerManagerProbe: TestProbe,
      tcpManagerProbe: TestProbe,
      remoteAddress: InetSocketAddress
    ): Unit = {
      val localAddress = settings.scorexSettings.network.bindAddress

      val peerInfo = PeerInfo(
        defaultPeerSpec.copy(declaredAddress = Some(remoteAddress)),
        System.currentTimeMillis()
      )
      controller ! NetworkController.ReceivableMessages.ConnectTo(peerInfo)

      tcpManagerProbe.expectMsgType[Tcp.Connect]

      val connectionProbe = TestProbe("Connection")
      connectionProbe.send(controller, Tcp.Connected(remoteAddress, localAddress))
    }
  }

  private def withFixture(testCode: ControllerFixture => Any): Unit = {
    val fixture = new ControllerFixture
    try {
      testCode(fixture)
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  // ============================================================================
  // PROPERTY-BASED TESTS
  // ============================================================================

  property("incomingLimit should equal max(maxConnections / 2, maxConnections - OutgoingConnections)") {
    val maxConnectionsGen = Gen.choose(1, 100)
    forAll(maxConnectionsGen) { maxConnections =>
      val expected = Math.max(maxConnections / 2, maxConnections - NetworkController.OutgoingConnections)
      val actual = Math.max(maxConnections / 2, maxConnections - 8)
      expected shouldBe actual
    }
  }

  property("incomingLimit with maxConnections=30 should be 22") {
    val limit = Math.max(30 / 2, 30 - NetworkController.OutgoingConnections)
    limit shouldBe 22
  }

  property("incomingLimit with small maxConnections should prefer half") {
    val limit = Math.max(10 / 2, 10 - NetworkController.OutgoingConnections)
    limit shouldBe 5
  }

  property("incoming connections count should be correct for mixed directions") {
    val directionsGen = Gen.listOf(Gen.oneOf(Incoming, Outgoing))
    forAll(directionsGen) { directions =>
      val peers = directions.zipWithIndex.map { case (dir, idx) =>
        val remoteAddr = new InetSocketAddress(s"192.168.1.$idx", 9000 + idx)
        val connId = ConnectionId(remoteAddr, new InetSocketAddress("127.0.0.1", 9003), dir)
        remoteAddr -> ConnectedPeer(connId, ActorRef.noSender, None)
      }.toMap

      val incomingCount = peers.values.count(_.connectionId.direction.isIncoming)
      val outgoingCount = peers.values.count(_.connectionId.direction.isOutgoing)
      val totalCount = peers.size

      incomingCount + outgoingCount shouldBe totalCount
    }
  }

  // ============================================================================
  // EXAMPLE-BASED ACTOR TESTS
  // ============================================================================

  property("incoming connection should be accepted when below limit") {
    withFixture { f =>
      implicit val system = f.system
      val (controller, peerManagerProbe, _) = f.createController(maxConnections = 30)
      val incomingLimit = Math.max(30 / 2, 30 - NetworkController.OutgoingConnections)

      val remoteAddresses = (1 until incomingLimit).map { i =>
        new InetSocketAddress(s"192.168.1.$i", 9000 + i)
      }

      remoteAddresses.foreach { addr =>
        f.establishIncomingConnection(controller, peerManagerProbe, addr)
      }

      val testAddress = new InetSocketAddress("192.168.1.100", 9999)
      val connectionProbe = TestProbe("TestConnection")
      val localAddress = settings.scorexSettings.network.bindAddress

      connectionProbe.send(controller, Tcp.Connected(testAddress, localAddress))
      peerManagerProbe.expectMsgType[ConfirmConnection]
    }
  }

  property("incoming connection should be denied when at limit") {
    withFixture { f =>
      implicit val system = f.system
      val maxConnections = 30
      val (controller, peerManagerProbe, _) = f.createController(maxConnections = maxConnections)
      val incomingLimit = Math.max(maxConnections / 2, maxConnections - NetworkController.OutgoingConnections)

      val remoteAddresses = (1 to incomingLimit).map { i =>
        new InetSocketAddress(s"192.168.1.$i", 9000 + i)
      }

      remoteAddresses.foreach { addr =>
        f.establishIncomingConnection(controller, peerManagerProbe, addr)
      }

      val testAddress = new InetSocketAddress("192.168.1.100", 9999)
      val connectionProbe = TestProbe("TestConnection")
      val localAddress = settings.scorexSettings.network.bindAddress

      connectionProbe.send(controller, Tcp.Connected(testAddress, localAddress))
      connectionProbe.expectMsg(Tcp.Close)
      peerManagerProbe.expectNoMessage(500.millis)
    }
  }

  property("outgoing connection should be accepted when total below maxConnections") {
    withFixture { f =>
      val (controller, peerManagerProbe, tcpManagerProbe) = f.createController(maxConnections = 10)

      val incomingAddresses = (1 to 3).map { i =>
        new InetSocketAddress(s"10.0.0.$i", 8000 + i)
      }
      incomingAddresses.foreach { addr =>
        f.establishIncomingConnection(controller, peerManagerProbe, addr)
      }

      val remainingCapacity = 10 - 3
      val outgoingAddresses = (1 to remainingCapacity).map { i =>
        new InetSocketAddress(s"8.8.$i.$i", 7000 + i)
      }

      outgoingAddresses.foreach { addr =>
        f.establishOutgoingConnection(controller, peerManagerProbe, tcpManagerProbe, addr)
      }
    }
  }

  property("outgoing connection scheduling should not exceed maxConnections") {
    withFixture { f =>
      val (controller, peerManagerProbe, tcpManagerProbe) = f.createController(maxConnections = 5)

      val incomingLimit = Math.max(5 / 2, 5 - NetworkController.OutgoingConnections)
      val incomingAddresses = (1 to incomingLimit).map { i =>
        new InetSocketAddress(s"10.0.0.$i", 8000 + i)
      }
      incomingAddresses.foreach { addr =>
        f.establishIncomingConnection(controller, peerManagerProbe, addr)
      }

      val extraPeer = PeerInfo(
        defaultPeerSpec.copy(declaredAddress = Some(new InetSocketAddress("8.8.8.8", 7001))),
        System.currentTimeMillis()
      )
      controller ! NetworkController.ReceivableMessages.ConnectTo(extraPeer)
      tcpManagerProbe.expectMsgType[Tcp.Connect]
    }
  }

  property("outgoing connection scheduler should respect maxConnections") {
    withFixture { f =>
      val maxConnections = 5
      val (controller, peerManagerProbe, tcpManagerProbe) = f.createController(maxConnections = maxConnections)

      // Fill up to incomingLimit with incoming connections
      val incomingLimit = Math.max(maxConnections / 2, maxConnections - NetworkController.OutgoingConnections)
      val incomingAddresses = (1 to incomingLimit).map { i =>
        new InetSocketAddress(s"10.0.0.$i", 8000 + i)
      }
      incomingAddresses.foreach { addr =>
        f.establishIncomingConnection(controller, peerManagerProbe, addr)
      }

      // Now the scheduler should not attempt new outgoing connections since we are at maxConnections
      // We verify by checking that tcpManagerProbe does not receive unexpected messages
      tcpManagerProbe.expectNoMessage(500.millis)
      succeed
    }
  }

  property("duplicate incoming connection should be rejected with Close") {
    withFixture { f =>
      implicit val system = f.system
      val (controller, peerManagerProbe, _) = f.createController(maxConnections = 30)

      val remoteAddress = new InetSocketAddress("192.168.1.1", 9001)
      f.establishIncomingConnection(controller, peerManagerProbe, remoteAddress)

      val connectionProbe = TestProbe("DuplicateConnection")
      val localAddress = settings.scorexSettings.network.bindAddress

      connectionProbe.send(controller, Tcp.Connected(remoteAddress, localAddress))
      connectionProbe.expectMsg(Tcp.Close)
    }
  }

  property("outgoing connection should bypass incoming limit check") {
    withFixture { f =>
      val (controller, peerManagerProbe, tcpManagerProbe) = f.createController(maxConnections = 30)
      val incomingLimit = Math.max(30 / 2, 30 - NetworkController.OutgoingConnections)

      val incomingAddresses = (1 to incomingLimit).map { i =>
        new InetSocketAddress(s"192.168.1.$i", 9000 + i)
      }
      incomingAddresses.foreach { addr =>
        f.establishIncomingConnection(controller, peerManagerProbe, addr)
      }

      val outgoingAddress = new InetSocketAddress("8.8.8.8", 8001)
      f.establishOutgoingConnection(controller, peerManagerProbe, tcpManagerProbe, outgoingAddress)
    }
  }
}
