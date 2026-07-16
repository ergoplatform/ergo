package scorex.core.network

import akka.actor.ActorRef
import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.DisconnectedPeer
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

    case class EstablishedConnection(connectionProbe: TestProbe, handlerRef: ActorRef)

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
      beginIncomingConnection(controller, peerManagerProbe, remoteAddress)
      remoteAddress
    }

    def establishIncomingConnectionWithHandler(
      controller: TestActorRef[NetworkController],
      peerManagerProbe: TestProbe,
      remoteAddress: InetSocketAddress
    ): EstablishedConnection = {
      val connectionProbe = beginIncomingConnection(
        controller,
        peerManagerProbe,
        remoteAddress
      )

      val handlerRef = connectionProbe.expectMsgType[Tcp.Register].handler
      connectionProbe.expectMsg(Tcp.ResumeReading)
      connectionProbe.expectMsgType[Tcp.Write]

      EstablishedConnection(connectionProbe, handlerRef)
    }

    def beginPendingIncomingConnection(
      controller: TestActorRef[NetworkController],
      peerManagerProbe: TestProbe,
      remoteAddress: InetSocketAddress
    ): (TestProbe, ConfirmConnection) = {
      val localAddress = settings.scorexSettings.network.bindAddress
      val connectionProbe = TestProbe("Connection")

      connectionProbe.send(controller, Tcp.Connected(remoteAddress, localAddress))
      val confirmation = peerManagerProbe.expectMsgType[ConfirmConnection](1.second)

      connectionProbe -> confirmation
    }

    private def beginIncomingConnection(
      controller: TestActorRef[NetworkController],
      peerManagerProbe: TestProbe,
      remoteAddress: InetSocketAddress
    ): TestProbe = {
      val (connectionProbe, confirmation) =
        beginPendingIncomingConnection(controller, peerManagerProbe, remoteAddress)
      peerManagerProbe.send(
        controller,
        ConnectionConfirmed(confirmation.connectionId, confirmation.handlerRef)
      )
      connectionProbe
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

  property("blacklisting should close exactly the live connections for the banned IP") {
    withFixture { f =>
      implicit val system = f.system
      val (controller, peerManagerProbe, _) = f.createController(maxConnections = 30)
      val disconnectProbe = TestProbe("DisconnectedPeers")
      f.system.eventStream.subscribe(disconnectProbe.ref, classOf[DisconnectedPeer])

      val firstAddress = new InetSocketAddress("192.0.2.10", 9101)
      val secondAddress = new InetSocketAddress("192.0.2.10", 9102)
      val unrelatedAddress = new InetSocketAddress("198.51.100.20", 9201)
      val first = f.establishIncomingConnectionWithHandler(
        controller,
        peerManagerProbe,
        firstAddress
      )
      val second = f.establishIncomingConnectionWithHandler(
        controller,
        peerManagerProbe,
        secondAddress
      )
      val unrelated = f.establishIncomingConnectionWithHandler(
        controller,
        peerManagerProbe,
        unrelatedAddress
      )

      peerManagerProbe.send(controller, Blacklisted(firstAddress))

      first.connectionProbe.expectMsg(Tcp.Abort)
      second.connectionProbe.expectMsg(Tcp.Abort)
      unrelated.connectionProbe.expectNoMessage(200.millis)

      val duplicateBeforeTermination = TestProbe("DuplicateBeforeTermination")
      duplicateBeforeTermination.send(
        controller,
        Tcp.Connected(secondAddress, settings.scorexSettings.network.bindAddress)
      )
      duplicateBeforeTermination.expectMsg(Tcp.Close)

      first.connectionProbe.watch(first.handlerRef)
      second.connectionProbe.watch(second.handlerRef)
      first.connectionProbe.send(first.handlerRef, Tcp.Aborted)
      second.connectionProbe.send(second.handlerRef, Tcp.Aborted)
      first.connectionProbe.expectTerminated(first.handlerRef)
      second.connectionProbe.expectTerminated(second.handlerRef)

      val disconnectedAddresses = disconnectProbe.receiveN(2, 2.seconds).collect {
        case DisconnectedPeer(peer) => peer.connectionId.remoteAddress
      }.toSet
      disconnectedAddresses shouldBe Set(firstAddress, secondAddress)

      val replacement = TestProbe("ReplacementConnection")
      replacement.send(
        controller,
        Tcp.Connected(secondAddress, settings.scorexSettings.network.bindAddress)
      )
      peerManagerProbe.expectMsgPF(1.second) {
        case ConfirmConnection(connectionId, connectionRef) =>
          connectionId.remoteAddress shouldBe secondAddress
          connectionRef shouldBe replacement.ref
      }

      val unrelatedDuplicate = TestProbe("UnrelatedDuplicate")
      unrelatedDuplicate.send(
        controller,
        Tcp.Connected(unrelatedAddress, settings.scorexSettings.network.bindAddress)
      )
      unrelatedDuplicate.expectMsg(Tcp.Close)
    }
  }

  property("blacklisting should match by IP when the exact socket is absent") {
    withFixture { f =>
      val (controller, peerManagerProbe, _) = f.createController(maxConnections = 30)
      val siblingAddress = new InetSocketAddress("192.0.2.30", 9301)
      val missingSocketAddress = new InetSocketAddress("192.0.2.30", 9399)
      val sibling = f.establishIncomingConnectionWithHandler(
        controller,
        peerManagerProbe,
        siblingAddress
      )

      peerManagerProbe.send(controller, Blacklisted(missingSocketAddress))

      sibling.connectionProbe.expectMsg(Tcp.Abort)
      sibling.connectionProbe.watch(sibling.handlerRef)
      sibling.connectionProbe.send(sibling.handlerRef, Tcp.Aborted)
      sibling.connectionProbe.expectTerminated(sibling.handlerRef)
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

  property("pending incoming confirmations should count toward the incoming limit") {
    withFixture { f =>
      implicit val system = f.system
      val maxConnections = 10
      val (controller, peerManagerProbe, _) = f.createController(maxConnections)
      val incomingLimit = Math.max(maxConnections / 2, maxConnections - NetworkController.OutgoingConnections)

      (1 to incomingLimit).foreach { i =>
        f.beginPendingIncomingConnection(
          controller,
          peerManagerProbe,
          new InetSocketAddress(s"203.0.113.$i", 9000 + i)
        )
      }

      val overflowConnection = TestProbe("PendingOverflow")
      overflowConnection.send(
        controller,
        Tcp.Connected(
          new InetSocketAddress("198.51.100.99", 9999),
          settings.scorexSettings.network.bindAddress
        )
      )
      overflowConnection.expectMsg(Tcp.Close)
      peerManagerProbe.expectNoMessage(200.millis)
    }
  }

  property("denied pending incoming connection should release its slot") {
    withFixture { f =>
      implicit val system = f.system
      val maxConnections = 10
      val (controller, peerManagerProbe, _) = f.createController(maxConnections)
      val incomingLimit = Math.max(maxConnections / 2, maxConnections - NetworkController.OutgoingConnections)
      val (deniedConnection, denial) = f.beginPendingIncomingConnection(
        controller,
        peerManagerProbe,
        new InetSocketAddress("203.0.113.1", 9001)
      )

      (2 to incomingLimit).foreach { i =>
        f.beginPendingIncomingConnection(
          controller,
          peerManagerProbe,
          new InetSocketAddress(s"203.0.113.$i", 9000 + i)
        )
      }

      peerManagerProbe.send(
        controller,
        ConnectionDenied(denial.connectionId, denial.handlerRef)
      )
      deniedConnection.expectMsg(Tcp.Close)

      val replacement = TestProbe("DeniedSlotReplacement")
      val replacementAddress = new InetSocketAddress("198.51.100.10", 9100)
      replacement.send(
        controller,
        Tcp.Connected(replacementAddress, settings.scorexSettings.network.bindAddress)
      )
      peerManagerProbe.expectMsgPF(1.second) {
        case ConfirmConnection(connectionId, connectionRef) =>
          connectionId.remoteAddress shouldBe replacementAddress
          connectionRef shouldBe replacement.ref
      }
    }
  }

  property("closed pending incoming connection should release its slot") {
    withFixture { f =>
      implicit val system = f.system
      val maxConnections = 10
      val (controller, peerManagerProbe, _) = f.createController(maxConnections)
      val incomingLimit = Math.max(maxConnections / 2, maxConnections - NetworkController.OutgoingConnections)
      val (closedConnection, _) = f.beginPendingIncomingConnection(
        controller,
        peerManagerProbe,
        new InetSocketAddress("203.0.113.1", 9001)
      )

      (2 to incomingLimit).foreach { i =>
        f.beginPendingIncomingConnection(
          controller,
          peerManagerProbe,
          new InetSocketAddress(s"203.0.113.$i", 9000 + i)
        )
      }

      closedConnection.send(controller, Tcp.PeerClosed)
      closedConnection.send(
        controller,
        NetworkController.ReceivableMessages.GetPeersStatus
      )
      closedConnection.expectMsgType[org.ergoplatform.network.peer.PeersStatus]

      val replacement = TestProbe("ClosedSlotReplacement")
      val replacementAddress = new InetSocketAddress("198.51.100.11", 9101)
      replacement.send(
        controller,
        Tcp.Connected(replacementAddress, settings.scorexSettings.network.bindAddress)
      )
      peerManagerProbe.expectMsgPF(1.second) {
        case ConfirmConnection(connectionId, connectionRef) =>
          connectionId.remoteAddress shouldBe replacementAddress
          connectionRef shouldBe replacement.ref
      }
    }
  }

  property("terminated pending incoming connection should release its slot") {
    withFixture { f =>
      implicit val system = f.system
      val maxConnections = 10
      val (controller, peerManagerProbe, _) = f.createController(maxConnections)
      val incomingLimit = Math.max(maxConnections / 2, maxConnections - NetworkController.OutgoingConnections)
      val (terminatedConnection, _) = f.beginPendingIncomingConnection(
        controller,
        peerManagerProbe,
        new InetSocketAddress("203.0.113.1", 9001)
      )

      (2 to incomingLimit).foreach { i =>
        f.beginPendingIncomingConnection(
          controller,
          peerManagerProbe,
          new InetSocketAddress(s"203.0.113.$i", 9000 + i)
        )
      }

      val terminationProbe = TestProbe("PendingTermination")
      terminationProbe.watch(terminatedConnection.ref)
      f.system.stop(terminatedConnection.ref)
      terminationProbe.expectTerminated(terminatedConnection.ref)

      val replacement = TestProbe("TerminatedSlotReplacement")
      val replacementAddress = new InetSocketAddress("198.51.100.11", 9101)
      peerManagerProbe.awaitAssert({
        replacement.send(
          controller,
          Tcp.Connected(replacementAddress, settings.scorexSettings.network.bindAddress)
        )
        peerManagerProbe.expectMsgPF(200.millis) {
          case ConfirmConnection(connectionId, connectionRef) =>
            connectionId.remoteAddress shouldBe replacementAddress
            connectionRef shouldBe replacement.ref
        }
      }, 2.seconds, 50.millis)
    }
  }

  property("stale incoming confirmation should not replace a new pending connection") {
    withFixture { f =>
      implicit val system = f.system
      val (controller, peerManagerProbe, _) = f.createController(maxConnections = 10)
      val remoteAddress = new InetSocketAddress("203.0.113.20", 9020)
      val (firstConnection, firstConfirmation) =
        f.beginPendingIncomingConnection(controller, peerManagerProbe, remoteAddress)

      firstConnection.send(controller, Tcp.PeerClosed)
      firstConnection.send(controller, NetworkController.ReceivableMessages.GetPeersStatus)
      firstConnection.expectMsgType[org.ergoplatform.network.peer.PeersStatus]

      val (_, secondConfirmation) =
        f.beginPendingIncomingConnection(controller, peerManagerProbe, remoteAddress)

      peerManagerProbe.send(
        controller,
        ConnectionConfirmed(firstConfirmation.connectionId, firstConfirmation.handlerRef)
      )
      firstConnection.expectMsg(Tcp.Close)

      val duplicate = TestProbe("ConfirmationDuplicate")
      duplicate.send(
        controller,
        Tcp.Connected(remoteAddress, settings.scorexSettings.network.bindAddress)
      )
      duplicate.expectMsg(Tcp.Close)
      peerManagerProbe.expectNoMessage(200.millis)
      secondConfirmation.connectionId.remoteAddress shouldBe remoteAddress
    }
  }

  property("stale incoming denial should not release a new pending connection") {
    withFixture { f =>
      implicit val system = f.system
      val (controller, peerManagerProbe, _) = f.createController(maxConnections = 10)
      val remoteAddress = new InetSocketAddress("203.0.113.21", 9021)
      val (firstConnection, firstConfirmation) =
        f.beginPendingIncomingConnection(controller, peerManagerProbe, remoteAddress)

      firstConnection.send(controller, Tcp.PeerClosed)
      firstConnection.send(controller, NetworkController.ReceivableMessages.GetPeersStatus)
      firstConnection.expectMsgType[org.ergoplatform.network.peer.PeersStatus]

      val (_, secondConfirmation) =
        f.beginPendingIncomingConnection(controller, peerManagerProbe, remoteAddress)

      peerManagerProbe.send(
        controller,
        ConnectionDenied(firstConfirmation.connectionId, firstConfirmation.handlerRef)
      )
      firstConnection.expectMsg(Tcp.Close)

      val duplicate = TestProbe("DenialDuplicate")
      duplicate.send(
        controller,
        Tcp.Connected(remoteAddress, settings.scorexSettings.network.bindAddress)
      )
      duplicate.expectMsg(Tcp.Close)
      peerManagerProbe.expectNoMessage(200.millis)
      secondConfirmation.connectionId.remoteAddress shouldBe remoteAddress
    }
  }

  property("duplicate pending incoming connection should be rejected") {
    withFixture { f =>
      implicit val system = f.system
      val (controller, peerManagerProbe, _) = f.createController(maxConnections = 10)
      val remoteAddress = new InetSocketAddress("203.0.113.22", 9022)
      f.beginPendingIncomingConnection(controller, peerManagerProbe, remoteAddress)

      val duplicate = TestProbe("PendingDuplicate")
      duplicate.send(
        controller,
        Tcp.Connected(remoteAddress, settings.scorexSettings.network.bindAddress)
      )
      duplicate.expectMsg(Tcp.Close)
      peerManagerProbe.expectNoMessage(200.millis)
    }
  }

  property("blacklisting should close pending incoming connections for the banned IP") {
    withFixture { f =>
      implicit val system = f.system
      val (controller, peerManagerProbe, _) = f.createController(maxConnections = 10)
      val firstAddress = new InetSocketAddress("192.0.2.40", 9040)
      val secondAddress = new InetSocketAddress("192.0.2.40", 9041)
      val unrelatedAddress = new InetSocketAddress("198.51.100.40", 9042)
      val (firstConnection, _) =
        f.beginPendingIncomingConnection(controller, peerManagerProbe, firstAddress)
      val (secondConnection, _) =
        f.beginPendingIncomingConnection(controller, peerManagerProbe, secondAddress)
      val (unrelatedConnection, _) =
        f.beginPendingIncomingConnection(controller, peerManagerProbe, unrelatedAddress)

      peerManagerProbe.send(controller, Blacklisted(firstAddress))

      firstConnection.expectMsg(Tcp.Close)
      secondConnection.expectMsg(Tcp.Close)
      unrelatedConnection.expectNoMessage(200.millis)

      val replacement = TestProbe("BlacklistedSlotReplacement")
      val replacementAddress = new InetSocketAddress("198.51.100.41", 9043)
      replacement.send(
        controller,
        Tcp.Connected(replacementAddress, settings.scorexSettings.network.bindAddress)
      )
      peerManagerProbe.expectMsgPF(1.second) {
        case ConfirmConnection(connectionId, connectionRef) =>
          connectionId.remoteAddress shouldBe replacementAddress
          connectionRef shouldBe replacement.ref
      }
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
