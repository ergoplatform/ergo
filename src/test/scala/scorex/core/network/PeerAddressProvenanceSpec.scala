package scorex.core.network

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{
  DisconnectedPeer,
  HandshakedPeer
}
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.{
  AddVerifiedOutboundPeer,
  GetAllPeers,
  RemovePeer,
  SeenPeers
}
import org.ergoplatform.network.peer.{
  LocalAddressPeerFeature,
  PeerInfo,
  PeerManagerRef,
  SessionIdPeerFeature
}
import org.ergoplatform.network.{Handshake, HandshakeSerializer, PeerSpec}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.{defaultPeerSpec, settings}
import scorex.core.app.ScorexContext
import scorex.testkit.utils.AkkaFixture

import java.net.InetSocketAddress
import java.nio.file.Files
import scala.concurrent.Await
import scala.concurrent.duration._

class PeerAddressProvenanceSpec extends ErgoCorePropertyTest {

  private val advertisedAddress = new InetSocketAddress("198.51.100.20", 9030)
  private val alternateAddress = new InetSocketAddress("203.0.113.21", 9030)
  private val attackerTransport = new InetSocketAddress("192.0.2.19", 55000)

  private class Fixture(knownPeers: Seq[InetSocketAddress] = Seq(advertisedAddress),
                        allowLocal: Boolean = settings.scorexSettings.network.allowLocal)
    extends AkkaFixture {
    implicit val ec = system.dispatcher
    implicit val actorSystem: ActorSystem = system

    val context = ScorexContext(Seq.empty, None, None)
    val isolatedSettings = settings.copy(
      directory = Files.createTempDirectory("ergo-peer-address-provenance-").toString,
      scorexSettings = settings.scorexSettings.copy(
        network = settings.scorexSettings.network.copy(
          knownPeers = knownPeers,
          allowLocal = allowLocal
        )
      )
    )
    val peerManager: ActorRef = system.actorOf(PeerManagerRef.props(isolatedSettings, context))
    val managerMessages = TestProbe("PeerManagerMessages")
    val controllerManager = system.actorOf(Props(new Actor {
      override def receive: Receive = {
        case message =>
          managerMessages.ref ! message
          peerManager.forward(message)
      }
    }))
    val tcpManager = TestProbe("TcpManager")
    val events = TestProbe("PeerEvents")
    system.eventStream.subscribe(events.ref, classOf[HandshakedPeer])
    system.eventStream.subscribe(events.ref, classOf[DisconnectedPeer])

    val controller: TestActorRef[NetworkController] = TestActorRef(new NetworkController(
      isolatedSettings,
      controllerManager,
      context,
      tcpManager.ref,
      _ => Map.empty[MessageCode, ActorRef]
    ))
    tcpManager.expectMsgType[Tcp.Bind]

    def allPeers: Map[InetSocketAddress, PeerInfo] = {
      val query = TestProbe()
      query.send(peerManager, GetAllPeers)
      query.expectMsgType[Map[InetSocketAddress, PeerInfo]]
    }

    def awaitPeer(address: InetSocketAddress)(check: PeerInfo => Unit): Unit = {
      val query = TestProbe()
      query.awaitAssert {
        query.send(peerManager, GetAllPeers)
        val peers = query.expectMsgType[Map[InetSocketAddress, PeerInfo]]
        check(peers(address))
      }
    }

    def knownPeer: PeerInfo = allPeers(advertisedAddress)

    def seenPeers: Seq[PeerInfo] = {
      val query = TestProbe()
      query.send(peerManager, SeenPeers(10))
      query.expectMsgType[Seq[PeerInfo]]
    }

    def connectedPeers: Iterable[ConnectedPeer] = {
      val query = TestProbe()
      query.send(controller, NetworkController.ReceivableMessages.GetConnectedPeers)
      query.expectMsgType[Iterable[ConnectedPeer]]
    }

    private def handshake(connection: TestProbe, advertised: PeerSpec): (TestProbe, ActorRef) = {
      val handler = connection.expectMsgType[Tcp.Register].handler
      connection.expectMsg(Tcp.ResumeReading)
      connection.expectMsgType[Tcp.Write]
      val bytes = HandshakeSerializer.toBytes(Handshake(advertised, System.currentTimeMillis()))
      connection.send(handler, Tcp.Received(ByteString(bytes)))
      connection.expectMsg(Tcp.ResumeReading)
      (connection, handler)
    }

    def incoming(remote: InetSocketAddress, advertised: PeerSpec): (TestProbe, ActorRef) = {
      val connection = TestProbe()
      connection.send(
        controller,
        Tcp.Connected(remote, isolatedSettings.scorexSettings.network.bindAddress)
      )
      handshake(connection, advertised)
    }

    def outgoing(advertised: PeerSpec,
                 dialAddress: InetSocketAddress = advertisedAddress): (TestProbe, ActorRef) = {
      controller ! NetworkController.ReceivableMessages.ConnectTo(allPeers(dialAddress))
      tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe dialAddress
      val connection = TestProbe()
      val localEphemeral = new InetSocketAddress(
        isolatedSettings.scorexSettings.network.bindAddress.getAddress,
        55010
      )
      connection.send(controller, Tcp.Connected(dialAddress, localEphemeral))
      handshake(connection, advertised)
    }
  }

  private def withFixture(test: Fixture => Unit): Unit = {
    withFixture(Seq(advertisedAddress), settings.scorexSettings.network.allowLocal)(test)
  }

  private def withFixture(knownPeers: Seq[InetSocketAddress], allowLocal: Boolean)
                         (test: Fixture => Unit): Unit = {
    val fixture = new Fixture(knownPeers, allowLocal)
    try test(fixture)
    finally Await.result(fixture.system.terminate(), Duration.Inf)
  }

  property("an inbound claim cannot replace a verified peer or redirect a later dial") {
    withFixture { f =>
      val (verifiedConnection, verifiedHandler) = f.outgoing(
        defaultPeerSpec.copy(declaredAddress = Some(advertisedAddress))
      )
      f.events.expectMsgType[HandshakedPeer]
      f.awaitPeer(advertisedAddress)(_.lastHandshake should be > 0L)
      val verified = f.knownPeer
      verifiedConnection.watch(verifiedHandler)
      verifiedConnection.send(verifiedHandler, Tcp.PeerClosed)
      verifiedConnection.expectTerminated(verifiedHandler)
      f.events.expectMsgType[DisconnectedPeer]

      val claim = defaultPeerSpec.copy(
        declaredAddress = Some(advertisedAddress),
        features = Seq(LocalAddressPeerFeature(alternateAddress))
      )
      val (connection, handler) = f.incoming(attackerTransport, claim)
      val event = f.events.expectMsgType[HandshakedPeer]
      event.remote.peerInfo.get.peerSpec.address shouldBe Some(advertisedAddress)
      f.connectedPeers.find(_.connectionId.remoteAddress == attackerTransport)
        .flatMap(_.peerInfo).flatMap(_.peerSpec.localAddressOpt) shouldBe Some(alternateAddress)

      f.awaitPeer(advertisedAddress)(_ shouldBe verified)
      f.allPeers.keySet shouldBe Set(advertisedAddress)

      connection.watch(handler)
      connection.send(handler, Tcp.PeerClosed)
      connection.expectTerminated(handler)
      f.events.expectMsgType[DisconnectedPeer]

      f.controller ! NetworkController.ReceivableMessages.ConnectTo(f.knownPeer)
      f.tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe advertisedAddress
    }
  }

  property("same claimed address can remain live on distinct transport endpoints") {
    withFixture(Seq(advertisedAddress, alternateAddress), settings.scorexSettings.network.allowLocal) { f =>
      f.outgoing(defaultPeerSpec.copy(declaredAddress = Some(advertisedAddress)))
      f.events.expectMsgType[HandshakedPeer]
      f.awaitPeer(advertisedAddress)(_.connectionType shouldBe Some(Outgoing))
      val verified = f.knownPeer

      val (claimConnection, _) = f.incoming(
        attackerTransport,
        defaultPeerSpec.copy(declaredAddress = Some(advertisedAddress))
      )
      f.events.expectMsgType[HandshakedPeer]
      claimConnection.expectNoMessage(100.millis)

      // Both messages below originate in the controller. Reaching the later
      // outbound update is a FIFO barrier for any earlier destructive removal.
      f.outgoing(defaultPeerSpec.copy(declaredAddress = Some(alternateAddress)), alternateAddress)
      f.events.expectMsgType[HandshakedPeer]
      var reachedBarrier = false
      while (!reachedBarrier) {
        f.managerMessages.expectMsgPF(3.seconds) {
          case RemovePeer(address) if address == advertisedAddress =>
            fail("an inbound claimed duplicate removed the verified row")
          case AddVerifiedOutboundPeer(peerInfo, address)
            if address == alternateAddress &&
              peerInfo.peerSpec.declaredAddress.contains(alternateAddress) =>
            reachedBarrier = true
          case _ => ()
        }
      }

      f.awaitPeer(advertisedAddress)(_ shouldBe verified)
      f.connectedPeers.map(_.connectionId.remoteAddress).toSet should contain allOf (
        advertisedAddress,
        attackerTransport,
        alternateAddress
      )
    }
  }

  property("an outbound endpoint is stored under the dialed address, not its claim") {
    withFixture(Seq(advertisedAddress, attackerTransport), settings.scorexSettings.network.allowLocal) { f =>
      val original = f.knownPeer
      val claim = defaultPeerSpec.copy(
        declaredAddress = Some(advertisedAddress),
        features = Seq(LocalAddressPeerFeature(alternateAddress))
      )
      f.outgoing(claim, attackerTransport)
      f.events.expectMsgType[HandshakedPeer]

      f.awaitPeer(attackerTransport) { stored =>
        stored.peerSpec.declaredAddress shouldBe Some(attackerTransport)
        stored.peerSpec.localAddressOpt shouldBe None
        stored.connectionType shouldBe Some(Outgoing)
        stored.lastHandshake should be > 0L
      }
      f.allPeers(advertisedAddress) shouldBe original
      f.controller ! NetworkController.ReceivableMessages.ConnectTo(f.knownPeer)
      f.tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe advertisedAddress
    }
  }

  property("an outbound handshake refreshes its exact endpoint without a local shortcut") {
    withFixture { f =>
      val claimedSelf = new InetSocketAddress(
        "127.0.0.1",
        f.isolatedSettings.scorexSettings.network.bindAddress.getPort
      )
      val claim = defaultPeerSpec.copy(
        declaredAddress = Some(advertisedAddress),
        features = Seq(LocalAddressPeerFeature(claimedSelf))
      )
      val (connection, handler) = f.outgoing(claim)
      f.events.expectMsgType[HandshakedPeer]

      f.awaitPeer(advertisedAddress) { stored =>
        stored.lastHandshake should be > 0L
        stored.connectionType shouldBe Some(Outgoing)
        stored.peerSpec.declaredAddress shouldBe Some(advertisedAddress)
        stored.peerSpec.localAddressOpt shouldBe None
      }
      f.seenPeers.map(_.peerSpec.address) should contain(Some(advertisedAddress))

      connection.watch(handler)
      connection.send(handler, Tcp.PeerClosed)
      connection.expectTerminated(handler)
      f.events.expectMsgType[DisconnectedPeer]
      f.controller ! NetworkController.ReceivableMessages.ConnectTo(f.knownPeer)
      f.tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe advertisedAddress
    }
  }

  property("a rejected outbound handshake removes its dialed invalid endpoint") {
    withFixture(Seq(advertisedAddress, alternateAddress), settings.scorexSettings.network.allowLocal) { f =>
      val wrongMagic = f.isolatedSettings.scorexSettings.network.magicBytes.map(b => (b ^ 0xff).toByte)
      val bad = defaultPeerSpec.copy(
        declaredAddress = Some(advertisedAddress),
        features = Seq(SessionIdPeerFeature(wrongMagic, 1L))
      )
      val (connection, _) = f.outgoing(bad)
      connection.expectMsg(Tcp.Abort)

      f.outgoing(defaultPeerSpec.copy(declaredAddress = Some(alternateAddress)), alternateAddress)
      f.events.expectMsgType[HandshakedPeer]
      var reachedBarrier = false
      var removedDialedEndpoint = false
      while (!reachedBarrier) {
        f.managerMessages.expectMsgPF(3.seconds) {
          case RemovePeer(address) if address == advertisedAddress =>
            removedDialedEndpoint = true
          case AddVerifiedOutboundPeer(_, address) if address == alternateAddress =>
            reachedBarrier = true
          case _ => ()
        }
      }
      removedDialedEndpoint shouldBe true
      f.allPeers.keySet should not contain advertisedAddress
    }
  }

  property("a rejected inbound handshake never removes its claimed endpoint") {
    withFixture(Seq(advertisedAddress, alternateAddress), settings.scorexSettings.network.allowLocal) { f =>
      val wrongMagic = f.isolatedSettings.scorexSettings.network.magicBytes.map(b => (b ^ 0xff).toByte)
      val bad = defaultPeerSpec.copy(
        declaredAddress = Some(advertisedAddress),
        features = Seq(SessionIdPeerFeature(wrongMagic, 1L))
      )
      val (connection, _) = f.incoming(attackerTransport, bad)
      connection.expectMsg(Tcp.Abort)

      f.outgoing(defaultPeerSpec.copy(declaredAddress = Some(alternateAddress)), alternateAddress)
      f.events.expectMsgType[HandshakedPeer]
      var reachedBarrier = false
      while (!reachedBarrier) {
        f.managerMessages.expectMsgPF(3.seconds) {
          case RemovePeer(address) =>
            fail(s"an invalid inbound handshake removed $address")
          case AddVerifiedOutboundPeer(_, address) if address == alternateAddress =>
            reachedBarrier = true
          case _ => ()
        }
      }
      f.allPeers.keySet should contain(advertisedAddress)
    }
  }

  property("an inbound claim cannot suppress a dial to the claimed endpoint") {
    withFixture { f =>
      f.incoming(
        attackerTransport,
        defaultPeerSpec.copy(declaredAddress = Some(advertisedAddress))
      )
      f.events.expectMsgType[HandshakedPeer]
      f.controller ! NetworkController.ReceivableMessages.ConnectTo(f.knownPeer)
      f.tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe advertisedAddress
    }
  }

  property("an outbound false claim cannot suppress a dial to the claimed endpoint") {
    withFixture(Seq(advertisedAddress, attackerTransport), settings.scorexSettings.network.allowLocal) { f =>
      f.outgoing(
        defaultPeerSpec.copy(declaredAddress = Some(advertisedAddress)),
        attackerTransport
      )
      f.events.expectMsgType[HandshakedPeer]
      f.controller ! NetworkController.ReceivableMessages.ConnectTo(f.knownPeer)
      f.tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe advertisedAddress
    }
  }

  property("periodic selection excludes outgoing transports rather than their claims") {
    withFixture(Seq(advertisedAddress, attackerTransport), settings.scorexSettings.network.allowLocal) { f =>
      f.outgoing(
        defaultPeerSpec.copy(declaredAddress = Some(advertisedAddress)),
        attackerTransport
      )
      f.events.expectMsgType[HandshakedPeer]
      f.controller ! Tcp.Bound(f.isolatedSettings.scorexSettings.network.bindAddress)

      f.tcpManager.expectMsgType[Tcp.Connect](7.seconds).remoteAddress shouldBe advertisedAddress
    }
  }
}
