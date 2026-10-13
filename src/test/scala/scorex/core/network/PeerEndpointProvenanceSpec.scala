package scorex.core.network

import akka.actor.{ActorRef, ActorSystem}
import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import org.ergoplatform.db.DBSpec
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{DisconnectedPeer, HandshakedPeer}
import org.ergoplatform.network.message.{GetPeersSpec, Message, PeersSpec}
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.peer.{
  LocalAddressPeerFeature,
  PeerInfo,
  PeerInfoSerializer,
  PeerManagerRef
}
import org.ergoplatform.network.{Handshake, HandshakeSerializer, PeerSpec}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.{defaultPeerSpec, settings}
import scorex.core.app.ScorexContext
import scorex.db.LDBFactory
import scorex.testkit.utils.AkkaFixture

import java.io.{ByteArrayOutputStream, File, ObjectOutputStream}
import java.net.{InetAddress, InetSocketAddress}
import scala.concurrent.Await
import scala.concurrent.duration._

class PeerEndpointProvenanceSpec extends ErgoCorePropertyTest with DBSpec {

  import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.{
    AddVerifiedOutboundPeer,
    GetAllPeers,
    SeenPeers
  }
  import scorex.core.network.NetworkController.ReceivableMessages.{ConnectTo, SendToNetwork}

  private val claimed = new InetSocketAddress("198.51.100.20", 9030)
  private val connected = new InetSocketAddress("203.0.113.21", 9031)
  private val otherClaim = new InetSocketAddress("192.0.2.23", 9033)
  private val localClaim = new InetSocketAddress("192.0.2.24", 9034)

  private class Fixture(allowLocal: Boolean = settings.scorexSettings.network.allowLocal,
                        bindAddress: InetSocketAddress = settings.scorexSettings.network.bindAddress,
                        knownPeers: Seq[InetSocketAddress] = Seq(claimed, connected),
                        contextOverride: Option[ScorexContext] = None,
                        persistedPeers: Seq[PeerInfo] = Seq.empty) extends AkkaFixture {
    implicit val ec = system.dispatcher
    implicit val actorSystem: ActorSystem = system

    val directory: File = createTempDir
    val config = settings.copy(
      directory = directory.getAbsolutePath,
      scorexSettings = settings.scorexSettings.copy(
        network = settings.scorexSettings.network.copy(
          knownPeers = knownPeers,
          allowLocal = allowLocal,
          bindAddress = bindAddress
        )
      )
    )
    if (persistedPeers.nonEmpty) {
      val store = LDBFactory.createKvDb(new File(directory, "peers").getAbsolutePath)
      try {
        persistedPeers.foreach { peerInfo =>
          val address = peerInfo.peerSpec.declaredAddress.get
          val keyBytes = new ByteArrayOutputStream()
          val keyStream = new ObjectOutputStream(keyBytes)
          keyStream.writeObject(address)
          keyStream.close()
          store.insert(keyBytes.toByteArray, PeerInfoSerializer.toBytes(peerInfo)).get
        }
      } finally store.close()
    }

    val context = contextOverride.getOrElse(ScorexContext(Seq.empty, None, None))
    val peerManager: ActorRef = system.actorOf(PeerManagerRef.props(config, context))
    val tcpManager = TestProbe("TcpManager")
    val events = TestProbe("PeerEvents")
    system.eventStream.subscribe(events.ref, classOf[HandshakedPeer])
    val lifecycle = TestProbe("PeerLifecycle")
    system.eventStream.subscribe(lifecycle.ref, classOf[HandshakedPeer])
    system.eventStream.subscribe(lifecycle.ref, classOf[DisconnectedPeer])
    val forwardedMessages = TestProbe("ForwardedMessages")

    val controller: TestActorRef[NetworkController] = TestActorRef(new NetworkController(
      config,
      peerManager,
      context,
      tcpManager.ref,
      _ => Map[MessageCode, ActorRef](GetPeersSpec.messageCode -> forwardedMessages.ref)
    ))
    tcpManager.expectMsgType[Tcp.Bind]

    def allPeers: Map[InetSocketAddress, PeerInfo] = {
      val query = TestProbe()
      query.send(peerManager, GetAllPeers)
      query.expectMsgType[Map[InetSocketAddress, PeerInfo]]
    }

    def awaitPeers(check: Map[InetSocketAddress, PeerInfo] => Unit): Unit = {
      val query = TestProbe()
      query.awaitAssert(check(allPeers))
    }

    def connectedPeers: Iterable[ConnectedPeer] = {
      val query = TestProbe()
      query.send(controller, NetworkController.ReceivableMessages.GetConnectedPeers)
      query.expectMsgType[Iterable[ConnectedPeer]]
    }

    private def receiveHandshake(connection: TestProbe, advertised: PeerSpec): (HandshakedPeer, Handshake) = {
      val handler = connection.expectMsgType[Tcp.Register].handler
      connection.expectMsg(Tcp.ResumeReading)
      val ownHandshake = HandshakeSerializer.parseBytesTry(connection.expectMsgType[Tcp.Write].data.toArray).get
      val bytes = HandshakeSerializer.toBytes(Handshake(advertised, System.currentTimeMillis()))
      connection.send(handler, Tcp.Received(ByteString(bytes)))
      connection.expectMsg(Tcp.ResumeReading)
      val event = events.expectMsgType[HandshakedPeer]
      event -> ownHandshake
    }

    def incoming(remote: InetSocketAddress, advertised: PeerSpec): HandshakedPeer = {
      incomingAtLocal(remote, config.scorexSettings.network.bindAddress, advertised)
    }

    def incomingAtLocal(remote: InetSocketAddress,
                        local: InetSocketAddress,
                        advertised: PeerSpec): HandshakedPeer = {
      val connection = TestProbe()
      connection.send(controller, Tcp.Connected(remote, local))
      receiveHandshake(connection, advertised)._1
    }

    def beginDial(address: InetSocketAddress): Unit = {
      controller ! ConnectTo(PeerInfo.fromAddress(address))
      tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe address
    }

    def completedDial(address: InetSocketAddress, advertised: PeerSpec): HandshakedPeer = {
      val local = new InetSocketAddress("192.0.2.77", 55010)
      completedDialAtLocal(address, local, advertised)._1
    }

    def completedDialAtLocal(address: InetSocketAddress,
                             local: InetSocketAddress,
                             advertised: PeerSpec): (HandshakedPeer, Handshake) = {
      val connection = TestProbe()
      connection.send(controller, Tcp.Connected(address, local))
      receiveHandshake(connection, advertised)
    }

    def incomingWithLocal(remote: InetSocketAddress,
                          local: InetSocketAddress,
                          advertised: PeerSpec): Handshake = {
      val connection = TestProbe()
      connection.send(controller, Tcp.Connected(remote, local))
      receiveHandshake(connection, advertised)._2
    }

    def outgoing(address: InetSocketAddress, advertised: PeerSpec): HandshakedPeer = {
      beginDial(address)
      completedDial(address, advertised)
    }
  }

  property("an inbound address is only a dial candidate until an outbound handshake") {
    val f = new Fixture
    try {
      val candidateAddress = new InetSocketAddress("192.0.2.25", 9035)
      val inboundSource = new InetSocketAddress("198.51.100.40", 55001)
      val advertised = defaultPeerSpec.copy(
        declaredAddress = Some(candidateAddress),
        features = Seq(LocalAddressPeerFeature(localClaim))
      )
      f.incoming(inboundSource, advertised)

      f.awaitPeers { peers =>
        val candidate = peers(candidateAddress)
        candidate.lastHandshake shouldBe 0L
        candidate.connectionType shouldBe None
        candidate.peerSpec.declaredAddress shouldBe Some(candidateAddress)
        candidate.peerSpec.localAddressOpt shouldBe None
        peers(claimed).lastHandshake shouldBe 0L
      }

      val query = TestProbe()(f.system)
      query.send(f.peerManager, SeenPeers(10))
      query.expectMsgType[Seq[PeerInfo]].map(_.peerSpec.address) should not contain Some(candidateAddress)

      f.outgoing(candidateAddress, defaultPeerSpec.copy(declaredAddress = Some(candidateAddress)))
      f.awaitPeers { peers =>
        val verified = peers(candidateAddress)
        verified.lastHandshake should be > 0L
        verified.connectionType shouldBe Some(Outgoing)
        verified.peerSpec.address shouldBe Some(candidateAddress)
      }
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("a matching LAN address is a candidate only when local dialing is allowed") {
    val lanAddress = new InetSocketAddress("192.168.44.200", 9030)
    val inboundSource = new InetSocketAddress(lanAddress.getAddress, 55002)
    val advertised = defaultPeerSpec.copy(
      declaredAddress = Some(otherClaim),
      features = Seq(LocalAddressPeerFeature(lanAddress))
    )

    Seq(true, false).foreach { allowLocal =>
      val f = new Fixture(allowLocal = allowLocal)
      try {
        f.incoming(inboundSource, advertised)
        val chosen = if (allowLocal) lanAddress else otherClaim
        val rejected = if (allowLocal) otherClaim else lanAddress
        f.awaitPeers { stored =>
          stored.keySet should contain(chosen)
          stored.keySet should not contain rejected
          stored(chosen).lastHandshake shouldBe 0L
          stored(chosen).connectionType shouldBe None
          stored(chosen).peerSpec.localAddressOpt shouldBe None
        }
      } finally {
        Await.result(f.system.terminate(), Duration.Inf)
        deleteRecursive(f.directory)
      }
    }
  }

  property("an IPv6 unique local claim is not discovered when local dialing is disabled") {
    val f = new Fixture(allowLocal = false)
    try {
      val ulaClaim = new InetSocketAddress("fd12:3456::1", 9036)
      val inboundSource = new InetSocketAddress("198.51.100.41", 55003)
      f.incoming(inboundSource, defaultPeerSpec.copy(declaredAddress = Some(ulaClaim)))
      f.awaitPeers { peers =>
        peers.keySet should not contain ulaClaim
      }
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("an allowed IPv6 unique local link sends a usable declared address on the handshake wire") {
    val localBind = new InetSocketAddress("fd12:3456::2", settings.scorexSettings.network.bindAddress.getPort)
    val f = new Fixture(allowLocal = true, bindAddress = localBind)
    try {
      val remote = new InetSocketAddress("fd12:3456::1", 55004)
      val ownHandshake = f.incomingWithLocal(remote, localBind, defaultPeerSpec.copy(declaredAddress = Some(remote)))
      ownHandshake.peerSpec.declaredAddress shouldBe Some(localBind)
      ownHandshake.peerSpec.localAddressOpt shouldBe None
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("a pending dial does not turn an inbound socket into a verified outbound endpoint") {
    val f = new Fixture
    try {
      f.beginDial(claimed)
      val inbound = f.incoming(claimed, defaultPeerSpec.copy(declaredAddress = Some(claimed)))
      inbound.remote.connectionId.direction shouldBe Incoming
      f.awaitPeers { peers =>
        peers(claimed).lastHandshake shouldBe 0L
      }

      val outbound = f.completedDial(claimed, defaultPeerSpec.copy(declaredAddress = Some(claimed)))
      outbound.remote.connectionId.direction shouldBe Outgoing
      f.awaitPeers { peers =>
        peers(claimed).lastHandshake should be > 0L
      }
      f.lifecycle.expectMsgType[HandshakedPeer].remote.handlerRef shouldBe inbound.remote.handlerRef
      f.lifecycle.expectMsgType[DisconnectedPeer].peer.handlerRef shouldBe inbound.remote.handlerRef
      f.lifecycle.expectMsgType[HandshakedPeer].remote.handlerRef shouldBe outbound.remote.handlerRef

      val liveMessage = Message[Unit](GetPeersSpec, Right(()), Some(outbound.remote))
      f.controller ! liveMessage
      f.forwardedMessages.expectMsg(liveMessage)
      f.controller ! Message[Unit](GetPeersSpec, Right(()), Some(inbound.remote))
      f.forwardedMessages.expectNoMessage(200.millis)

      f.system.stop(inbound.remote.handlerRef)
      f.lifecycle.expectNoMessage(200.millis)
      f.connectedPeers.map(_.handlerRef) should contain(outbound.remote.handlerRef)
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("closing an inbound socket leaves an unrelated pending dial active") {
    val f = new Fixture
    try {
      f.beginDial(claimed)
      val inbound = f.incoming(claimed, defaultPeerSpec.copy(declaredAddress = Some(claimed)))
      inbound.remote.connectionId.direction shouldBe Incoming
      f.system.stop(inbound.remote.handlerRef)
      f.lifecycle.expectMsgType[HandshakedPeer].remote.handlerRef shouldBe inbound.remote.handlerRef
      f.lifecycle.expectMsgType[DisconnectedPeer].peer.handlerRef shouldBe inbound.remote.handlerRef

      val outbound = f.completedDial(claimed, defaultPeerSpec.copy(declaredAddress = Some(claimed)))
      outbound.remote.connectionId.direction shouldBe Outgoing
      f.awaitPeers { peers =>
        peers(claimed).lastHandshake should be > 0L
      }
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("a dynamic listener port distinguishes an inbound socket from its pending dial") {
    val requestedBind = new InetSocketAddress(settings.scorexSettings.network.bindAddress.getAddress, 0)
    val f = new Fixture(bindAddress = requestedBind)
    try {
      val actualBind = new InetSocketAddress(requestedBind.getAddress, 9120)
      f.tcpManager.send(f.controller, Tcp.Bound(actualBind))
      f.beginDial(claimed)
      val inbound = f.incomingAtLocal(claimed, actualBind, defaultPeerSpec.copy(declaredAddress = Some(claimed)))
      inbound.remote.connectionId.direction shouldBe Incoming
      f.awaitPeers { peers =>
        peers(claimed).lastHandshake shouldBe 0L
      }

      val outbound = f.completedDial(claimed, defaultPeerSpec.copy(declaredAddress = Some(claimed)))
      outbound.remote.connectionId.direction shouldBe Outgoing
      f.awaitPeers { peers =>
        peers(claimed).lastHandshake should be > 0L
      }
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("a dynamic IPv4 listener advertises its actual port in the local-address feature") {
    val requestedBind = new InetSocketAddress(settings.scorexSettings.network.bindAddress.getAddress, 0)
    val f = new Fixture(allowLocal = true, bindAddress = requestedBind)
    try {
      val actualBind = new InetSocketAddress(requestedBind.getAddress, 9120)
      val local = new InetSocketAddress("192.168.44.201", actualBind.getPort)
      val remote = new InetSocketAddress("192.168.44.202", 55012)
      f.tcpManager.send(f.controller, Tcp.Bound(actualBind))
      val ownHandshake = f.incomingWithLocal(remote, local, defaultPeerSpec.copy(declaredAddress = Some(remote)))
      ownHandshake.peerSpec.localAddressOpt shouldBe Some(local)
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("an outbound IPv6 unique local link advertises its bound listening endpoint") {
    val localBind = new InetSocketAddress("fd12:3456::2", settings.scorexSettings.network.bindAddress.getPort)
    val f = new Fixture(allowLocal = true, bindAddress = localBind)
    try {
      val remote = new InetSocketAddress("fd12:3456::3", 9037)
      val localEphemeral = new InetSocketAddress(localBind.getAddress, 55011)
      f.beginDial(remote)
      val (event, ownHandshake) = f.completedDialAtLocal(
        remote, localEphemeral, defaultPeerSpec.copy(declaredAddress = Some(remote)))
      event.remote.connectionId.direction shouldBe Outgoing
      ownHandshake.peerSpec.declaredAddress shouldBe Some(localBind)
      ownHandshake.peerSpec.localAddressOpt shouldBe None
      f.awaitPeers { peers =>
        peers(remote).connectionType shouldBe Some(Outgoing)
      }
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("a declared endpoint is dialed directly despite a claimed local shortcut") {
    val f = new Fixture
    try {
      val declared = new InetSocketAddress("198.51.100.70", 9070)
      val shortcut = new InetSocketAddress("192.168.44.70", 9070)
      val peer = PeerInfo(
        defaultPeerSpec.copy(
          declaredAddress = Some(declared),
          features = Seq(LocalAddressPeerFeature(shortcut))
        ),
        0L,
        None
      )

      f.controller ! ConnectTo(peer)
      f.tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe declared
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("a UPnP mapping cannot redirect a declared endpoint") {
    val declared = new InetSocketAddress("198.51.100.71", 9071)
    val mapped = new InetSocketAddress("192.168.44.71", 19071)
    val gateway = new UPnPGateway {
      override val localAddress: InetAddress = mapped.getAddress
      override val externalAddress: InetAddress = declared.getAddress
      override def addPort(port: Int): Unit = ()
      override def deletePort(port: Int): Unit = ()
      override def getLocalAddressForExternalPort(externalPort: Int): Option[InetSocketAddress] =
        Some(mapped)
    }
    val context = ScorexContext(Seq.empty, Some(gateway), Some(declared))
    val f = new Fixture(contextOverride = Some(context))
    try {
      f.controller ! ConnectTo(PeerInfo.fromAddress(declared))
      f.tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe declared
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("a WAN relay contributes its public endpoint without its private shortcut") {
    val f = new Fixture(allowLocal = true)
    try {
      val source = f.incoming(
        new InetSocketAddress("203.0.113.72", 55072),
        defaultPeerSpec.copy(declaredAddress = None)
      )
      val public = new InetSocketAddress("198.51.100.72", 9072)
      val privateShortcut = new InetSocketAddress("192.168.44.72", 9072)
      val relayed = defaultPeerSpec.copy(
        declaredAddress = Some(public),
        features = Seq(LocalAddressPeerFeature(privateShortcut))
      )
      val network = TestProbe()(f.system)
      val synchronizer = f.system.actorOf(
        PeerSynchronizerRef.props(network.ref, f.peerManager, f.config.scorexSettings.network)(f.ec)
      )
      val peersSpec = new PeersSpec(f.config.scorexSettings.network.maxPeerSpecObjects)

      synchronizer ! Message(peersSpec, Left(peersSpec.toBytes(Seq(relayed))), Some(source.remote))
      f.awaitPeers { peers =>
        peers.keySet should contain(public)
        peers.keySet should not contain privateShortcut
        peers(public).peerSpec.localAddressOpt shouldBe None
      }
      f.controller ! ConnectTo(f.allPeers(public))
      f.tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe public
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("a LAN relay contributes a matching local-only endpoint as an unverified candidate") {
    val sourceAddress = new InetSocketAddress("192.168.44.73", 55073)
    val candidate = new InetSocketAddress(sourceAddress.getAddress, 9073)
    val f = new Fixture(allowLocal = true)
    try {
      val source = f.incoming(sourceAddress, defaultPeerSpec.copy(declaredAddress = None))
      val relayed = defaultPeerSpec.copy(
        declaredAddress = None,
        features = Seq(LocalAddressPeerFeature(candidate))
      )
      val network = TestProbe()(f.system)
      val synchronizer = f.system.actorOf(
        PeerSynchronizerRef.props(network.ref, f.peerManager, f.config.scorexSettings.network)(f.ec)
      )
      val peersSpec = new PeersSpec(f.config.scorexSettings.network.maxPeerSpecObjects)

      synchronizer ! Message(peersSpec, Left(peersSpec.toBytes(Seq(relayed))), Some(source.remote))
      f.awaitPeers { peers =>
        peers.keySet should contain(candidate)
        peers(candidate).lastHandshake shouldBe 0L
        peers(candidate).connectionType shouldBe None
        peers(candidate).peerSpec.declaredAddress shouldBe Some(candidate)
        peers(candidate).peerSpec.localAddressOpt shouldBe None
      }
      f.controller ! ConnectTo(f.allPeers(candidate))
      f.tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe candidate
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("private gossip is shared only with an outgoing LAN recipient") {
    val f = new Fixture(allowLocal = true)
    try {
      val privateEndpoint = new InetSocketAddress("192.168.44.74", 9074)
      val wanEndpoint = new InetSocketAddress("198.51.100.74", 9075)
      val inboundEndpoint = new InetSocketAddress("192.168.44.75", 55075)
      val privatePeer = f.outgoing(
        privateEndpoint,
        defaultPeerSpec.copy(declaredAddress = Some(privateEndpoint))
      )
      val wanPeer = f.outgoing(
        wanEndpoint,
        defaultPeerSpec.copy(declaredAddress = Some(wanEndpoint))
      )
      val inboundPeer = f.incoming(
        inboundEndpoint,
        defaultPeerSpec.copy(declaredAddress = None)
      )
      val network = TestProbe()(f.system)
      val synchronizer = f.system.actorOf(
        PeerSynchronizerRef.props(network.ref, f.peerManager, f.config.scorexSettings.network)(f.ec)
      )

      def gossipTo(recipient: ConnectedPeer): Set[InetSocketAddress] = {
        synchronizer ! Message[Unit](GetPeersSpec, Left(Array.emptyByteArray), Some(recipient))
        val sent = network.fishForMessage(5.seconds) {
          case SendToNetwork(message, SendToPeer(peer))
            if message.spec.messageCode == PeersSpec.messageCode && peer == recipient => true
          case _ => false
        }.asInstanceOf[SendToNetwork]
        sent.message.input.right.get.asInstanceOf[Seq[PeerSpec]].flatMap(_.declaredAddress).toSet
      }

      gossipTo(privatePeer.remote) should contain(privateEndpoint)
      gossipTo(wanPeer.remote) should not contain privateEndpoint
      gossipTo(inboundPeer.remote) should not contain privateEndpoint
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("a configured LAN endpoint is recovered from a nonempty migrated store and dialed exactly") {
    val configuredLan = new InetSocketAddress("192.168.44.76", 9076)
    val f = new Fixture(
      allowLocal = true,
      knownPeers = Seq(configuredLan),
      persistedPeers = Seq(PeerInfo.fromAddress(connected))
    )
    try {
      val recovered = f.allPeers(configuredLan)
      recovered.lastHandshake shouldBe 0L
      recovered.connectionType shouldBe None

      val choice = org.ergoplatform.network.peer.PeerManager.ReceivableMessages
        .RandomPeerExcluding(Seq.empty)
        .choose(f.allPeers.filter(_._1 == configuredLan), Seq.empty, f.context)
        .get
      f.controller ! ConnectTo(choice)
      f.tcpManager.expectMsgType[Tcp.Connect].remoteAddress shouldBe configuredLan

      val (_, ownHandshake) = f.completedDialAtLocal(
        configuredLan,
        new InetSocketAddress("192.168.44.10", 55076),
        defaultPeerSpec.copy(declaredAddress = Some(configuredLan))
      )
      ownHandshake.peerSpec.localAddressOpt.map(_.getPort) shouldBe
        Some(f.config.scorexSettings.network.bindAddress.getPort)
      f.awaitPeers { peers =>
        peers(configuredLan).peerSpec.declaredAddress shouldBe Some(configuredLan)
        peers(configuredLan).connectionType shouldBe Some(Outgoing)
        peers(configuredLan).lastHandshake should be > 0L
      }
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("the manager accepts only fully verified outbound peer records") {
    val f = new Fixture
    try {
      val endpoint = new InetSocketAddress("198.51.100.79", 9079)
      val otherEndpoint = new InetSocketAddress("198.51.100.80", 9080)
      val localShortcut = new InetSocketAddress("192.168.44.79", 9079)
      val valid = PeerInfo(
        defaultPeerSpec.copy(declaredAddress = Some(endpoint), features = Seq.empty),
        lastHandshake = 123L,
        connectionType = Some(Outgoing),
        lastStoredActivityTime = 456L
      )
      val writer = TestProbe()(f.system)

      writer.send(f.peerManager, AddVerifiedOutboundPeer(valid, endpoint))
      writer.send(f.peerManager, GetAllPeers)
      val validSnapshot = writer.expectMsgType[Map[InetSocketAddress, PeerInfo]]
      validSnapshot(endpoint) shouldBe valid

      val rejected = Seq(
        valid.copy(connectionType = Some(Incoming)),
        valid.copy(connectionType = None),
        valid.copy(lastHandshake = 0L),
        valid.copy(peerSpec = valid.peerSpec.copy(declaredAddress = Some(otherEndpoint))),
        valid.copy(peerSpec = valid.peerSpec.copy(
          features = Seq(LocalAddressPeerFeature(localShortcut))))
      )
      rejected.foreach { candidate =>
        writer.send(f.peerManager, AddVerifiedOutboundPeer(candidate, endpoint))
        writer.send(f.peerManager, GetAllPeers)
        writer.expectMsgType[Map[InetSocketAddress, PeerInfo]] shouldBe validSnapshot
      }
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }

  property("GetPeers falls back to a stale public endpoint when a recent LAN peer is ineligible") {
    val f = new Fixture(allowLocal = true)
    try {
      val now = System.currentTimeMillis()
      val recentLan = new InetSocketAddress("192.168.44.77", 9077)
      val stalePublic = new InetSocketAddress("198.51.100.77", 9078)
      val writer = TestProbe()(f.system)
      writer.send(
        f.peerManager,
        AddVerifiedOutboundPeer(
          PeerInfo.fromAddress(recentLan).copy(
            lastHandshake = now,
            connectionType = Some(Outgoing),
            lastStoredActivityTime = now
          ),
          recentLan
        )
      )
      writer.send(
        f.peerManager,
        AddVerifiedOutboundPeer(
          PeerInfo.fromAddress(stalePublic).copy(
            lastHandshake = now - 4.hours.toMillis,
            connectionType = Some(Outgoing),
            lastStoredActivityTime = now - 4.hours.toMillis
          ),
          stalePublic
        )
      )
      writer.send(f.peerManager, SeenPeers(10, allowLocal = true))
      writer.expectMsgType[Seq[PeerInfo]].flatMap(_.peerSpec.declaredAddress).toSet shouldBe
        Set(recentLan)

      val wanRecipient = f.incoming(
        new InetSocketAddress("203.0.113.77", 55077),
        defaultPeerSpec.copy(declaredAddress = None)
      ).remote
      val network = TestProbe()(f.system)
      val synchronizer = f.system.actorOf(
        PeerSynchronizerRef.props(network.ref, f.peerManager, f.config.scorexSettings.network)(f.ec)
      )

      def gossipTo(recipient: ConnectedPeer): Set[InetSocketAddress] = {
        synchronizer ! Message[Unit](GetPeersSpec, Left(Array.emptyByteArray), Some(recipient))
        val sent = network.fishForMessage(5.seconds) {
          case SendToNetwork(message, SendToPeer(peer))
            if message.spec.messageCode == PeersSpec.messageCode && peer == recipient => true
          case _ => false
        }.asInstanceOf[SendToNetwork]
        sent.message.input.right.get.asInstanceOf[Seq[PeerSpec]].flatMap(_.declaredAddress).toSet
      }

      val wanResponse = gossipTo(wanRecipient)
      wanResponse should contain(stalePublic)
      wanResponse should not contain recentLan
      val lanRecipient = f.outgoing(
        recentLan,
        defaultPeerSpec.copy(declaredAddress = Some(recentLan))
      ).remote
      gossipTo(lanRecipient) should contain(recentLan)
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
      deleteRecursive(f.directory)
    }
  }
}
