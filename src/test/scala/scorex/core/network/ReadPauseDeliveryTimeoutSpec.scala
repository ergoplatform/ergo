package scorex.core.network

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, ChangedMempool, HandshakedPeer}
import org.ergoplatform.network.message._
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.network.{ErgoNodeViewSynchronizer, ErgoSyncTracker, Handshake, HandshakeSerializer, ModePeerFeature}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetNodeViewChanges
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.app.ScorexContext
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
import scorex.testkit.utils.AkkaFixture
import scorex.util.ModifierId

import java.net.InetSocketAddress
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

object ReadPauseDeliveryTimeoutSpec {
  final case class RequestHeaders(ids: Seq[ModifierId], peer: ConnectedPeer)

  class RequestingSynchronizer(nc: ActorRef,
                               vh: ActorRef,
                               settings: ErgoSettings,
                               syncTracker: ErgoSyncTracker,
                               deliveryTracker: DeliveryTracker)
                              (implicit ec: scala.concurrent.ExecutionContext)
    extends ErgoNodeViewSynchronizer(nc, vh, ErgoSyncInfoMessageSpec, settings, syncTracker, deliveryTracker)(ec) {

    // lets the test make the real request path (requestBlockSection) run inside the actor
    override protected def processDataFromPeer(msgHandlers: PartialFunction[(MessageSpec[_], _, ConnectedPeer), Unit]): Receive = {
      val requests: Receive = {
        case RequestHeaders(ids, peer) => ids.foreach(id => requestBlockSection(Header.modifierTypeId, Seq(id), peer))
      }
      requests orElse super.processDataFromPeer(msgHandlers)
    }
  }
}

class ReadPauseDeliveryTimeoutSpec extends AnyPropSpec with Matchers {
  import ReadPauseDeliveryTimeoutSpec._
  import org.ergoplatform.network.peer.PeerManager.ReceivableMessages._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.HistoryTestHelpers._

  private val treeHasInboundCredits: Boolean = Try(
    Class.forName("scorex.core.network.PeerConnectionHandler$ReceivableMessages$InboundMessageProcessed")
  ).isSuccess

  private val modifiersFromRemoteProcessedOpt: Option[AnyRef] = Try(
    Class.forName("org.ergoplatform.nodeView.ErgoNodeViewHolder$ReceivableMessages$ModifiersFromRemoteProcessed$")
      .getField("MODULE$")
      .get(null)
  ).toOption

  property("a read pause suspends delivery timeouts for unread replies buffered in PeerConnectionHandler") {
    val fixture = new AkkaFixture {}
    try {
      implicit val system: ActorSystem = fixture.system
      implicit val ec: scala.concurrent.ExecutionContextExecutor = system.dispatcher

      val testSettings = settings.copy(
        scorexSettings = settings.scorexSettings.copy(
          network = settings.scorexSettings.network.copy(deliveryTimeout = 1.second)
        )
      )
      val netCfg = testSettings.scorexSettings.network

      val ncProbe = TestProbe("SynchronizerNC")
      val vhProbe = TestProbe("SilentViewHolder")
      val syncTracker = ErgoSyncTracker(netCfg)
      val tracker = DeliveryTracker.empty(testSettings)

      val h = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1)
      val headers = genHeaderChain(17, h, diffBitsOpt = None, useRealTs = false).headers
      val mp = ErgoMemPool.empty(testSettings)

      val sync = system.actorOf(Props(new RequestingSynchronizer(ncProbe.ref, vhProbe.ref, testSettings, syncTracker, tracker)))
      vhProbe.expectMsgType[GetNodeViewChanges]
      sync ! ChangedHistory(h)
      sync ! ChangedMempool(mp)

      val peerManager = TestProbe("PeerManager")
      val tcpManager = TestProbe("TcpManager")
      val controller = TestActorRef(new NetworkController(
        testSettings,
        peerManager.ref,
        ScorexContext(Seq(ModifiersSpec), None, None),
        tcpManager.ref,
        _ => Map(ModifiersSpec.messageCode -> sync)
      ))
      tcpManager.expectMsgType[Tcp.Bind]
      controller ! Tcp.Bound(netCfg.bindAddress)

      val events = TestProbe("Handshakes")
      system.eventStream.subscribe(events.ref, classOf[HandshakedPeer])
      val remoteA = new InetSocketAddress("203.0.113.60", 9060)
      val connectionA = TestProbe("ConnectionA")
      connectionA.send(controller, Tcp.Connected(remoteA, netCfg.bindAddress))
      peerManager.expectMsgPF(3.seconds) {
        case ConfirmConnection(_, handlerRef) =>
          controller ! ConnectionConfirmed(ConnectionId(remoteA, netCfg.bindAddress, Incoming), handlerRef)
      }
      val handlerA = connectionA.expectMsgType[Tcp.Register].handler
      connectionA.receiveWhile(500.millis) { case m => m }
      val handshake = HandshakeSerializer.toBytes(
        Handshake(defaultPeerSpec.copy(declaredAddress = Some(remoteA)), System.currentTimeMillis())
      )
      connectionA.send(handlerA, Tcp.Received(ByteString(handshake)))
      val peerA = events.expectMsgType[HandshakedPeer](3.seconds).remote
      peerA.handlerRef shouldBe handlerA
      connectionA.receiveWhile(500.millis) { case m => m }

      val pchB = TestProbe("PCH-B")
      val specB = defaultPeerSpec.copy(features = Seq(
        ModePeerFeature(StateType.Utxo, true, None, ModePeerFeature.AllBlocksKept)
      ))
      val peerB = ConnectedPeer(connectionIdGen.sample.get, pchB.ref, Some(PeerInfo(specB, System.currentTimeMillis())))
      syncTracker.updateStatus(peerB, org.ergoplatform.consensus.Older, Some(1000))

      val requestedAt = System.nanoTime()
      sync ! RequestHeaders(headers.map(_.id), peerA)
      val drainDeadline = requestedAt + 3.seconds.toNanos
      var requestsToA = 0
      while (requestsToA < 17 && System.nanoTime() < drainDeadline) {
        ncProbe.receiveOne(100.millis) match {
          case SendToNetwork(msg, SendToPeer(to)) if to == peerA && msg.spec.messageCode == RequestModifierSpec.messageCode =>
            requestsToA += 1
          case _ =>
        }
      }
      requestsToA shouldBe 17
      headers.foreach(hd => tracker.status(hd.id, Header.modifierTypeId, Seq.empty) shouldBe ModifiersStatus.Requested)

      // A answers all 17 requests in a single TCP read
      val serializer = new MessageSerializer(Seq(ModifiersSpec), netCfg.magicBytes)
      val frames = headers.map { hd =>
        serializer.serialize(Message(
          ModifiersSpec,
          Right(ModifiersData(Header.modifierTypeId, Map(hd.id -> hd.bytes))),
          None
        ))
      }.reduce(_ ++ _)
      connectionA.send(handlerA, Tcp.Received(frames))
      val sentAt = System.nanoTime()
      (sentAt - requestedAt) should be < (netCfg.deliveryTimeout.toNanos / 2)

      // Observe past the 1 s delivery timeout while handlerA is read-suspended
      val deadline = sentAt + 3500.millis.toNanos
      val timed = Iterator.continually(ncProbe.receiveOne(100.millis)).takeWhile(_ => System.nanoTime() < deadline)
        .collect { case m if m != null => (System.nanoTime(), m) }.toList
      val timedRequests = timed.collect {
        case (at, SendToNetwork(msg, SendToPeer(to))) if msg.spec.messageCode == RequestModifierSpec.messageCode =>
          msg.input match {
            case Right(d: InvData) => ((at - requestedAt) / 1000000, to, d.ids)
            case _ => ((at - requestedAt) / 1000000, to, Seq.empty[ModifierId])
          }
      }
      val penalties = timed.collect {
        case (at, PenalizePeer(addr, t)) => ((at - requestedAt) / 1000000, addr, t)
      }
      val penaltiesForA = penalties.filter(_._2 == remoteA)
      val reRequests = timedRequests.map { case (_, to, ids) => (to, ids) }
      val connectionMsgs = connectionA.receiveWhile(300.millis) { case m => m }
      val item17 = headers.last.id
      val status17 = tracker.status(item17, Header.modifierTypeId, Seq.empty)

      if (treeHasInboundCredits) {
        connectionMsgs should contain(Tcp.SuspendReading)
        headers.init.foreach(hd => tracker.status(hd.id, Header.modifierTypeId, Seq.empty) shouldBe ModifiersStatus.Received)
        status17 shouldBe ModifiersStatus.Requested
        // Because peerA's delivery deadline is paused while its read is suspended, item 17 is not re-requested or penalized
        reRequests shouldBe empty
        penaltiesForA shouldBe empty

        // Once the view holder acknowledges one batch, handlerA drains item 17 from chunksBuffer and delivers it
        modifiersFromRemoteProcessedOpt.foreach(ack => vhProbe.send(sync, ack))
        vhProbe.awaitAssert(
          tracker.status(item17, Header.modifierTypeId, Seq.empty) shouldBe ModifiersStatus.Received,
          3.seconds,
          100.millis
        )
        // A second acknowledgement drops in-flight messages below 16 with chunksBuffer empty, resuming TCP reads
        modifiersFromRemoteProcessedOpt.foreach(ack => vhProbe.send(sync, ack))
        connectionA.expectMsg(3.seconds, Tcp.ResumeReading)
      } else {
        status17 shouldBe ModifiersStatus.Received
        reRequests shouldBe empty
        penaltiesForA shouldBe empty
      }
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  property("a missing reply is re-requested from another peer after reading resumes") {
    if (treeHasInboundCredits) {
      val fixture = new AkkaFixture {}
      try {
        implicit val system: ActorSystem = fixture.system
        implicit val ec: scala.concurrent.ExecutionContextExecutor = system.dispatcher

        val testSettings = settings.copy(
          scorexSettings = settings.scorexSettings.copy(
            network = settings.scorexSettings.network.copy(deliveryTimeout = 1.second)
          )
        )
        val netCfg = testSettings.scorexSettings.network

        val ncProbe = TestProbe("SynchronizerNC")
        val vhProbe = TestProbe("SilentViewHolder")
        val syncTracker = ErgoSyncTracker(netCfg)
        val tracker = DeliveryTracker.empty(testSettings)

        val h = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1)
        val headers = genHeaderChain(17, h, diffBitsOpt = None, useRealTs = false).headers
        val mp = ErgoMemPool.empty(testSettings)

        val sync = system.actorOf(Props(new RequestingSynchronizer(ncProbe.ref, vhProbe.ref, testSettings, syncTracker, tracker)))
        vhProbe.expectMsgType[GetNodeViewChanges]
        sync ! ChangedHistory(h)
        sync ! ChangedMempool(mp)

        val peerManager = TestProbe("PeerManager")
        val tcpManager = TestProbe("TcpManager")
        val controller = TestActorRef(new NetworkController(
          testSettings,
          peerManager.ref,
          ScorexContext(Seq(ModifiersSpec), None, None),
          tcpManager.ref,
          _ => Map(ModifiersSpec.messageCode -> sync)
        ))
        tcpManager.expectMsgType[Tcp.Bind]
        controller ! Tcp.Bound(netCfg.bindAddress)

        val events = TestProbe("Handshakes")
        system.eventStream.subscribe(events.ref, classOf[HandshakedPeer])
        val remoteA = new InetSocketAddress("203.0.113.61", 9061)
        val connectionA = TestProbe("ConnectionA")
        connectionA.send(controller, Tcp.Connected(remoteA, netCfg.bindAddress))
        peerManager.expectMsgPF(3.seconds) {
          case ConfirmConnection(_, handlerRef) =>
            controller ! ConnectionConfirmed(ConnectionId(remoteA, netCfg.bindAddress, Incoming), handlerRef)
        }
        val handlerA = connectionA.expectMsgType[Tcp.Register].handler
        connectionA.receiveWhile(500.millis) { case m => m }
        val handshake = HandshakeSerializer.toBytes(
          Handshake(defaultPeerSpec.copy(declaredAddress = Some(remoteA)), System.currentTimeMillis())
        )
        connectionA.send(handlerA, Tcp.Received(ByteString(handshake)))
        val peerA = events.expectMsgType[HandshakedPeer](3.seconds).remote
        connectionA.receiveWhile(500.millis) { case m => m }

        val pchB = TestProbe("PCH-B")
        val specB = defaultPeerSpec.copy(features = Seq(
          ModePeerFeature(StateType.Utxo, true, None, ModePeerFeature.AllBlocksKept)
        ))
        val peerB = ConnectedPeer(connectionIdGen.sample.get, pchB.ref, Some(PeerInfo(specB, System.currentTimeMillis())))
        syncTracker.updateStatus(peerB, org.ergoplatform.consensus.Older, Some(1000))

        val requestedAt = System.nanoTime()
        sync ! RequestHeaders(headers.map(_.id), peerA)
        val drainDeadline = requestedAt + 3.seconds.toNanos
        var requestsToA = 0
        while (requestsToA < 17 && System.nanoTime() < drainDeadline) {
          ncProbe.receiveOne(100.millis) match {
            case SendToNetwork(msg, SendToPeer(to)) if to == peerA && msg.spec.messageCode == RequestModifierSpec.messageCode =>
              requestsToA += 1
            case _ =>
          }
        }
        requestsToA shouldBe 17

        // A sends only the first 16 headers and never sends header 17
        val serializer = new MessageSerializer(Seq(ModifiersSpec), netCfg.magicBytes)
        val frames16 = headers.init.map { hd =>
          serializer.serialize(Message(
            ModifiersSpec,
            Right(ModifiersData(Header.modifierTypeId, Map(hd.id -> hd.bytes))),
            None
          ))
        }.reduce(_ ++ _)
        connectionA.send(handlerA, Tcp.Received(frames16))
        connectionA.expectMsg(3.seconds, Tcp.SuspendReading)

        val item17 = headers.last.id
        // Wait past the 1 s timeout while reading is suspended: no re-request yet
        val pauseEnd = System.nanoTime() + 1500.millis.toNanos
        val whilePaused = Iterator.continually(ncProbe.receiveOne(100.millis)).takeWhile(_ => System.nanoTime() < pauseEnd)
          .collect {
            case SendToNetwork(msg, SendToPeer(to)) if msg.spec.messageCode == RequestModifierSpec.messageCode => to
          }.toList
        whilePaused shouldBe empty

        // Resume reading by acknowledging one batch; the paused delivery timer for item 17 resumes and fires
        modifiersFromRemoteProcessedOpt.foreach(ack => vhProbe.send(sync, ack))
        connectionA.expectMsg(3.seconds, Tcp.ResumeReading)

        val resumeDeadline = System.nanoTime() + 3000.millis.toNanos
        val afterResume = Iterator.continually(ncProbe.receiveOne(100.millis)).takeWhile(_ => System.nanoTime() < resumeDeadline)
          .collect {
            case SendToNetwork(msg, SendToPeer(to)) if msg.spec.messageCode == RequestModifierSpec.messageCode =>
              msg.input match {
                case Right(d: InvData) => (to, d.ids)
                case _ => (to, Seq.empty[ModifierId])
              }
          }.toList
        afterResume.exists { case (to, ids) => to == peerB && ids == Seq(item17) } shouldBe true
      } finally {
        Await.result(fixture.system.terminate(), Duration.Inf)
      }
    }
  }
}
