package org.ergoplatform.network.llm_generated

import akka.actor.{ActorRef, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.consensus.{Equal, Fork, Older, Unknown, Younger}
import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.{ErgoNodeViewSynchronizer, ErgoSyncTracker, ModePeerFeature, Version}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, ChangedMempool, DisconnectedPeer, HandshakedPeer}
import org.ergoplatform.network.message.Message
import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec, ErgoSyncInfoV2}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, StateType, UtxoState}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.{ConnectedPeer, DeliveryTracker, SendToPeer}
import scorex.crypto.authds.LeafData
import scorex.testkit.utils.AkkaFixture
import scorex.util.{bytesToId, idToBytes}
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.data.TrivialProp.TrueProp
import sigma.interpreter.ProverResult

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

/** Replay is scoped to the first V2 response which establishes an equal peer at the local full-block height. */
class MatrixInputTipSyncSpec extends AnyPropSpec with Matchers with FileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._

  private class Synchronizer(nc: ActorRef, vh: ActorRef, cfg: ErgoSettings,
                             val statuses: ErgoSyncTracker)(implicit ec: ExecutionContext)
    extends ErgoNodeViewSynchronizer(nc, vh, ErgoSyncInfoMessageSpec, cfg,
      statuses, DeliveryTracker.empty(cfg)) {
    def outbound(history: ErgoHistory): Unit = sendSync(history)
    def inboundV2(history: ErgoHistory, peer: ConnectedPeer): Unit =
      processSyncV2(history, history.syncInfoV2(full = true), peer)
    def inboundV2(history: ErgoHistory, info: ErgoSyncInfoV2, peer: ConnectedPeer): Unit =
      processSyncV2(history, info, peer)
    def inboundV1(history: ErgoHistory, peer: ConnectedPeer): Unit =
      processSyncV1(history, history.syncInfoV1, peer)
    def handshaked(peer: ConnectedPeer): Unit = peerManagerEvents(HandshakedPeer(peer))
    def disconnected(peer: ConnectedPeer): Unit = peerManagerEvents(DisconnectedPeer(peer))
  }

  private class Fixture(initialHeight: Int) extends AkkaFixture {
    implicit val ec: ExecutionContext = system.dispatcher
    lazy val nc: TestProbe = TestProbe()
    lazy val vh: TestProbe = TestProbe()
    val cfg: ErgoSettings = settings.copy(directory = createTempDir.getAbsolutePath)
    val boxes: Seq[ErgoBox] = (0 until 4).map { index =>
      new ErgoBox(value = 1000000000L, ergoTree = ErgoTree.fromProposition(TrueProp), creationHeight = 0,
        additionalTokens = Colls.emptyColl, additionalRegisters = Map.empty,
        transactionId = bytesToId(Algos.hash(s"matrix-input-tip-sync-box-$index")), index = index.toShort)
    }
    var state: UtxoState = _
    var history: ErgoHistory = _
    var orderingChain: Seq[ErgoFullBlock] = Seq.empty
    val tracker: ErgoSyncTracker = ErgoSyncTracker(cfg.scorexSettings.network)
    var sync: Synchronizer = _

    def initialize(): Unit = {
      state = UtxoState.fromBoxHolder(BoxHolder(boxes), None, createTempDir, cfg, parameters)
      history = ErgoHistory.readOrGenerate(cfg)(null)
      orderingChain = genChain(height = initialHeight, history = history, stateOpt = Some(state))
      history = applyChain(history, orderingChain)
      history.fullBlockHeight shouldBe initialHeight
      val ref: TestActorRef[Synchronizer] = TestActorRef(Props(new Synchronizer(nc.ref, vh.ref, cfg, tracker)))
      ref ! ChangedHistory(history)
      ref ! ChangedMempool(ErgoMemPool.empty(cfg))
      sync = ref.underlyingActor
    }

    def peer(version: Version = Version.SubblocksVersion,
             mode: Option[StateType] = Some(StateType.Utxo)): ConnectedPeer = {
      val spec = defaultPeerSpec.copy(protocolVersion = version,
        features = mode.toSeq.map(t => ModePeerFeature(t, true, None, -1)))
      ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref,
        Some(PeerInfo(spec, System.currentTimeMillis())))
    }

    def transactions(count: Int): Seq[ErgoTransaction] = boxes.take(count).map { box =>
      new ErgoTransaction(IndexedSeq(Input(box.id, ProverResult.empty)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(box.value, box.ergoTree, 0, box.additionalTokens, Map.empty)))
    }

    def announcement(parent: Option[InputBlockAnnouncement] = None,
                     transactions: Seq[ErgoTransaction] = Seq.empty): InputBlockAnnouncement = {
      val empty = InputBlockFields.empty
      val fields = new InputBlockFields(parent.map(a => idToBytes(a.id)),
        Algos.merkleTreeRoot(transactions.map(tx => LeafData @@ tx.serializedId)),
        empty.prevTransactionsDigest, empty.inputBlockFieldsProof)
      val weakIds = if (transactions.isEmpty) None else Some(transactions.map(_.weakId))
      InputBlockAnnouncement(1.toByte, genChain(1, history).last.header, fields, weakIds)
    }

    def processedTip(transactions: Seq[ErgoTransaction] = Seq.empty): InputBlockAnnouncement = {
      val tip = announcement(transactions = transactions)
      history.applyInputBlock(tip) shouldBe None
      history.applyInputBlockTransactions(tip.id, transactions, state)._1 shouldBe Seq(tip.id)
      history.bestBlocks._2.map(_.id) shouldBe Some(tip.id)
      history.bestInputBlocksChain() shouldBe Seq(tip.id)
      tip
    }

    def markOutdated(peer: ConnectedPeer): Unit = {
      val old = tracker.statuses(peer)
      tracker.statuses.update(peer, old.copy(lastSyncSentTime = Some(System.currentTimeMillis() - 61000L)))
    }

    def messages(): Seq[SendToNetwork] = nc.receiveWhile(300.millis, 30.millis) { case m => m }
      .collect { case m: SendToNetwork => m }

    def announcements(messages: Seq[SendToNetwork]): Seq[(InputBlockAnnouncement, SendToNetwork)] =
      messages.collect {
        case m @ SendToNetwork(Message(spec, Right(a: InputBlockAnnouncement), _), _) =>
          spec shouldBe InputBlockMessageSpec
          a -> m
      }

    def requireSync(messages: Seq[SendToNetwork]): Unit =
      messages.exists {
        case SendToNetwork(Message(_, Right(_: ErgoSyncInfo), _), _) => true
        case _ => false
      } shouldBe true

    def requireReplay(messages: Seq[SendToNetwork], tip: InputBlockAnnouncement, remote: ConnectedPeer): Unit = {
      val replays = announcements(messages)
      replays should have size 1
      InputBlockAnnouncement.serializer.toBytes(replays.head._1) shouldBe
        InputBlockAnnouncement.serializer.toBytes(tip)
      replays.head._2.sendingStrategy shouldBe SendToPeer(remote)
    }
  }

  private def withFixture(test: Fixture => Unit): Unit = withFixtureAtHeight(4)(test)

  private def withFixtureAtHeight(height: Int)(test: Fixture => Unit): Unit = {
    val fixture = new Fixture(height)
    try {
      fixture.initialize()
      test(fixture)
    } finally {
      try Await.result(fixture.system.terminate(), 10.seconds)
      finally {
        try {
          if (fixture.history != null) fixture.history.closeStorage()
        } finally {
          if (fixture.state != null) fixture.state.closeStorage()
        }
      }
    }
  }

  property("the first Unknown-to-Equal V2 response after a local sync send replays the processed tip") {
    withFixture { f =>
      val tip = f.processedTip()
      val peer = f.peer()
      f.tracker.updateStatus(peer, Unknown, None)

      f.sync.outbound(f.history)
      val outbound = f.messages()
      f.requireSync(outbound)
      f.announcements(outbound) shouldBe empty
      f.tracker.notSyncedOrOutdated(peer) shouldBe false

      f.sync.inboundV2(f.history, peer)
      f.tracker.getStatus(peer) shouldBe Some(Equal)
      f.tracker.fullInfo().find(_.peer == peer).map(_.height) shouldBe Some(f.history.fullBlockHeight)
      val response = f.messages()
      f.requireSync(response)
      f.requireReplay(response, tip, peer)
    }
  }

  property("periodic sync sends no input-tip announcement to an eligible outdated peer") {
    withFixture { f =>
      f.processedTip()
      val peer = f.peer()
      f.tracker.updateStatus(peer, Equal, Some(f.history.fullBlockHeight))
      f.markOutdated(peer)
      f.sync.outbound(f.history)
      val messages = f.messages()
      f.requireSync(messages)
      f.announcements(messages) shouldBe empty
    }
  }

  property("V1 sends no input-tip announcement even after V2 established the peer height") {
    withFixture { f =>
      val tip = f.processedTip()
      val peer = f.peer()
      f.sync.inboundV2(f.history, peer)
      f.requireReplay(f.messages(), tip, peer)
      f.markOutdated(peer)

      f.sync.inboundV1(f.history, peer)
      f.tracker.getStatus(peer) shouldBe Some(Equal)
      f.tracker.fullInfo().find(_.peer == peer).map(_.height) shouldBe Some(f.history.fullBlockHeight)
      val messages = f.messages()
      f.requireSync(messages)
      f.announcements(messages) shouldBe empty
    }
  }

  property("an already Equal peer with a recent V2 response gets no unsolicited replay") {
    withFixture { f =>
      val tip = f.processedTip()
      val peer = f.peer()
      f.sync.inboundV2(f.history, peer)
      f.requireReplay(f.messages(), tip, peer)

      f.sync.inboundV2(f.history, peer)
      f.messages() shouldBe empty
    }
  }

  property("an expired sync timestamp does not replay the processed tip on the same connection") {
    withFixture { f =>
      val tip = f.processedTip()
      val peer = f.peer()
      f.sync.inboundV2(f.history, peer)
      f.requireReplay(f.messages(), tip, peer)

      f.markOutdated(peer)
      f.sync.inboundV2(f.history, peer)
      val response = f.messages()
      f.requireSync(response)
      f.announcements(response) shouldBe empty
    }
  }

  property("disconnect then reconnect at the same address permits one new replay") {
    withFixture { f =>
      val tip = f.processedTip()
      val firstConnection = f.peer()
      f.sync.handshaked(firstConnection)
      f.sync.inboundV2(f.history, firstConnection)
      f.requireReplay(f.messages(), tip, firstConnection)
      f.sync.inputTipReplayConnectionCount shouldBe 1

      f.sync.disconnected(firstConnection)
      f.sync.inputTipReplayConnectionCount shouldBe 0
      val nextConnection = firstConnection.copy(handlerRef = TestProbe()(f.system).ref)
      nextConnection shouldBe firstConnection // ConnectedPeer equality uses the remote address.
      f.sync.handshaked(nextConnection)
      f.sync.inboundV2(f.history, nextConnection)
      f.requireReplay(f.messages(), tip, nextConnection)
      f.sync.inputTipReplayConnectionCount shouldBe 1
    }
  }

  property("terminating a replayed peer handler clears it without a disconnect event") {
    withFixture { f =>
      val tip = f.processedTip()
      val peer = f.peer()
      f.sync.inboundV2(f.history, peer)
      f.requireReplay(f.messages(), tip, peer)
      f.sync.inputTipReplayConnectionCount shouldBe 1

      f.system.stop(peer.handlerRef)
      f.nc.awaitAssert {
        f.sync.inputTipReplayConnectionCount shouldBe 0
      }
    }
  }

  property("a pending child does not replace the processed prefix in a V2 reconnect reply") {
    withFixture { f =>
      val tip = f.processedTip()
      val child = f.announcement(Some(tip))
      f.history.applyInputBlock(child) shouldBe None
      f.history.bestBlocks._2.map(_.id) shouldBe Some(child.id)
      f.history.bestInputBlocksChain() shouldBe Seq(tip.id)

      val peer = f.peer()
      f.sync.inboundV2(f.history, peer)
      f.requireReplay(f.messages(), tip, peer)
    }
  }

  property("an empty processed input chain preserves the V2 sync response without replay") {
    withFixture { f =>
      val unprocessed = f.announcement()
      f.history.applyInputBlock(unprocessed) shouldBe None
      f.history.bestInputBlocksChain() shouldBe empty

      val peer = f.peer()
      f.sync.inboundV2(f.history, peer)
      val messages = f.messages()
      f.requireSync(messages)
      f.announcements(messages) shouldBe empty

      f.history.applyInputBlockTransactions(unprocessed.id, Seq.empty, f.state)._1 shouldBe
        Seq(unprocessed.id)
      f.history.bestInputBlocksChain() shouldBe Seq(unprocessed.id)
      val tip = unprocessed
      f.markOutdated(peer)
      f.sync.inboundV2(f.history, peer)
      val laterResponse = f.messages()
      f.requireSync(laterResponse)
      f.requireReplay(laterResponse, tip, peer)
    }
  }

  for (count <- Seq(3, 4)) {
    property(s"V2 reconnect replay preserves the weak-ID payload rule for $count processed transactions") {
      withFixture { f =>
        val transactions = f.transactions(count)
        val tip = f.processedTip(transactions)
        tip.weakTxIds.map(_.size) shouldBe Some(count)

        val peer = f.peer()
        f.sync.inboundV2(f.history, peer)
        val expected = if (count <= 3) tip else tip.copy(weakTxIds = None)
        f.requireReplay(f.messages(), expected, peer)
        f.history.getInputBlock(tip.id).get.weakTxIds.map(_.map(_.toSeq)) shouldBe
          tip.weakTxIds.map(_.map(_.toSeq))
      }
    }
  }

  for ((label, version, mode) <- Seq(
    ("older protocol", Version(6, 4, 0), Some(StateType.Utxo)),
    ("digest mode", Version.SubblocksVersion, Some(StateType.Digest)),
    ("absent mode", Version.SubblocksVersion, None))) {
    property(s"an Equal V2 response excludes $label from input-tip replay") {
      withFixture { f =>
        f.processedTip()
        val peer = f.peer(version, mode)
        f.sync.inboundV2(f.history, peer)
        f.tracker.getStatus(peer) shouldBe Some(Equal)
        val messages = f.messages()
        f.requireSync(messages)
        f.announcements(messages) shouldBe empty
      }
    }
  }

  property("V2 responses from Younger, Older, and Fork peers do not replay an input tip") {
    withFixture { f =>
      f.processedTip()
      val youngerInfo = ErgoSyncInfoV2(Seq(f.orderingChain.dropRight(1).last.header))
      val olderInfo = ErgoSyncInfoV2(Seq(
        genChain(height = 1, history = f.history, stateOpt = Some(f.state)).last.header))
      val common = f.orderingChain.dropRight(1).last
      val localTip = f.orderingChain.last.header
      val forkTip = Iterator.continually(genChain(1, common).last.header)
        .find(_.id != localTip.id).get
      val forkInfo = ErgoSyncInfoV2(Seq(forkTip, common.header))

      Seq(
        (Younger, youngerInfo),
        (Older, olderInfo),
        (Fork, forkInfo)
      ).foreach { case (expectedStatus, info) =>
        f.history.compare(info) shouldBe expectedStatus
        val peer = f.peer()
        f.sync.inboundV2(f.history, info, peer)
        f.tracker.getStatus(peer) shouldBe Some(expectedStatus)
        f.announcements(f.messages()) shouldBe empty
      }
    }
  }

  property("Equal header height ahead of local full-block height does not qualify for replay") {
    withFixture { f =>
      f.processedTip()
      val next = genChain(height = 1, history = f.history, stateOpt = Some(f.state)).last
      f.history = f.history.append(next.header).get._1
      f.history.headersHeight shouldBe f.history.fullBlockHeight + 1
      val peer = f.peer()

      f.sync.inboundV2(f.history, peer)
      f.tracker.getStatus(peer) shouldBe Some(Equal)
      f.tracker.fullInfo().find(_.peer == peer).map(_.height) shouldBe Some(f.history.headersHeight)
      val messages = f.messages()
      f.requireSync(messages)
      f.announcements(messages) shouldBe empty
    }
  }

  property("zero local full-block height cannot qualify for V2 replay") {
    withFixtureAtHeight(0) { f =>
      f.history.bestInputBlocksChain() shouldBe empty
      val peer = f.peer()
      f.sync.inboundV2(f.history, peer)
      f.tracker.getStatus(peer) shouldBe Some(Equal)
      f.messages() shouldBe empty
    }
  }
}
