package org.ergoplatform.network.llm_generated

import akka.actor.{ActorRef, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.consensus.{Equal, Nonsense, PeerChainStatus, Unknown}
import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.{ErgoNodeViewSynchronizer, ErgoSyncTracker, ModePeerFeature, Version}
import org.ergoplatform.network.message.Message
import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.state.{BoxHolder, StateType, UtxoState}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.{ConnectedPeer, DeliveryTracker, SendToPeer}
import scorex.testkit.utils.AkkaFixture
import scorex.crypto.authds.LeafData
import scorex.util.{bytesToId, idToBytes}
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.data.TrivialProp.TrueProp
import sigma.interpreter.ProverResult

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

/** Replay is exercised through ordinary sync entry points, without locally mined block events. */
class MatrixInputTipSyncSpec extends AnyPropSpec with Matchers with FileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ChainGenerator._

  private class Synchronizer(nc: ActorRef, vh: ActorRef, cfg: ErgoSettings,
                             val statuses: ErgoSyncTracker)(implicit ec: ExecutionContext)
    extends ErgoNodeViewSynchronizer(nc, vh, ErgoSyncInfoMessageSpec, cfg,
      statuses, DeliveryTracker.empty(cfg)) {
    def outbound(history: ErgoHistory): Unit = sendSync(history)
    def inboundV2(history: ErgoHistory, peer: ConnectedPeer): Unit =
      processSyncV2(history, history.syncInfoV2(full = true), peer)
    def inboundV1(history: ErgoHistory, peer: ConnectedPeer): Unit =
      processSyncV1(history, history.syncInfoV1, peer)
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
    val tracker: ErgoSyncTracker = ErgoSyncTracker(cfg.scorexSettings.network)
    var sync: Synchronizer = _

    def initialize(): Unit = {
      state = UtxoState.fromBoxHolder(BoxHolder(boxes), None, createTempDir, cfg, parameters)
      history = ErgoHistory.readOrGenerate(cfg)(null)
      applyChain(history, genChain(height = initialHeight, history = history, stateOpt = Some(state)))
      history.fullBlockHeight shouldBe initialHeight
      val ref: TestActorRef[Synchronizer] = TestActorRef(Props(new Synchronizer(nc.ref, vh.ref, cfg, tracker)))
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

    def eligible(peer: ConnectedPeer, offset: Int = 0, status: PeerChainStatus = Equal): Unit = {
      tracker.updateStatus(peer, status, Some(history.fullBlockHeight + offset))
      // A normal periodic sync opportunity, without wall-clock sleeping.
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
      InputBlockAnnouncement.serializer.toBytes(replays.head._1) shouldBe InputBlockAnnouncement.serializer.toBytes(tip)
      replays.head._2.sendingStrategy shouldBe SendToPeer(remote)
    }
  }

  private def withFixture(test: Fixture => Unit): Unit = withFixtureAtHeight(4)(test)

  private def withFixtureAtHeight(height: Int)(test: Fixture => Unit): Unit = {
    val fixture = new Fixture(height)
    try {
      fixture.initialize()
      test(fixture)
    }
    finally {
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

  property("an Unknown peer learns the equal V2 height and receives the already processed tip") {
    withFixture { f =>
      val tip = f.processedTip()
      val peer = f.peer()
      f.tracker.updateStatus(peer, Unknown, None)
      f.sync.inboundV2(f.history, peer)
      f.tracker.getStatus(peer) shouldBe Some(Equal)
      f.tracker.fullInfo().find(_.peer == peer).map(_.height) shouldBe Some(f.history.fullBlockHeight)
      val messages = f.messages()
      f.requireSync(messages)
      f.requireReplay(messages, tip, peer)
    }
  }

  property("outbound periodic sync replays the tip even though it refreshes the send timestamp") {
    withFixture { f =>
      val tip = f.processedTip()
      val peer = f.peer()
      f.eligible(peer)
      f.sync.outbound(f.history)
      f.tracker.notSyncedOrOutdated(peer) shouldBe false
      val messages = f.messages()
      f.requireSync(messages)
      f.requireReplay(messages, tip, peer)
      // Equal simultaneous sync responses need no reply; discovery must already have happened above.
      f.sync.inboundV2(f.history, peer)
      f.messages() shouldBe empty
    }
  }

  property("an announced but unprocessed child does not replace the replayed processed tip") {
    withFixture { f =>
      val tip = f.processedTip()
      val child = f.announcement(Some(tip))
      f.history.applyInputBlock(child) shouldBe None
      f.history.getInputBlock(child.id).isDefined shouldBe true
      f.history.bestBlocks._2.map(_.id) shouldBe Some(child.id)
      f.history.bestInputBlocksChain() shouldBe Seq(tip.id)
      val peer = f.peer()
      f.eligible(peer)
      f.sync.outbound(f.history)
      f.requireReplay(f.messages(), tip, peer)
    }
  }

  property("an empty processed input chain preserves normal sync without an announcement") {
    withFixture { f =>
      val unprocessed = f.announcement()
      f.history.applyInputBlock(unprocessed)
      f.history.bestBlocks._2 shouldBe None
      f.history.bestInputBlocksChain() shouldBe empty
      val peer = f.peer()
      f.eligible(peer)
      f.sync.outbound(f.history)
      val messages = f.messages()
      f.requireSync(messages)
      f.announcements(messages) shouldBe empty
    }
  }

  property("V1 can reuse an established V2 height for a sync response replay") {
    withFixture { f =>
      val tip = f.processedTip()
      val peer = f.peer()
      f.eligible(peer)
      f.sync.inboundV1(f.history, peer)
      val messages = f.messages()
      f.requireSync(messages)
      f.requireReplay(messages, tip, peer)
    }
  }

  property("fresh V1 sync without an established height does not replay an input tip") {
    withFixture { f =>
      f.processedTip()
      val peer = f.peer()
      f.sync.inboundV1(f.history, peer)
      val messages = f.messages()
      f.requireSync(messages)
      f.announcements(messages) shouldBe empty
    }
  }

  for (offset <- Seq(-3, -2, 2, 3)) {
    property(s"outbound tip replay observes the two-block height window at offset $offset") {
      withFixture { f =>
        val tip = f.processedTip()
        val peer = f.peer()
        f.eligible(peer, offset)
        f.sync.outbound(f.history)
        val messages = f.messages()
        f.requireSync(messages)
        if (math.abs(offset) <= 2) f.requireReplay(messages, tip, peer)
        else f.announcements(messages) shouldBe empty
      }
    }
  }

  for ((label, version, mode) <- Seq(
    ("older version", Version(6, 4, 0), Some(StateType.Utxo)),
    ("digest mode", Version.SubblocksVersion, Some(StateType.Digest)),
    ("absent mode", Version.SubblocksVersion, None))) {
    property(s"outbound ordinary sync does not send an input tip to $label") {
      withFixture { f =>
        f.processedTip()
        val peer = f.peer(version, mode)
        f.eligible(peer)
        f.sync.outbound(f.history)
        val messages = f.messages()
        f.requireSync(messages)
        f.announcements(messages) shouldBe empty
      }
    }
  }

  for (status <- Seq(Unknown, Nonsense)) {
    property(s"outbound ordinary sync does not replay a tip for unusable status $status") {
      withFixture { f =>
        f.processedTip()
        val peer = f.peer()
        f.eligible(peer, status = status)
        f.sync.outbound(f.history)
        val messages = f.messages()
        f.requireSync(messages)
        f.announcements(messages) shouldBe empty
      }
    }
  }

  for (count <- Seq(3, 4)) {
    property(s"sync replay preserves the existing weak-ID payload rule for $count processed transactions") {
      withFixture { f =>
        val transactions = f.transactions(count)
        transactions.size shouldBe count
        val tip = f.processedTip(transactions)
        tip.weakTxIds.map(_.size) shouldBe Some(count)
        f.history.getInputBlockTransactions(tip.id).map(_.map(_.id)) shouldBe Some(transactions.map(_.id))
        val peer = f.peer()
        f.eligible(peer)
        f.sync.outbound(f.history)
        val expected = if (count <= 3) tip else tip.copy(weakTxIds = None)
        f.requireReplay(f.messages(), expected, peer)
        // Replay must not rewrite the stored announcement used by later body retrieval.
        f.history.getInputBlock(tip.id).get.weakTxIds.map(_.map(_.toSeq)) shouldBe
          tip.weakTxIds.map(_.map(_.toSeq))
      }
    }
  }

  property("zero peer height is not established eligibility even within the two-block window") {
    withFixtureAtHeight(2) { f =>
      f.processedTip()
      val peer = f.peer()
      f.eligible(peer, offset = -2)
      f.tracker.fullInfo().find(_.peer == peer).map(_.height) shouldBe Some(0)
      f.sync.outbound(f.history)
      val messages = f.messages()
      f.requireSync(messages)
      f.announcements(messages) shouldBe empty
    }
  }

  property("without a local full ordering block ordinary sync has no input tip to replay") {
    withFixtureAtHeight(0) { f =>
      f.history.bestInputBlocksChain() shouldBe empty
      val peer = f.peer()
      f.eligible(peer, offset = 1)
      f.sync.outbound(f.history)
      val messages = f.messages()
      f.requireSync(messages)
      f.announcements(messages) shouldBe empty
    }
  }

  property("outbound sync replays only to its selected eligible destinations") {
    withFixture { f =>
      val tip = f.processedTip()
      val first = f.peer()
      val second = f.peer()
      val digest = f.peer(mode = Some(StateType.Digest))
      val recentlySynced = f.peer()
      Seq(first, second, digest, recentlySynced).foreach(f.eligible(_))
      f.tracker.updateLastSyncSentTime(recentlySynced)
      f.sync.outbound(f.history)
      val messages = f.messages()
      f.requireSync(messages)
      val announcements = f.announcements(messages)
      announcements should have size 2
      announcements.map(_._2.sendingStrategy).toSet shouldBe Set(SendToPeer(first), SendToPeer(second))
      announcements.foreach { case (announcement, _) =>
        InputBlockAnnouncement.serializer.toBytes(announcement) shouldBe InputBlockAnnouncement.serializer.toBytes(tip)
      }
    }
  }

  for ((label, version, mode) <- Seq(
    ("older version", Version(6, 4, 0), Some(StateType.Utxo)),
    ("digest mode", Version.SubblocksVersion, Some(StateType.Digest)),
    ("absent mode", Version.SubblocksVersion, None)); useV2 <- Seq(false, true)) {
    property(s"V${if (useV2) 2 else 1} response retains ordinary sync but excludes $label from tip replay") {
      withFixture { f =>
        f.processedTip()
        val peer = f.peer(version, mode)
        f.eligible(peer)
        if (useV2) f.sync.inboundV2(f.history, peer) else f.sync.inboundV1(f.history, peer)
        val messages = f.messages()
        f.requireSync(messages)
        f.announcements(messages) shouldBe empty
      }
    }
  }

  for (offset <- Seq(-3, -2, 2, 3)) {
    property(s"V1 response uses the established peer height at offset $offset") {
      withFixture { f =>
        val tip = f.processedTip()
        val peer = f.peer()
        f.eligible(peer, offset)
        f.sync.inboundV1(f.history, peer)
        val messages = f.messages()
        f.requireSync(messages)
        if (math.abs(offset) <= 2) f.requireReplay(messages, tip, peer)
        else f.announcements(messages) shouldBe empty
      }
    }
  }
}
