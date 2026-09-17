package org.ergoplatform.network

import akka.actor.{ActorRef, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.AutolykosSolution
import org.ergoplatform.consensus.Older
import org.ergoplatform.mining.{AutolykosPowScheme, InputBlockFields}
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.network.message.{InvData, Message, ModifiersData, ModifiersSpec, RequestModifierSpec}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryUtils, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
import scorex.core.network.{ConnectedPeer, DeliveryTracker, ModifiersStatus, SendToPeer}
import scorex.testkit.utils.AkkaFixture
import scorex.util.{ModifierId, bytesToId}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}

/**
  * An input block announcement is processed only if its parent header is known, is in the best chain, and is
  * exactly one block below the announced header (at genesis height, with no full blocks yet, the configured
  * initial difficulty is used). The expected difficulty is derived from that parent. Otherwise the announcement is
  * not processed, and an unknown parent header is requested from the sender only, once: the request expires after
  * the delivery timeout without asking another peer or penalizing anyone.
  *
  * The synchronizer under test validates proof-of-work with the real Autolykos scheme (the default test
  * configuration uses a fake scheme that accepts any header), while the local history is built with the
  * test configuration as usual.
  */
class InputBlockParentBindingSpec extends AnyPropSpec with Matchers with FileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._

  class SynchronizerUnderTest(networkControllerRef: ActorRef,
                              viewHolderRef: ActorRef,
                              settings: ErgoSettings,
                              syncTracker: ErgoSyncTracker,
                              deliveryTracker: DeliveryTracker)(implicit ec: ExecutionContextExecutor)
    extends ErgoNodeViewSynchronizer(networkControllerRef, viewHolderRef, ErgoSyncInfoMessageSpec,
      settings, syncTracker, deliveryTracker)(ec)

  /**
    * @param initialDifficultyHex overrides the test chain's difficulty (the local chain keeps it)
    * @param applyLocalChain      whether the generated chain is applied to the local history
    */
  class Fixture(initialDifficultyHex: Option[String] = None,
                applyLocalChain: Boolean = true,
                requestTimeout: FiniteDuration = 2.seconds) extends AkkaFixture {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val historySettings: ErgoSettings = {
      val s = settings.copy(directory = createTempDir.getAbsolutePath)
      initialDifficultyHex.fold(s)(hex => s.copy(chainSettings = s.chainSettings.copy(initialDifficultyHex = hex)))
    }

    private val cs = historySettings.chainSettings
    val realPowScheme = new AutolykosPowScheme(cs.powScheme.k, cs.powScheme.n)
    // Expiration tests use a short timeout; replay tests keep unrelated deadlines outside their window.
    val synchronizerSettings: ErgoSettings =
      historySettings.copy(chainSettings = cs.copy(powScheme = realPowScheme),
        scorexSettings = historySettings.scorexSettings.copy(
          network = historySettings.scorexSettings.network.copy(deliveryTimeout = requestTimeout)))

    val ncProbe = TestProbe("NetworkControllerProbe")
    val viewHolderProbe = TestProbe("ViewHolderProbe")
    val pchProbe = TestProbe("PeerHandlerProbe")

    val syncTracker = ErgoSyncTracker(synchronizerSettings.scorexSettings.network)
    val deliveryTracker: DeliveryTracker = DeliveryTracker.empty(synchronizerSettings)

    val synchronizer: TestActorRef[SynchronizerUnderTest] = TestActorRef(Props(
      new SynchronizerUnderTest(ncProbe.ref, viewHolderProbe.ref, synchronizerSettings, syncTracker, deliveryTracker)))

    val peer: ConnectedPeer = ConnectedPeer(connectionIdGen.sample.get, pchProbe.ref,
      Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis())))

    val hist: ErgoHistory = ErgoHistory.readOrGenerate(historySettings)(null)
    val chain: Seq[ErgoFullBlock] = genChain(3, hist, nBits = historySettings.chainSettings.initialNBits)
    if (applyLocalChain) applyChain(hist, chain)

    val state: WrappedUtxoState =
      boxesHolderGen.map(WrappedUtxoState(_, createTempDir, parameters, historySettings)).sample.get
    val mempool: ErgoMemPool = ErgoMemPool.empty(historySettings)

    def process(ib: InputBlockAnnouncement): Unit =
      synchronizer.underlyingActor.processInputBlock(ib, hist, mempool, peer, Some(state))

    /** Makes the synchronizer handle scheduled and network messages, with another peer that headers could be asked from. */
    def initialize(): ConnectedPeer = {
      val otherPeer = ConnectedPeer(connectionIdGen.sample.get, TestProbe("OtherPeer").ref,
        Some(PeerInfo(defaultPeerSpec.copy(features = Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))),
          System.currentTimeMillis())))
      syncTracker.updateStatus(otherPeer, Older, Some(hist.fullBlockHeight + 10))
      synchronizer ! ChangedState(state)
      synchronizer ! ChangedHistory(hist)
      synchronizer ! ChangedMempool(mempool)
      Thread.sleep(300)
      viewHolderProbe.receiveWhile(max = 300.millis, idle = 100.millis) { case m => m }
      ncProbe.receiveWhile(max = 300.millis, idle = 100.millis) { case m => m }
      otherPeer
    }

    def deliverHeader(h: Header): Unit = synchronizer ! Message(ModifiersSpec,
      Left(ModifiersSpec.toBytes(ModifiersData(Header.modifierTypeId, Map(h.id -> HeaderSerializer.toBytes(h))))), Some(peer))
  }

  private def withFixture(test: Fixture => Any): Unit = withFixture(new Fixture)(test)

  private def withFixture(f: Fixture)(test: Fixture => Any): Unit =
    try test(f) finally Await.result(f.system.terminate(), Duration.Inf)

  // difficulty 2^80: a zero nonce does not meet the target
  private val highDifficultyHex = "01" + "00" * 10

  /**
    * Builds an input block announcement at `height` naming `parentId`, with a version 2 header declaring
    * `nBits` and a Merkle proof consistent with the header's extension root. The nonce is left at zero.
    */
  private def announcement(f: Fixture, parentId: ModifierId, height: Int, nBits: Long): InputBlockAnnouncement = {
    import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
    import scorex.crypto.hash.Digest32

    val txDigest = Digest32 @@ Array.fill(32)(0.toByte)
    val prevTxDigest = Digest32 @@ Array.fill(32)(0.toByte)
    val ibExtension = InputBlockFields.toExtensionFields(None, txDigest, prevTxDigest)
    val block = nextBlock(f.hist.bestFullBlockOpt, f.chain.head.blockTransactions.txs, defaultExtension ++ ibExtension)
    val fields = block.extension.fields
    val proof = ExtensionCandidate(fields).proofForInputBlockData.get

    val sol = block.header.powSolution
    val header = block.header.copy(
      version = Header.Interpreter60Version,
      parentId = parentId,
      height = height,
      nBits = nBits,
      powSolution = new AutolykosSolution(sol.pk, sol.w, Array.fill(8)(0: Byte), sol.d)
    )
    InputBlockAnnouncement(InputBlockAnnouncement.initialMessageVersion, header,
      new InputBlockFields(None, txDigest, prevTxDigest, proof), None)
  }

  private def requiredNBitsAfter(f: Fixture, parent: Header): Long =
    DifficultySerializer.encodeCompactBits(f.hist.requiredDifficultyAfter(parent))

  private def viewHolderGotInputBlock(f: Fixture): Boolean =
    f.viewHolderProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }
      .exists(_.isInstanceOf[ProcessInputBlock])

  private def networkMessages(f: Fixture): Seq[Any] =
    f.ncProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }

  private def headerRequests(msgs: Seq[Any]): Seq[(Seq[ModifierId], Any)] = msgs.collect {
    case SendToNetwork(msg, strategy) if msg.spec.messageCode == RequestModifierSpec.messageCode &&
      msg.data.get.asInstanceOf[InvData].typeId == Header.modifierTypeId =>
      msg.data.get.asInstanceOf[InvData].ids -> strategy
  }

  private def penalized(msgs: Seq[Any]): Boolean = msgs.exists(_.isInstanceOf[PenalizePeer])

  property("input block with unknown parent header is dropped, and the parent header is requested from the sender") {
    withFixture { f =>
      val unknownParent = bytesToId(Array.fill(32)(0x5a.toByte))
      val tip = f.hist.bestFullBlockOpt.get.header
      val ib = announcement(f, unknownParent, tip.height + 1, DifficultySerializer.encodeCompactBits(1))

      // the announced header on its own passes the proof-of-work and Merkle checks
      ib.valid(f.realPowScheme, f.state.stateContext.currentParameters, None) shouldBe true

      f.process(ib)

      viewHolderGotInputBlock(f) shouldBe false
      val msgs = networkMessages(f)
      penalized(msgs) shouldBe false
      headerRequests(msgs) shouldBe Seq(Seq(unknownParent) -> SendToPeer(f.peer))
      msgs.collect { case s: SendToNetwork => s }.size shouldBe 1
      // the request is tracked, so the header is accepted when it arrives
      f.deliveryTracker.status(unknownParent, Header.modifierTypeId, Seq.empty) shouldBe ModifiersStatus.Requested
    }
  }

  property("unknown parent header: the header delivered by the sender is accepted") {
    withFixture { f =>
      f.initialize()
      val tip = f.hist.bestFullBlockOpt.get
      // a real header that is not in local history: a sibling of the best full block
      val sibling = nextBlock(Some(f.chain(1)), tip.blockTransactions.txs, defaultExtension).header
      f.process(announcement(f, sibling.id, tip.header.height + 1, DifficultySerializer.encodeCompactBits(1)))
      headerRequests(networkMessages(f)) shouldBe Seq(Seq(sibling.id) -> SendToPeer(f.peer))

      f.deliverHeader(sibling)

      penalized(networkMessages(f)) shouldBe false
      f.viewHolderProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }.exists {
        case ModifiersFromRemote(mods) => mods.exists(_.id == sibling.id)
        case _ => false
      } shouldBe true
    }
  }

  property("input block whose known parent is an old header is dropped without requests") {
    withFixture { f =>
      val oldParent = f.chain.head.header
      val tip = f.hist.bestFullBlockOpt.get.header
      oldParent.id should not be tip.id
      val ib = announcement(f, oldParent.id, tip.height + 1, requiredNBitsAfter(f, oldParent))

      ib.valid(f.realPowScheme, f.state.stateContext.currentParameters, Some(requiredNBitsAfter(f, oldParent))) shouldBe true

      f.process(ib)

      viewHolderGotInputBlock(f) shouldBe false
      networkMessages(f) shouldBe empty
    }
  }

  property("an input announcement is accepted on replay after its parent joins the best header chain") {
    withFixture(new Fixture(requestTimeout = 30.seconds)) { f =>
      val originalTip = f.hist.bestFullBlockOpt.get
      val parent = nextBlock(Some(f.chain(1)), originalTip.blockTransactions.txs, defaultExtension)
      parent.height shouldBe originalTip.height
      f.hist.contains(parent.header) shouldBe false
      val ib = announcement(f, parent.id, originalTip.height + 1, requiredNBitsAfter(f, parent.header))
      ib.valid(f.realPowScheme, f.state.stateContext.currentParameters,
        Some(requiredNBitsAfter(f, parent.header))) shouldBe true

      f.process(ib)

      viewHolderGotInputBlock(f) shouldBe false
      val initialMessages = networkMessages(f)
      headerRequests(initialMessages) shouldBe Seq(Seq(parent.id) -> SendToPeer(f.peer))
      penalized(initialMessages) shouldBe false

      f.hist.append(parent.header).get
      f.hist.isInBestChain(parent.header) shouldBe false
      val nextHeader = nextBlock(Some(parent), originalTip.blockTransactions.txs, defaultExtension).header
      f.hist.append(nextHeader).get
      f.hist.isInBestChain(parent.header) shouldBe true
      f.hist.bestFullBlockIdOpt shouldBe Some(originalTip.id)
      ib.header.height shouldBe f.hist.fullBlockHeight + 1

      f.process(ib)

      f.viewHolderProbe.expectMsg(ProcessInputBlock(ib, f.peer))
      val replayMessages = networkMessages(f)
      penalized(replayMessages) shouldBe false
      headerRequests(replayMessages) shouldBe empty
    }
  }

  property("an input announcement is accepted on replay after its parent becomes the best full block in history") {
    withFixture(new Fixture(requestTimeout = 30.seconds)) { f =>
      val originalTip = f.hist.bestFullBlockOpt.get
      val parent = nextBlock(Some(originalTip), originalTip.blockTransactions.txs, defaultExtension)
      f.hist.contains(parent.header) shouldBe false
      val ib = announcement(f, parent.id, parent.height + 1, requiredNBitsAfter(f, parent.header))
      ib.header.height shouldBe f.hist.fullBlockHeight + 2
      ib.valid(f.realPowScheme, f.state.stateContext.currentParameters,
        Some(requiredNBitsAfter(f, parent.header))) shouldBe true

      f.process(ib)

      viewHolderGotInputBlock(f) shouldBe false
      val initialMessages = networkMessages(f)
      headerRequests(initialMessages) shouldBe Seq(Seq(parent.id) -> SendToPeer(f.peer))
      penalized(initialMessages) shouldBe false

      applyChain(f.hist, Seq(parent))
      f.hist.bestFullBlockIdOpt shouldBe Some(parent.id)
      f.hist.isInBestChain(parent.header) shouldBe true
      ib.header.height shouldBe f.hist.fullBlockHeight + 1

      f.process(ib)

      f.viewHolderProbe.expectMsg(ProcessInputBlock(ib, f.peer))
      val replayMessages = networkMessages(f)
      penalized(replayMessages) shouldBe false
      headerRequests(replayMessages) shouldBe empty
    }
  }

  property("unknown-parent discovery expires without requesting the header from another peer") {
    withFixture { f =>
      val otherPeer = f.initialize()
      val unknownParent = bytesToId(Array.fill(32)(0x5a.toByte))
      val tip = f.hist.bestFullBlockOpt.get.header
      val ib = announcement(f, unknownParent, tip.height + 1, DifficultySerializer.encodeCompactBits(1))

      f.process(ib)

      // Observe two delivery deadlines, including a potential retry against the other peer.
      val messages = f.ncProbe.receiveWhile(max = 5.seconds, idle = 5.seconds) { case m => m }
      val requests = headerRequests(messages).filter(_._1.contains(unknownParent))
      requests.map(_._2 == SendToPeer(f.peer)) shouldBe Seq(true)
      messages.exists {
        case p: PenalizePeer => p.address == otherPeer.connectionId.remoteAddress
        case _ => false
      } shouldBe false
      f.deliveryTracker.getRequestedInfo(Header.modifierTypeId, unknownParent) shouldBe None
      f.deliveryTracker.status(unknownParent, Header.modifierTypeId, Seq.empty) shouldBe ModifiersStatus.Unknown
      viewHolderGotInputBlock(f) shouldBe false
    }
  }

  property("an expiration from an earlier request attempt does not clear the current request for the header") {
    withFixture(new Fixture(requestTimeout = 30.seconds)) { f =>
      f.initialize()
      val tip = f.hist.bestFullBlockOpt.get
      val sibling = nextBlock(Some(f.chain(1)), tip.blockTransactions.txs, defaultExtension).header
      val ib = announcement(f, sibling.id, tip.header.height + 1, DifficultySerializer.encodeCompactBits(1))
      f.process(ib)
      headerRequests(networkMessages(f)) shouldBe Seq(Seq(sibling.id) -> SendToPeer(f.peer))
      val first = f.deliveryTracker.getRequestedInfo(Header.modifierTypeId, sibling.id).get

      // the first request is cleared (as when its expiration is already queued) and the header is requested again
      f.deliveryTracker.setUnknown(sibling.id, Header.modifierTypeId)
      f.process(ib)
      headerRequests(networkMessages(f)) shouldBe Seq(Seq(sibling.id) -> SendToPeer(f.peer))
      val current = f.deliveryTracker.getRequestedInfo(Header.modifierTypeId, sibling.id).get
      current should not be theSameInstanceAs(first)

      // the first attempt's expiration arrives now
      val stale = new SenderOnlyRequestExpired(Header.modifierTypeId, sibling.id)
      stale.timer = first.cancellable
      f.synchronizer ! stale

      networkMessages(f) shouldBe empty
      f.deliveryTracker.getRequestedInfo(Header.modifierTypeId, sibling.id).get should be theSameInstanceAs current
      f.deliveryTracker.status(sibling.id, Header.modifierTypeId, Seq.empty) shouldBe ModifiersStatus.Requested

      f.deliverHeader(sibling)

      penalized(networkMessages(f)) shouldBe false
      f.viewHolderProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }.exists {
        case ModifiersFromRemote(mods) => mods.exists(_.id == sibling.id)
        case _ => false
      } shouldBe true
    }
  }

  property("input block whose parent is a known header outside the best chain is dropped") {
    withFixture { f =>
      val tip = f.hist.bestFullBlockOpt.get
      val forkBlock = nextBlock(Some(f.chain(1)), tip.blockTransactions.txs, defaultExtension)
      f.hist.append(forkBlock.header).get
      forkBlock.header.height shouldBe tip.header.height
      forkBlock.header.id should not be tip.id
      f.hist.bestFullBlockIdOpt shouldBe Some(tip.id)
      f.hist.isInBestChain(forkBlock.header) shouldBe false

      val ib = announcement(f, forkBlock.header.id, tip.header.height + 1, requiredNBitsAfter(f, forkBlock.header))
      ib.valid(f.realPowScheme, f.state.stateContext.currentParameters, Some(requiredNBitsAfter(f, forkBlock.header))) shouldBe true

      f.process(ib)

      viewHolderGotInputBlock(f) shouldBe false
      networkMessages(f) shouldBe empty
    }
  }

  property("input block extending the best chain with the required difficulty is still processed") {
    withFixture { f =>
      val tip = f.hist.bestFullBlockOpt.get.header
      val ib = announcement(f, tip.id, tip.height + 1, requiredNBitsAfter(f, tip))

      f.process(ib)

      viewHolderGotInputBlock(f) shouldBe true
      penalized(networkMessages(f)) shouldBe false
    }
  }

  property("input block extending the best chain with a different difficulty is rejected and penalized") {
    withFixture { f =>
      val tip = f.hist.bestFullBlockOpt.get.header
      val required = f.hist.requiredDifficultyAfter(tip)
      val ib = announcement(f, tip.id, tip.height + 1, DifficultySerializer.encodeCompactBits(required * 2))

      f.process(ib)

      viewHolderGotInputBlock(f) shouldBe false
      penalized(networkMessages(f)) shouldBe true
    }
  }

  property("input block extending the best chain with the required difficulty but insufficient work is rejected and penalized") {
    withFixture(new Fixture(Some(highDifficultyHex))) { f =>
      val tip = f.hist.bestFullBlockOpt.get.header
      val required = requiredNBitsAfter(f, tip)
      required shouldBe tip.nBits
      val ib = announcement(f, tip.id, tip.height + 1, required)
      val params = f.state.stateContext.currentParameters
      f.realPowScheme.checkInputBlockPoW(ib.header, params) shouldBe false
      ib.valid(f.realPowScheme, params, Some(required)) shouldBe false

      f.process(ib)

      viewHolderGotInputBlock(f) shouldBe false
      val msgs = networkMessages(f)
      penalized(msgs) shouldBe true
      msgs.collect { case s: SendToNetwork => s } shouldBe empty
    }
  }

  property("input block whose known parent is an old header is dropped without a penalty, even with insufficient work") {
    withFixture(new Fixture(Some(highDifficultyHex))) { f =>
      val oldParent = f.chain.head.header
      val tip = f.hist.bestFullBlockOpt.get.header
      val ib = announcement(f, oldParent.id, tip.height + 1, requiredNBitsAfter(f, oldParent))
      f.realPowScheme.checkInputBlockPoW(ib.header, f.state.stateContext.currentParameters) shouldBe false

      f.process(ib)

      viewHolderGotInputBlock(f) shouldBe false
      networkMessages(f) shouldBe empty
    }
  }

  property("input block at genesis height with no full blocks is checked against the configured initial difficulty") {
    withFixture(new Fixture(applyLocalChain = false)) { f =>
      f.hist.bestFullBlockIdOpt shouldBe None
      val initial = f.historySettings.chainSettings.initialNBits
      val ib = announcement(f, Header.GenesisParentId, ErgoHistoryUtils.GenesisHeight, initial)

      f.process(ib)

      viewHolderGotInputBlock(f) shouldBe true
      penalized(networkMessages(f)) shouldBe false
    }
  }

  property("input block at genesis height with a different difficulty is rejected and penalized") {
    withFixture(new Fixture(Some(highDifficultyHex), applyLocalChain = false)) { f =>
      // configured initial difficulty 2^80; the announcement declares difficulty 1
      val other = DifficultySerializer.encodeCompactBits(1)
      other should not be f.historySettings.chainSettings.initialNBits
      val ib = announcement(f, Header.GenesisParentId, ErgoHistoryUtils.GenesisHeight, other)
      // on its own, the header meets the difficulty it declares
      ib.valid(f.realPowScheme, f.state.stateContext.currentParameters, None) shouldBe true

      f.process(ib)

      viewHolderGotInputBlock(f) shouldBe false
      penalized(networkMessages(f)) shouldBe true
    }
  }
}
