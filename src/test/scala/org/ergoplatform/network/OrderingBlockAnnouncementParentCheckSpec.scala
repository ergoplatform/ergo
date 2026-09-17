package org.ergoplatform.network

import akka.actor.{ActorRef, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.AutolykosSolution
import org.ergoplatform.consensus.{Equal, Older}
import org.ergoplatform.mining.{AutolykosPowScheme, InputBlockFields}
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.{ErgoFullBlock, OrderingBlockAnnouncementTypeId}
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.network.message.inputblocks.{OrderingBlockAnnouncement, OrderingBlockAnnouncementMessageSpec}
import org.ergoplatform.network.message.{InvData, InvSpec, Message, ModifiersData, ModifiersSpec, RequestModifierSpec}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryUtils, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.ErgoSettings
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
  * An ordering block announcement is stored, relayed and handed to the node view holder only if its parent
  * header is known, is in the best header chain, and is exactly one block below the announced header; the
  * expected difficulty is derived from that parent (at genesis height, with an empty header chain, the configured
  * initial difficulty is used). If the parent header is unknown, it is requested from the sender only, once: the
  * request expires after the delivery timeout without asking another peer or penalizing anyone.
  *
  * The synchronizer under test validates proof-of-work with the real Autolykos scheme (the default test
  * configuration uses a fake scheme that accepts any header), while the local history is built with the
  * test configuration as usual.
  */
class OrderingBlockAnnouncementParentCheckSpec extends AnyPropSpec with Matchers with FileUtils {
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
    * @param requestTimeout       delivery timeout of the synchronizer under test
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
    // a short delivery timeout by default, so that what happens after it can be observed
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

    // a peer supporting sub-blocks, within the relay height window
    val subBlockPeer: ConnectedPeer = ConnectedPeer(connectionIdGen.sample.get, TestProbe("SubBlockPeer").ref,
      Some(PeerInfo(PeerSpec(synchronizerSettings.scorexSettings.network.agentName, Version.SubblocksVersion,
        synchronizerSettings.scorexSettings.network.nodeName, None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))), System.currentTimeMillis())))
    syncTracker.updateStatus(subBlockPeer, Equal, Some(hist.fullBlockHeight + 1))

    val state: WrappedUtxoState =
      boxesHolderGen.map(WrappedUtxoState(_, createTempDir, parameters, historySettings)).sample.get
    synchronizer ! ChangedState(state)
    synchronizer ! ChangedHistory(hist)
    synchronizer ! ChangedMempool(ErgoMemPool.empty(historySettings))
    Thread.sleep(300)
    viewHolderProbe.receiveWhile(max = 300.millis, idle = 100.millis) { case m => m }
    ncProbe.receiveWhile(max = 300.millis, idle = 100.millis) { case m => m }

    def send(oba: OrderingBlockAnnouncement): Unit =
      synchronizer ! Message(OrderingBlockAnnouncementMessageSpec,
        Left(OrderingBlockAnnouncementMessageSpec.toBytes(oba)), Some(peer))

    /** Adds another peer that headers could be asked from. */
    def addOlderPeer(): ConnectedPeer = {
      val otherPeer = ConnectedPeer(connectionIdGen.sample.get, TestProbe("OtherPeer").ref,
        Some(PeerInfo(defaultPeerSpec.copy(features = Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))),
          System.currentTimeMillis())))
      syncTracker.updateStatus(otherPeer, Older, Some(hist.fullBlockHeight + 10))
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
    * Builds an ordering block announcement at `height` naming `parentId`, with a version 2 header declaring
    * `nBits` and extension fields consistent with the header's extension root. The nonce is left at zero.
    */
  private def announcement(f: Fixture, parentId: ModifierId, height: Int, nBits: Long): OrderingBlockAnnouncement = {
    import scorex.crypto.hash.Digest32

    val digest = Digest32 @@ Array.fill(32)(0.toByte)
    val ibExtension = InputBlockFields.toExtensionFields(None, digest, digest)
    val block = nextBlock(f.hist.bestFullBlockOpt, f.chain.head.blockTransactions.txs, defaultExtension ++ ibExtension)
    val sol = block.header.powSolution
    val header = block.header.copy(
      version = Header.Interpreter60Version,
      parentId = parentId,
      height = height,
      nBits = nBits,
      powSolution = new AutolykosSolution(sol.pk, sol.w, Array.fill(8)(0: Byte), sol.d)
    )
    OrderingBlockAnnouncement(OrderingBlockAnnouncement.CurrentVersion, header, Seq.empty, Seq.empty,
      block.extension.fields)
  }

  private def requiredNBitsAfter(f: Fixture, parent: Header): Long =
    DifficultySerializer.encodeCompactBits(f.hist.requiredDifficultyAfter(parent))

  private case class Outcome(stored: Boolean, relayed: Boolean, handedOff: Boolean, penalized: Boolean,
                             headerRequests: Seq[(Seq[ModifierId], Any)])

  private def outcome(f: Fixture, oba: OrderingBlockAnnouncement): Outcome = {
    val net = f.ncProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }
    val vh = f.viewHolderProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }
    def inv(s: SendToNetwork) = s.message.data.get.asInstanceOf[InvData]
    Outcome(
      stored = f.hist.getOrderingBlockAnnouncement(oba.header.id).isDefined,
      relayed = net.exists {
        case s: SendToNetwork => s.message.spec.messageCode == InvSpec.messageCode &&
          inv(s).typeId == OrderingBlockAnnouncementTypeId.value && inv(s).ids.contains(oba.header.id)
        case _ => false
      },
      handedOff = vh.exists {
        case ProcessOrderingBlock(o) => o.header.id == oba.header.id
        case _ => false
      },
      penalized = net.exists(_.isInstanceOf[PenalizePeer]),
      headerRequests = net.collect {
        case s: SendToNetwork if s.message.spec.messageCode == RequestModifierSpec.messageCode &&
          inv(s).typeId == Header.modifierTypeId => inv(s).ids -> s.sendingStrategy
      }
    )
  }

  property("ordering block announcement with unknown parent header is not stored, relayed or processed; parent header is requested from the sender") {
    withFixture { f =>
      val unknownParent = bytesToId(Array.fill(32)(0x5a.toByte))
      val oba = announcement(f, unknownParent, f.hist.fullBlockHeight + 1, DifficultySerializer.encodeCompactBits(1))

      // the announced header on its own passes the proof-of-work and extension checks
      oba.valid(f.realPowScheme, None) shouldBe true

      f.send(oba)

      outcome(f, oba) shouldBe Outcome(stored = false, relayed = false, handedOff = false, penalized = false,
        headerRequests = Seq(Seq(unknownParent) -> SendToPeer(f.peer)))
      // the request is tracked, so the header is accepted when it arrives
      f.deliveryTracker.status(unknownParent, Header.modifierTypeId, Seq.empty) shouldBe ModifiersStatus.Requested
    }
  }

  property("unknown parent header: the header delivered by the sender is accepted") {
    withFixture { f =>
      f.addOlderPeer()
      val tip = f.hist.bestFullBlockOpt.get
      // a real header that is not in local history: a sibling of the best full block
      val sibling = nextBlock(Some(f.chain(1)), tip.blockTransactions.txs, defaultExtension).header
      val oba = announcement(f, sibling.id, tip.header.height + 1, DifficultySerializer.encodeCompactBits(1))
      f.send(oba)
      outcome(f, oba).headerRequests shouldBe Seq(Seq(sibling.id) -> SendToPeer(f.peer))

      f.deliverHeader(sibling)

      f.ncProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }
        .exists(_.isInstanceOf[PenalizePeer]) shouldBe false
      f.viewHolderProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }.exists {
        case ModifiersFromRemote(mods) => mods.exists(_.id == sibling.id)
        case _ => false
      } shouldBe true
    }
  }

  property("unknown-parent request expires without requesting the header from another peer or penalizing anyone") {
    withFixture { f =>
      val otherPeer = f.addOlderPeer()
      val unknownParent = bytesToId(Array.fill(32)(0x5a.toByte))
      val oba = announcement(f, unknownParent, f.hist.fullBlockHeight + 1, DifficultySerializer.encodeCompactBits(1))

      f.send(oba)

      // observe two delivery deadlines, including a potential retry against the other peer
      val messages = f.ncProbe.receiveWhile(max = 5.seconds, idle = 5.seconds) { case m => m }
      val requests = messages.collect {
        case s: SendToNetwork if s.message.spec.messageCode == RequestModifierSpec.messageCode &&
          s.message.data.get.asInstanceOf[InvData].ids.contains(unknownParent) => s.sendingStrategy
      }
      requests shouldBe Seq(SendToPeer(f.peer))
      messages.exists {
        case p: PenalizePeer => p.address == otherPeer.connectionId.remoteAddress
        case _ => false
      } shouldBe false
      messages.exists(_.isInstanceOf[PenalizePeer]) shouldBe false
      f.deliveryTracker.getRequestedInfo(Header.modifierTypeId, unknownParent) shouldBe None
      f.deliveryTracker.status(unknownParent, Header.modifierTypeId, Seq.empty) shouldBe ModifiersStatus.Unknown
      f.hist.getOrderingBlockAnnouncement(oba.header.id) shouldBe None
    }
  }

  property("an expiration from an earlier request attempt does not clear the current request for the header") {
    withFixture(new Fixture(requestTimeout = 30.seconds)) { f =>
      f.addOlderPeer()
      val tip = f.hist.bestFullBlockOpt.get
      val sibling = nextBlock(Some(f.chain(1)), tip.blockTransactions.txs, defaultExtension).header
      val oba = announcement(f, sibling.id, tip.header.height + 1, DifficultySerializer.encodeCompactBits(1))
      f.send(oba)
      outcome(f, oba).headerRequests shouldBe Seq(Seq(sibling.id) -> SendToPeer(f.peer))
      val first = f.deliveryTracker.getRequestedInfo(Header.modifierTypeId, sibling.id).get

      // the first request is cleared (as when its expiration is already queued) and the header is requested again
      f.deliveryTracker.setUnknown(sibling.id, Header.modifierTypeId)
      f.send(oba)
      outcome(f, oba).headerRequests shouldBe Seq(Seq(sibling.id) -> SendToPeer(f.peer))
      val current = f.deliveryTracker.getRequestedInfo(Header.modifierTypeId, sibling.id).get
      current should not be theSameInstanceAs(first)

      // the first attempt's expiration arrives now
      val stale = new SenderOnlyRequestExpired(Header.modifierTypeId, sibling.id)
      stale.timer = first.cancellable
      f.synchronizer ! stale

      outcome(f, oba).headerRequests shouldBe empty
      f.deliveryTracker.getRequestedInfo(Header.modifierTypeId, sibling.id).get should be theSameInstanceAs current
      f.deliveryTracker.status(sibling.id, Header.modifierTypeId, Seq.empty) shouldBe ModifiersStatus.Requested

      f.deliverHeader(sibling)

      f.ncProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }
        .exists(_.isInstanceOf[PenalizePeer]) shouldBe false
      f.viewHolderProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }.exists {
        case ModifiersFromRemote(mods) => mods.exists(_.id == sibling.id)
        case _ => false
      } shouldBe true
    }
  }

  property("ordering block announcement whose known parent is an old header is not stored, relayed or processed") {
    withFixture { f =>
      val oldParent = f.chain.head.header
      val oba = announcement(f, oldParent.id, f.hist.fullBlockHeight + 1, requiredNBitsAfter(f, oldParent))
      oba.valid(f.realPowScheme, Some(requiredNBitsAfter(f, oldParent))) shouldBe true

      f.send(oba)

      outcome(f, oba) shouldBe Outcome(stored = false, relayed = false, handedOff = false, penalized = false,
        headerRequests = Seq.empty)
    }
  }

  property("ordering block announcement whose parent is a known header outside the best chain is not stored, relayed or processed") {
    withFixture { f =>
      val tip = f.hist.bestFullBlockOpt.get
      val forkBlock = nextBlock(Some(f.chain(1)), tip.blockTransactions.txs, defaultExtension)
      f.hist.append(forkBlock.header).get
      forkBlock.header.height shouldBe tip.header.height
      f.hist.isInBestChain(forkBlock.header) shouldBe false

      val oba = announcement(f, forkBlock.header.id, tip.header.height + 1, requiredNBitsAfter(f, forkBlock.header))
      oba.valid(f.realPowScheme, Some(requiredNBitsAfter(f, forkBlock.header))) shouldBe true

      f.send(oba)

      outcome(f, oba) shouldBe Outcome(stored = false, relayed = false, handedOff = false, penalized = false,
        headerRequests = Seq.empty)
    }
  }

  property("ordering block announcement whose height is not parent height + 1 is not stored, relayed or processed") {
    withFixture { f =>
      val parent = f.chain(1).header
      f.hist.isInBestChain(parent) shouldBe true
      val oba = announcement(f, parent.id, parent.height + 2, requiredNBitsAfter(f, parent))

      f.send(oba)

      outcome(f, oba) shouldBe Outcome(stored = false, relayed = false, handedOff = false, penalized = false,
        headerRequests = Seq.empty)
    }
  }

  property("ordering block announcement extending the best chain with the required difficulty is stored, relayed and processed") {
    withFixture { f =>
      val tip = f.hist.bestFullBlockOpt.get.header
      val oba = announcement(f, tip.id, tip.height + 1, requiredNBitsAfter(f, tip))

      f.send(oba)

      outcome(f, oba) shouldBe Outcome(stored = true, relayed = true, handedOff = true, penalized = false,
        headerRequests = Seq.empty)
    }
  }

  property("ordering block announcement competing with the best full block (parent in best chain) is still accepted") {
    withFixture { f =>
      val parent = f.chain(1).header
      val oba = announcement(f, parent.id, parent.height + 1, requiredNBitsAfter(f, parent))

      f.send(oba)

      outcome(f, oba) shouldBe Outcome(stored = true, relayed = true, handedOff = true, penalized = false,
        headerRequests = Seq.empty)
    }
  }

  property("ordering block announcement extending the best chain with a different difficulty is rejected and penalized") {
    withFixture { f =>
      val tip = f.hist.bestFullBlockOpt.get.header
      val oba = announcement(f, tip.id, tip.height + 1,
        DifficultySerializer.encodeCompactBits(f.hist.requiredDifficultyAfter(tip) * 2))

      f.send(oba)

      outcome(f, oba) shouldBe Outcome(stored = false, relayed = false, handedOff = false, penalized = true,
        headerRequests = Seq.empty)
    }
  }

  property("ordering block announcement extending the best chain with the required difficulty but insufficient work is rejected and penalized") {
    withFixture(new Fixture(Some(highDifficultyHex))) { f =>
      val tip = f.hist.bestFullBlockOpt.get.header
      val required = requiredNBitsAfter(f, tip)
      required shouldBe tip.nBits
      val oba = announcement(f, tip.id, tip.height + 1, required)
      f.realPowScheme.validate(oba.header).isSuccess shouldBe false
      oba.valid(f.realPowScheme, Some(required)) shouldBe false

      f.send(oba)

      outcome(f, oba) shouldBe Outcome(stored = false, relayed = false, handedOff = false, penalized = true,
        headerRequests = Seq.empty)
    }
  }

  property("ordering block announcement whose known parent is an old header is not penalized, even with insufficient work") {
    withFixture(new Fixture(Some(highDifficultyHex))) { f =>
      val oldParent = f.chain.head.header
      val oba = announcement(f, oldParent.id, f.hist.fullBlockHeight + 1, requiredNBitsAfter(f, oldParent))
      f.realPowScheme.validate(oba.header).isSuccess shouldBe false

      f.send(oba)

      outcome(f, oba) shouldBe Outcome(stored = false, relayed = false, handedOff = false, penalized = false,
        headerRequests = Seq.empty)
    }
  }

  property("ordering block announcement at genesis height with an empty header chain is checked against the configured initial difficulty") {
    withFixture(new Fixture(applyLocalChain = false)) { f =>
      f.hist.bestHeaderOpt shouldBe None
      val initial = f.historySettings.chainSettings.initialNBits
      val oba = announcement(f, Header.GenesisParentId, ErgoHistoryUtils.GenesisHeight, initial)

      f.send(oba)

      val o = outcome(f, oba)
      (o.stored, o.relayed, o.penalized) shouldBe ((true, true, false))
    }
  }

  property("ordering block announcement at genesis height with a different difficulty is rejected and penalized") {
    withFixture(new Fixture(Some(highDifficultyHex), applyLocalChain = false)) { f =>
      // configured initial difficulty 2^80; the announcement declares difficulty 1
      val other = DifficultySerializer.encodeCompactBits(1)
      other should not be f.historySettings.chainSettings.initialNBits
      val oba = announcement(f, Header.GenesisParentId, ErgoHistoryUtils.GenesisHeight, other)
      // on its own, the header meets the difficulty it declares
      oba.valid(f.realPowScheme, None) shouldBe true

      f.send(oba)

      outcome(f, oba) shouldBe Outcome(stored = false, relayed = false, handedOff = false, penalized = true,
        headerRequests = Seq.empty)
    }
  }
}
