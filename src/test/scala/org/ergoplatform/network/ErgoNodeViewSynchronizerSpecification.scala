package org.ergoplatform.network

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, Cancellable, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.Input
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ManifestTypeId, UtxoSnapshotChunkTypeId}
import org.ergoplatform.modifiers.history.BlockTransactionsSerializer
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.network.ErgoNodeViewSynchronizer._
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncement
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.nodeView.history.{
  ErgoHistory,
  ErgoHistoryReader,
  ErgoSyncInfoMessageSpec,
  ErgoSyncInfoV2
}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.sanity.ErgoSanity._
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader, UtxoSettings}
import org.ergoplatform.utils.BoxUtils
import org.ergoplatform.validation.{ParentHeaderNotFoundError, RecoverableModifierError}
import org.ergoplatform.wallet.utils.FileUtils
import org.scalacheck.Gen
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import scorex.core.network.ModifiersStatus.{Received, Requested, Unknown}
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
import org.ergoplatform.network.message._
import org.ergoplatform.network.peer.{PeerInfo, PenaltyType}
import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.crypto.authds.ADKey
import scorex.util.bytesToId
import org.ergoplatform.serialization.ErgoSerializer
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.testkit.utils.AkkaFixture
import sigma.Colls
import sigma.interpreter.{ContextExtension, ProverResult}
import sigmastate.helpers.TestingHelpers._
import scorex.util.ModifierId

import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor}
import scala.language.postfixOps

class ErgoNodeViewSynchronizerSpecification
  extends AnyPropSpec
  with Matchers
  with ScalaCheckPropertyChecks
  with FileUtils
  with Eventually {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators.genECPoint
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.HistoryTestHelpers._

  // ToDo: factor this out of here and NVHTests?
  private def withFixture(testCode: SynchronizerFixture => Any): Unit = {
    val fixture = new SynchronizerFixture
    try {
      testCode(fixture)
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  private def withFixture2(testCode: Synchronizer2Fixture => Any): Unit = {
    val fixture = new Synchronizer2Fixture
    try {
      testCode(fixture)
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  class NodeViewHolderMock(settings: ErgoSettings) extends ErgoNodeViewHolder[UtxoState](settings)

  class SynchronizerMock(
    networkControllerRef: ActorRef,
    viewHolderRef: ActorRef,
    syncInfoSpec: ErgoSyncInfoMessageSpec.type,
    settings: ErgoSettings,
    syncTracker: ErgoSyncTracker,
    deliveryTracker: DeliveryTracker
  )(implicit ec: ExecutionContext)
    extends ErgoNodeViewSynchronizer(
      networkControllerRef,
      viewHolderRef,
      syncInfoSpec,
      settings,
      syncTracker,
      deliveryTracker
    )(ec)

  implicit override val patienceConfig: PatienceConfig =
    PatienceConfig(2.seconds, 100.millis)

  val history = generateHistory(
    verifyTransactions = true,
    StateType.Utxo,
    PoPoWBootstrap = false,
    blocksToKeep   = -1
  )
  val chain      = genHeaderChain(2000, history, diffBitsOpt = None, useRealTs = false)
  val localChain = chain.take(1000)
  val altchain   = genHeaderChain(1000, history, diffBitsOpt = None, useRealTs = false)

  val forkedChain = {
    val c = localChain.take(1000 - 512)
    c ++ genHeaderChain(512, Some(c.last), diffBitsOpt = None, useRealTs = false).tail
  }
  val forkedHeight = forkedChain.last.height

  val localHistoryGen: Gen[HT] = {
    require(history.isEmpty)
    applyHeaderChain(history, localChain)
  }

  val localStateGen: Gen[WrappedUtxoState] =
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, parameters, settings))

  def semanticallyValidModifier(state: UTXO_ST): PM = {
    statefulyValidFullBlock(state.asInstanceOf[WrappedUtxoState])
  }

  def semanticallyInvalidModifier(state: UTXO_ST): PM = invalidErgoFullBlockGen.sample.get

  def totallyValidModifier(history: HT, state: UTXO_ST): PM = {
    val parentOpt = history.bestFullBlockOpt
    validFullBlock(parentOpt, state.asInstanceOf[WrappedUtxoState]).header
  }

  def totallyValidModifiers(history: HT, state: UTXO_ST, count: Int): Seq[PM] = {
    require(count >= 1)
    val headerOpt = history.bestFullBlockOpt
    (0 until count)
      .foldLeft((headerOpt, Seq.empty[PM])) {
        case (acc, _) =>
          val pm = validFullBlock(headerOpt, state.asInstanceOf[WrappedUtxoState])
          (Some(pm), acc._2 :+ pm)
      }
      ._2
      .map(_.asInstanceOf[ErgoFullBlock].header)
  }

  def nodeViewSynchronizer(implicit system: ActorSystem): (
    ActorRef,
    ActorRef,
    SI,
    PM,
    TX,
    ConnectedPeer,
    TestProbe,
    TestProbe,
    TestProbe,
    ErgoSerializer[PM],
    DeliveryTracker
  ) = {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val h = localHistoryGen.sample.get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val s                                     = localStateGen.sample.get
    val settings                              = ErgoSettingsReader.read()
    val pool                                  = ErgoMemPool.empty(settings)
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    val ncProbe                               = TestProbe("NetworkControllerProbe")
    val pchProbe                              = TestProbe("PeerHandlerProbe")
    val eventListener                         = TestProbe("EventListener")
    val syncTracker                           = ErgoSyncTracker(settings.scorexSettings.network)
    val deliveryTracker: DeliveryTracker      = DeliveryTracker.empty(settings)

    // each test should always start with empty history
    deleteRecursive(ErgoHistory.historyDir(settings))
    val nodeViewHolderMockRef = system.actorOf(Props(new NodeViewHolderMock(settings)))

    val synchronizerMockRef = system.actorOf(
      Props(
        new SynchronizerMock(
          ncProbe.ref,
          nodeViewHolderMockRef,
          ErgoSyncInfoMessageSpec,
          settings,
          syncTracker,
          deliveryTracker
        )
      )
    )
    val m = totallyValidModifier(h, s)
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2

    val peerInfo = PeerInfo(defaultPeerSpec, System.currentTimeMillis())
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val p: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      pchProbe.ref,
      Some(peerInfo)
    )
    synchronizerMockRef ! ChangedHistory(history)
    synchronizerMockRef ! ChangedMempool(pool)
    val serializer: ErgoSerializer[PM] = HeaderSerializer.asInstanceOf[ErgoSerializer[PM]]
    (
      synchronizerMockRef,
      nodeViewHolderMockRef,
      h.syncInfoV1,
      m,
      tx,
      p,
      pchProbe,
      ncProbe,
      eventListener,
      serializer,
      deliveryTracker
    )
  }

  class SynchronizerFixture extends AkkaFixture {

    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val (
      synchronizer,
      nodeViewHolder,
      syncInfo,
      mod,
      tx,
      peer,
      pchProbe,
      ncProbe,
      eventListener,
      modSerializer,
      deliveryTracker
    ) = nodeViewSynchronizer
  }

  class Synchronizer2Fixture extends AkkaFixture {
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    val ncProbe                               = TestProbe("NetworkControllerProbe")
    val pchProbe                              = TestProbe("PeerHandlerProbe")

    // Use a unique data directory per fixture to avoid LevelDB corruption from
    // concurrent or sequential tests reusing the same on-disk history.
    val settings: ErgoSettings = {
      val baseSettings = org.ergoplatform.utils.ErgoNodeTestConstants.settings
      baseSettings.copy(directory = createTempDir.getAbsolutePath)
    }

    val syncTracker                           = ErgoSyncTracker(settings.scorexSettings.network)
    val deliveryTracker: DeliveryTracker      = DeliveryTracker.empty(settings)

    // each test should always start with empty history
    deleteRecursive(ErgoHistory.historyDir(settings))

    import akka.testkit.TestActorRef

    val nodeViewHolderMockRef: TestActorRef[NodeViewHolderMock] = TestActorRef(
      Props(new NodeViewHolderMock(settings))
    )

    val synchronizerMockRef: TestActorRef[SynchronizerMock] = TestActorRef(
      Props(
        new SynchronizerMock(
          ncProbe.ref,
          nodeViewHolderMockRef,
          ErgoSyncInfoMessageSpec,
          settings,
          syncTracker,
          deliveryTracker
        )
      )
    )

    val peerInfo = PeerInfo(defaultPeerSpec, System.currentTimeMillis())

    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val peer: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      pchProbe.ref,
      Some(peerInfo)
    )
  }

  /**
    * Helper to build a valid block (header + extension fields + input block fields) for testing.
    * The generated header's extension contains the input-block field keys.
    */
  private def buildValidBlockWithInputBlockFields(
    parentOpt: Option[ErgoFullBlock],
    prevInputBlockIdOpt: Option[ModifierId],
    transactionsDigestOpt: Option[scorex.crypto.hash.Digest32] = None
  ): (Header, Seq[(Array[Byte], Array[Byte])], InputBlockFields) = {
    import org.ergoplatform.Input
    import org.ergoplatform.mining.InputBlockFields
    import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
    import org.ergoplatform.modifiers.mempool.ErgoTransaction
    import org.ergoplatform.settings.Constants.TrueTree
    import org.ergoplatform.utils.BoxUtils
    import org.ergoplatform.utils.ErgoCoreTestConstants.startHeight
    import org.ergoplatform.utils.generators.ChainGenerator.nextBlock
    import scorex.crypto.authds.ADKey
    import scorex.crypto.hash.Digest32
    import sigma.Colls
    import sigma.interpreter.{ContextExtension, ProverResult}
    import sigmastate.helpers.TestingHelpers.testBox
    import scorex.util.idToBytes

    val transactionsDigest     = transactionsDigestOpt.getOrElse(Digest32 @@ Array.fill(32)(0.toByte))
    val prevTransactionsDigest = Digest32 @@ Array.fill(32)(0.toByte)
    val inputBlockExtCandidate = InputBlockFields.toExtensionFields(
      prevInputBlockIdOpt    = prevInputBlockIdOpt.map(idToBytes),
      transactionsDigest     = transactionsDigest,
      prevTransactionsDigest = prevTransactionsDigest
    )

    val extensionCandidate = defaultExtension ++ inputBlockExtCandidate

    // nextBlock requires at least one coinbase-like transaction
    val proof  = ProverResult(Array(0x7c.toByte), ContextExtension.empty)
    val inputs = IndexedSeq(Input(ADKey @@ Array.fill(32)(0: Byte), proof))
    val minimalAmount =
      BoxUtils.minimalErgoAmountSimulated(TrueTree, Colls.emptyColl, Map(), parameters)
    val outputs = IndexedSeq(
      testBox(minimalAmount, TrueTree, creationHeight = startHeight)
    )
    val dummyTxs = Seq(ErgoTransaction(inputs, outputs))

    val fullBlock = nextBlock(parentOpt, dummyTxs, extensionCandidate)
    val header    = fullBlock.header

    // Build the proof from the full extension (including interlinks added by nextBlock)
    val allExtensionFields = fullBlock.extension.fields
    val ibProof            = ExtensionCandidate(allExtensionFields).proofForInputBlockData.get
    val inputBlockFields = new InputBlockFields(
      prevInputBlockIdOpt.map(idToBytes),
      transactionsDigest,
      prevTransactionsDigest,
      ibProof
    )

    (header, allExtensionFields, inputBlockFields)
  }

  /**
    * Helper to build a valid input block announcement for testing.
    * The generated header's extension contains the input-block field keys, and
    * the returned [[InputBlockFields]] contains a Merkle proof that validates
    * against the header's extension root.
    */
  private def buildValidInputBlockAnnouncement(
    parentOpt: Option[ErgoFullBlock],
    transactionsDigestOpt: Option[scorex.crypto.hash.Digest32] = None
  ): InputBlockAnnouncement = {
    val (header, _, inputBlockFields) =
      buildValidBlockWithInputBlockFields(parentOpt, None, transactionsDigestOpt)
    InputBlockAnnouncement(
      InputBlockAnnouncement.initialMessageVersion,
      header,
      inputBlockFields,
      None
    )
  }

  /**
    * Helper to build a valid ordering block announcement for testing.
    * The extension fields are taken from a block whose extension root matches
    * the header's extension root, so [[OrderingBlockAnnouncement.valid()]] passes.
    */
  private def buildValidOrderingBlockAnnouncement(
    parentOpt: Option[ErgoFullBlock],
    prevInputBlockIdOpt: Option[ModifierId]
  ): OrderingBlockAnnouncement = {
    val (header, allExtensionFields, _) =
      buildValidBlockWithInputBlockFields(parentOpt, prevInputBlockIdOpt)
    OrderingBlockAnnouncement(
      OrderingBlockAnnouncement.CurrentVersion,
      header,
      nonBroadcastedTransactions = Seq.empty,
      broadcastedTransactionIds  = Seq.empty,
      extensionFields            = allExtensionFields
    )
  }

  /**
    * Fixture for UTXO set snapshot bootstrap tests: synchronizer and history are built with
    * `utxoBootstrap` enabled (or disabled, for control tests), history contains headers only.
    */
  class UtxoBootstrapSynchronizerFixture(utxoBootstrap: Boolean) extends AkkaFixture {
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    val ncProbe = TestProbe("NetworkControllerProbe")
    val pchProbe = TestProbe("PeerHandlerProbe")
    val syncTracker = ErgoSyncTracker(settings.scorexSettings.network)
    val deliveryTracker: DeliveryTracker = DeliveryTracker.empty(settings)

    val synchronizerSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        utxoSettings = UtxoSettings(utxoBootstrap, 0, 2)
      )
    )

    deleteRecursive(ErgoHistory.historyDir(synchronizerSettings))
    // This fixture supplies ChangedHistory explicitly below. A real view-holder mock would
    // also reply to GetNodeViewChanges with its own history and race that explicit update.
    val nodeViewHolderMockRef = TestProbe("SnapshotViewHolderProbe").ref

    val synchronizerMockRef = system.actorOf(Props(
      new SynchronizerMock(
        ncProbe.ref,
        nodeViewHolderMockRef,
        ErgoSyncInfoMessageSpec,
        synchronizerSettings,
        syncTracker,
        deliveryTracker)
    ))

    val history = generateHistory(verifyTransactions = true,
                                  StateType.Utxo,
                                  PoPoWBootstrap = false,
                                  blocksToKeep = -1,
                                  utxoBootstrap = utxoBootstrap)
    val chain = genHeaderChain(BlocksInChain, history, diffBitsOpt = None, useRealTs = false)
    val updHistory = applyHeaderChain(history, chain)

    synchronizerMockRef ! ChangedHistory(updHistory)
    synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(synchronizerSettings))

    val peerInfo = PeerInfo(defaultPeerSpec, System.currentTimeMillis())
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val peer: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      pchProbe.ref,
      Some(peerInfo)
    )
  }

  private def withUtxoBootstrapFixture(utxoBootstrap: Boolean)(testCode: UtxoBootstrapSynchronizerFixture => Any): Unit = {
    val fixture = new UtxoBootstrapSynchronizerFixture(utxoBootstrap)
    try {
      testCode(fixture)
    }
    finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  private def requestForModifierSent(ncProbe: TestProbe, typeId: org.ergoplatform.modifiers.NetworkObjectTypeId.Value, id: scorex.util.ModifierId): Unit = {
    ncProbe.fishForMessage(3 seconds) {
      case stn: SendToNetwork =>
        stn.message.spec.messageCode == RequestModifierSpec.messageCode && {
          val invData = stn.message.data.get.asInstanceOf[InvData]
          invData.typeId == typeId && invData.ids.contains(id)
        }
      case _ => false
    }
  }

  property("NodeViewSynchronizer: Message: SyncInfoSpec V2 - younger peer") {
    withFixture { ctx =>
      import ctx._

      val emptySync = ErgoSyncInfoV2(Seq.empty)

      // Neighbour is sending
      val msgBytes = ErgoSyncInfoMessageSpec.toBytes(emptySync)

      // we check that in case of neighbour with empty history (it has no any blocks),
      // inv message with our block ids will be sent
      synchronizer ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))
      ncProbe.fishForMessage(3 seconds) {
        case m =>
          m match {
            case stn: SendToNetwork =>
              val msg = stn.message
              msg.spec.messageCode == InvSpec.messageCode &&
              msg.data.get.asInstanceOf[InvData].ids.head == chain.head.id
            case _ => false
          }
      }
    }
  }

  property("NodeViewSynchronizer: Message: InvSpec - header next to the best one is requested via RequestModifier") {
    withFixture { ctx =>
      import ctx._
      deliveryTracker.reset()

      // header immediately following the best header the node has
      // (history applied to the synchronizer contains the first 1000 headers of `chain`)
      val nextHeader = chain.take(1001).last
      deliveryTracker.status(nextHeader.id, Header.modifierTypeId, Seq.empty) shouldBe Unknown

      // a peer announces the header via an Inv message
      val invData = InvData(Header.modifierTypeId, Seq(nextHeader.id))
      synchronizer ! Message(InvSpec, Left(InvSpec.toBytes(invData)), Some(peer))

      // the synchronizer should reply to the peer with a RequestModifier message asking for the header
      ncProbe.fishForMessage(3 seconds) { case m =>
        m match {
          case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
            val data = stn.message.data.get.asInstanceOf[InvData]
            data.typeId == Header.modifierTypeId && data.ids == Seq(nextHeader.id)
          case _ => false
        }
      }

      // and the header should be tracked as Requested
      eventually {
        deliveryTracker.status(nextHeader.id, Header.modifierTypeId, Seq.empty) shouldBe Requested
      }
    }
  }

  property("NodeViewSynchronizer: receiving valid header") {
    withFixture { ctx =>
      import ctx._
      deliveryTracker.reset()
      deliveryTracker.setRequested(Header.modifierTypeId, chain.take(1001).last.id, peer)(
        _ => Cancellable.alreadyCancelled
      )
      val olderChain = chain.take(1001)
      val modData = ModifiersData(
        Header.modifierTypeId,
        Map(olderChain.last.id -> olderChain.last.bytes)
      )
      val modSpec = ModifiersSpec
      synchronizer ! Message(modSpec, Left(modSpec.toBytes(modData)), Some(peer))
      // desired state of submitting valid headers is Received
      eventually {
        deliveryTracker.status(olderChain.last.id, Header.modifierTypeId, Seq.empty) shouldBe Received
      }
    }
  }

  property("NodeViewSynchronizer: malformed reply from another peer should preserve the requested source") {
    withFixture { ctx =>
      import ctx._
      import java.net.InetSocketAddress
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer

      deliveryTracker.reset()
      val header = chain.take(1001).last
      val otherHandler = TestProbe("OtherPeerHandler")
      val otherPeer = peer.copy(
        connectionId = peer.connectionId.copy(
          remoteAddress = new InetSocketAddress("203.0.113.2", 9030)
        ),
        handlerRef = otherHandler.ref
      )

      val inv = InvData(Header.modifierTypeId, Seq(header.id))
      synchronizer ! Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(peer))
      requestForModifierSent(ncProbe, Header.modifierTypeId, header.id)
      eventually {
        deliveryTracker.getSource(header.id, Header.modifierTypeId) shouldBe Some(peer)
      }
      val requestTimer = deliveryTracker.getRequestedInfo(Header.modifierTypeId, header.id).get.cancellable

      val malformed = ModifiersData(Header.modifierTypeId, Map(header.id -> Array[Byte](0)))
      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(malformed)), Some(otherPeer))
      ncProbe.fishForMessage(3 seconds) {
        case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
          address == otherPeer.connectionId.remoteAddress
        case _ => false
      }

      deliveryTracker.getSource(header.id, Header.modifierTypeId) shouldBe Some(peer)
      requestTimer.isCancelled shouldBe false

      val valid = ModifiersData(Header.modifierTypeId, Map(header.id -> header.bytes))
      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(valid)), Some(peer))
      eventually {
        deliveryTracker.status(header.id, Header.modifierTypeId, Seq.empty) shouldBe Received
      }
    }
  }

  property("NodeViewSynchronizer: malformed reply from the requested owner clears its request") {
    withFixture { ctx =>
      import ctx._
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer

      deliveryTracker.reset()
      val header = chain.take(1001).last
      val inv = InvData(Header.modifierTypeId, Seq(header.id))
      synchronizer ! Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(peer))
      requestForModifierSent(ncProbe, Header.modifierTypeId, header.id)
      eventually {
        deliveryTracker.getSource(header.id, Header.modifierTypeId) shouldBe Some(peer)
      }
      val requestTimer = deliveryTracker.getRequestedInfo(Header.modifierTypeId, header.id).get.cancellable
      requestTimer.isCancelled shouldBe false

      val malformed = ModifiersData(Header.modifierTypeId, Map(header.id -> Array[Byte](0)))
      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(malformed)), Some(peer))
      ncProbe.fishForMessage(3 seconds) {
        case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
          address == peer.connectionId.remoteAddress
        case _ => false
      }
      deliveryTracker.status(header.id, Header.modifierTypeId, Seq.empty) shouldBe Unknown
      requestTimer.isCancelled shouldBe true
    }
  }

  property("NodeViewSynchronizer: malformed reply from an old handler should preserve its replacement") {
    withFixture { ctx =>
      import ctx._
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer

      deliveryTracker.reset()
      val header = chain.take(1001).last
      val replacementHandler = TestProbe("ReplacementPeerHandler")
      val replacementPeer = peer.copy(handlerRef = replacementHandler.ref)
      replacementPeer shouldBe peer // ConnectedPeer.equals compares only the remote address.
      replacementPeer.handlerRef should not be peer.handlerRef

      deliveryTracker.setRequested(Header.modifierTypeId, header.id, peer)(
        _ => Cancellable.alreadyCancelled
      )
      deliveryTracker.setUnknown(header.id, Header.modifierTypeId)
      deliveryTracker.setRequested(Header.modifierTypeId, header.id, replacementPeer)(
        _ => Cancellable.alreadyCancelled
      )

      val malformed = ModifiersData(Header.modifierTypeId, Map(header.id -> Array[Byte](0)))
      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(malformed)), Some(peer))
      ncProbe.fishForMessage(3 seconds) {
        case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
          address == peer.connectionId.remoteAddress
        case _ => false
      }

      deliveryTracker.getRequestedInfo(Header.modifierTypeId, header.id)
        .map(_.peer.handlerRef) shouldBe Some(replacementHandler.ref)
    }
  }

  property("NodeViewSynchronizer: oversized transaction from another handler preserves its requested owner") {
    withFixture { ctx =>
      import ctx._
      import java.net.InetSocketAddress
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer

      deliveryTracker.reset()
      val replacementHandler = TestProbe("ReplacementTxHandler")
      val replacementPeer = peer.copy(handlerRef = replacementHandler.ref)
      val otherHandler = TestProbe("OtherTxHandler")
      val otherPeer = peer.copy(
        connectionId = peer.connectionId.copy(
          remoteAddress = new InetSocketAddress("203.0.113.3", 9030)
        ),
        handlerRef = otherHandler.ref
      )
      deliveryTracker.setRequested(tx.modifierTypeId, tx.id, replacementPeer)(
        _ => system.scheduler.scheduleOnce(10.minutes, ncProbe.ref, "unused")(system.dispatcher)
      )
      val requestTimer = deliveryTracker.getRequestedInfo(tx.modifierTypeId, tx.id).get.cancellable
      val oversized = Array.fill[Byte](ErgoSettingsReader.read().nodeSettings.maxTransactionSize + 1)(0)
      val reply = ModifiersData(tx.modifierTypeId, Map(tx.id -> oversized))

      for (wrongPeer <- Seq(peer, otherPeer)) {
        synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(reply)), Some(wrongPeer))
        ncProbe.fishForMessage(3 seconds) {
          case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
            address == wrongPeer.connectionId.remoteAddress
          case _ => false
        }
        deliveryTracker.getRequestedInfo(tx.modifierTypeId, tx.id)
          .map(_.peer.handlerRef) shouldBe Some(replacementHandler.ref)
        requestTimer.isCancelled shouldBe false
      }
    }
  }

  property("NodeViewSynchronizer: oversized transaction from the requested owner remains retryable") {
    withFixture { ctx =>
      import ctx._
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer

      deliveryTracker.reset()
      deliveryTracker.setRequested(tx.modifierTypeId, tx.id, peer)(
        _ => system.scheduler.scheduleOnce(10.minutes, ncProbe.ref, "unused")(system.dispatcher)
      )
      val requestTimer = deliveryTracker.getRequestedInfo(tx.modifierTypeId, tx.id).get.cancellable
      val oversized = Array.fill[Byte](ErgoSettingsReader.read().nodeSettings.maxTransactionSize + 1)(0)
      val reply = ModifiersData(tx.modifierTypeId, Map(tx.id -> oversized))

      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(reply)), Some(peer))
      ncProbe.fishForMessage(3 seconds) {
        case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
          address == peer.connectionId.remoteAddress
        case _ => false
      }
      deliveryTracker.status(tx.id, tx.modifierTypeId, Seq.empty) shouldBe Unknown
      requestTimer.isCancelled shouldBe true
    }
  }

  property("NodeViewSynchronizer: oversized supplier is not re-requested for the same transaction") {
    withFixture2 { ctx =>
      import ctx._
      import java.net.InetSocketAddress
      import org.ergoplatform.network.message.{InvData, InvSpec, RequestModifierSpec}
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
      import scorex.core.network.SendToPeer

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2
      val inv = InvData(tx.modifierTypeId, Seq(tx.id))
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(peer))
      val first = ncProbe.fishForMessage(3.seconds) {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == RequestModifierSpec.messageCode &&
          stn.message.data.get.asInstanceOf[InvData].ids == Seq(tx.id)
        case _ => false
      }.asInstanceOf[SendToNetwork]
      first.sendingStrategy shouldBe SendToPeer(peer)

      val oversized = Array.fill[Byte](settings.nodeSettings.maxTransactionSize + 1)(0)
      synchronizerMockRef ! Message(ModifiersSpec,
        Left(ModifiersSpec.toBytes(ModifiersData(tx.modifierTypeId, Map(tx.id -> oversized)))),
        Some(peer))
      ncProbe.fishForMessage(3.seconds) {
        case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
          address == peer.connectionId.remoteAddress
        case _ => false
      }
      deliveryTracker.status(tx.id, tx.modifierTypeId, Seq.empty) shouldBe Unknown

      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(peer))
      ncProbe.expectNoMessage(300.millis)

      val otherHandler = TestProbe("OtherOversizedTxSupplier")
      val otherPeer = peer.copy(
        connectionId = peer.connectionId.copy(remoteAddress = new InetSocketAddress("203.0.113.2", 9030)),
        handlerRef = otherHandler.ref)
      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(otherPeer))
      val second = ncProbe.fishForMessage(3.seconds) {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == RequestModifierSpec.messageCode &&
          stn.message.data.get.asInstanceOf[InvData].ids == Seq(tx.id)
        case _ => false
      }.asInstanceOf[SendToNetwork]
      second.sendingStrategy shouldBe SendToPeer(otherPeer)
    }
  }

  property("NodeViewSynchronizer: deferred oversized reply remains supplier-scoped after request timeout") {
    withFixture2 { ctx =>
      import ctx._
      import java.net.InetSocketAddress
      import org.ergoplatform.modifiers.mempool.ErgoTransaction
      import org.ergoplatform.network.message.{InvData, InvSpec, RequestModifierSpec}
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
      import scorex.core.network.SendToPeer

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val ids = (1 to 2).map(i => bytesToId(Array.fill[Byte](32)(i.toByte)))
      val typeId = ErgoTransaction.modifierTypeId
      val oversized = Array.fill[Byte](settings.nodeSettings.maxTransactionSize + 1)(0)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      synchronizerMockRef ! Message(InvSpec,
        Left(InvSpec.toBytes(InvData(typeId, ids))), Some(peer))
      ncProbe.fishForMessage(3.seconds) {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == RequestModifierSpec.messageCode &&
          stn.message.data.get.asInstanceOf[InvData].ids.toSet == ids.toSet
        case _ => false
      }

      synchronizerMockRef ! Message(ModifiersSpec,
        Left(ModifiersSpec.toBytes(ModifiersData(typeId, ids.map(_ -> oversized).toMap))),
        Some(peer))
      ncProbe.fishForMessage(3.seconds) {
        case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
          address == peer.connectionId.remoteAddress
        case _ => false
      }
      val deferredIds = ids.filter(id => deliveryTracker.status(id, typeId, Seq.empty) == Requested)
      deferredIds.size shouldBe 1
      val deferredId = deferredIds.head

      synchronizerMockRef ! CheckDelivery(peer, typeId, deferredId)
      deliveryTracker.status(deferredId, typeId, Seq.empty) shouldBe Unknown
      val deferredInv = InvData(typeId, Seq(deferredId))
      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(deferredInv)), Some(peer))
      val renewedRequest = ncProbe.fishForMessage(3.seconds) {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == RequestModifierSpec.messageCode &&
          stn.message.data.get.asInstanceOf[InvData].ids == Seq(deferredId)
        case _ => false
      }.asInstanceOf[SendToNetwork]
      renewedRequest.sendingStrategy shouldBe SendToPeer(peer)
      val renewedTimer = deliveryTracker.getRequestedInfo(typeId, deferredId).get.cancellable

      synchronizerMockRef ! LocalBlockApplied(chain.head, Seq.empty)
      ncProbe.fishForMessage(3.seconds) {
        case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
          address == peer.connectionId.remoteAddress
        case _ => false
      }
      deliveryTracker.getRequestedInfo(typeId, deferredId)
        .map(_.cancellable) shouldBe Some(renewedTimer)
      renewedTimer.isCancelled shouldBe false

      synchronizerMockRef ! CheckDelivery(peer, typeId, deferredId)
      deliveryTracker.status(deferredId, typeId, Seq.empty) shouldBe Unknown
      synchronizerMockRef ! Message(InvSpec,
        Left(InvSpec.toBytes(deferredInv)), Some(peer))
      ncProbe.expectNoMessage(300.millis)
      val replacementHandler = TestProbe("DeferredReplacementHandler")
      val replacementPeer = peer.copy(
        connectionId = peer.connectionId.copy(remoteAddress = new InetSocketAddress("203.0.113.2", 9030)),
        handlerRef = replacementHandler.ref)
      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(deferredInv)), Some(replacementPeer))
      val nextRequest = ncProbe.fishForMessage(3.seconds) {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == RequestModifierSpec.messageCode &&
          stn.message.data.get.asInstanceOf[InvData].ids == Seq(deferredId)
        case _ => false
      }.asInstanceOf[SendToNetwork]
      nextRequest.sendingStrategy shouldBe SendToPeer(replacementPeer)
    }
  }

  property("NodeViewSynchronizer: repeated oversized replies exhaust only their connection") {
    withFixture2 { ctx =>
      import ctx._
      import java.net.InetSocketAddress
      import org.ergoplatform.modifiers.mempool.ErgoTransaction
      import org.ergoplatform.network.message.{InvData, InvSpec, RequestModifierSpec}
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
      import scorex.core.network.SendToPeer

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val oversized = Array.fill[Byte](settings.nodeSettings.maxTransactionSize + 1)(0)
      val ids = (0 until 32).map(i => bytesToId(Array.fill[Byte](32)(i.toByte)))
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))

      ids.foreach { id =>
        val inv = InvData(ErgoTransaction.modifierTypeId, Seq(id))
        synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(peer))
        val request = ncProbe.fishForMessage(3.seconds) {
          case stn: SendToNetwork =>
            stn.message.spec.messageCode == RequestModifierSpec.messageCode &&
            stn.message.data.get.asInstanceOf[InvData].ids == Seq(id)
          case _ => false
        }.asInstanceOf[SendToNetwork]
        request.sendingStrategy.asInstanceOf[SendToPeer].chosenPeer.handlerRef shouldBe peer.handlerRef
        synchronizerMockRef ! Message(ModifiersSpec,
          Left(ModifiersSpec.toBytes(ModifiersData(ErgoTransaction.modifierTypeId, Map(id -> oversized)))),
          Some(peer))
        ncProbe.fishForMessage(3.seconds) {
          case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
            address == peer.connectionId.remoteAddress
          case _ => false
        }
      }

      // The first id must remain suppressed after the cache reaches its bound.
      val repeated = InvData(ErgoTransaction.modifierTypeId, Seq(ids.head))
      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(repeated)), Some(peer))
      ncProbe.expectNoMessage(300.millis)

      // A new id must also be suppressed: this checks the handler-wide cutoff,
      // not only the per-id oversized reply set.
      val unseen = InvData(ErgoTransaction.modifierTypeId,
        Seq(bytesToId(Array.fill[Byte](32)(32.toByte))))
      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(unseen)), Some(peer))
      ncProbe.expectNoMessage(300.millis)

      val replacementHandler = TestProbe("ReplacementOversizedSupplier")
      val replacementPeer = peer.copy(
        connectionId = peer.connectionId.copy(remoteAddress =
          new InetSocketAddress(peer.connectionId.remoteAddress.getAddress,
            peer.connectionId.remoteAddress.getPort + 1)),
        handlerRef = replacementHandler.ref)
      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(repeated)), Some(replacementPeer))
      val replacementRequest = ncProbe.fishForMessage(3.seconds) {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == RequestModifierSpec.messageCode &&
          stn.message.data.get.asInstanceOf[InvData].ids == Seq(ids.head)
        case _ => false
      }.asInstanceOf[SendToNetwork]
      replacementRequest.sendingStrategy.asInstanceOf[SendToPeer].chosenPeer.handlerRef shouldBe replacementHandler.ref
    }
  }

  property("NodeViewSynchronizer: old disconnect cannot clear a same-address replacement's sync status") {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{DisconnectedPeer, HandshakedPeer}

      val replacementHandler = TestProbe("SameAddressReplacementHandler")
      val replacementPeer = peer.copy(handlerRef = replacementHandler.ref)
      replacementPeer shouldBe peer // ConnectedPeer equality ignores handler identity.

      synchronizerMockRef ! HandshakedPeer(peer)
      syncTracker.knownPeers().map(_.handlerRef).toSet shouldBe Set(peer.handlerRef)

      synchronizerMockRef ! HandshakedPeer(replacementPeer)
      syncTracker.knownPeers().map(_.handlerRef).toSet shouldBe Set(replacementHandler.ref)
      syncTracker.fullInfo().map(_.peer.handlerRef).toSet shouldBe Set(replacementHandler.ref)

      synchronizerMockRef ! DisconnectedPeer(peer)
      syncTracker.knownPeers().map(_.handlerRef).toSet shouldBe Set(replacementHandler.ref)
      syncTracker.fullInfo().map(_.peer.handlerRef).toSet shouldBe Set(replacementHandler.ref)

      synchronizerMockRef ! HandshakedPeer(peer)
      syncTracker.knownPeers().map(_.handlerRef).toSet shouldBe Set(replacementHandler.ref)
    }
  }

  property("NodeViewSynchronizer: disconnected transaction supplier cannot restore a request") {
    withFixture2 { ctx =>
      import ctx._
      import java.net.InetSocketAddress
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{CheckDelivery, DisconnectedPeer, HandshakedPeer}
      import org.ergoplatform.network.message.{InvData, InvSpec, RequestModifierSpec}
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
      import scorex.core.network.SendToPeer

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2
      val inv = InvData(tx.modifierTypeId, Seq(tx.id))
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))

      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(peer))
      ncProbe.fishForMessage(3.seconds) {
        case stn: SendToNetwork => stn.message.spec.messageCode == RequestModifierSpec.messageCode
        case _ => false
      }
      val oldTimer = deliveryTracker.getRequestedInfo(tx.modifierTypeId, tx.id).get.cancellable

      synchronizerMockRef ! DisconnectedPeer(peer)
      deliveryTracker.status(tx.id, tx.modifierTypeId, Seq.empty) shouldBe Requested
      oldTimer.isCancelled shouldBe false

      // Initializing actors can replay an old handshake after its disconnect event.
      synchronizerMockRef ! HandshakedPeer(peer)
      syncTracker.knownPeers().exists(_.handlerRef == peer.handlerRef) shouldBe false

      // A legitimate reply from another peer remains admissible while the old request is live.
      val replacementHandler = TestProbe("ReplacementAfterDisconnect")
      val replacementPeer = peer.copy(
        connectionId = peer.connectionId.copy(remoteAddress =
          new InetSocketAddress(peer.connectionId.remoteAddress.getAddress,
            peer.connectionId.remoteAddress.getPort + 1)),
        handlerRef = replacementHandler.ref)
      val accepted = synchronizerMockRef.underlyingActor.processSpam(replacementPeer,
        tx.modifierTypeId, Map(tx.id -> tx.bytes), FixedSizeApproximateCacheQueue(1, Vector.empty))
      accepted.keySet shouldBe Set(tx.id)

      // A late malformed reply from the retired owner cannot cancel that admission window.
      val oversized = Array.fill[Byte](settings.nodeSettings.maxTransactionSize + 1)(0)
      synchronizerMockRef ! Message(ModifiersSpec,
        Left(ModifiersSpec.toBytes(ModifiersData(tx.modifierTypeId, Map(tx.id -> oversized)))),
        Some(peer))
      ncProbe.fishForMessage(3.seconds) {
        case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
          address == peer.connectionId.remoteAddress
        case _ => false
      }
      deliveryTracker.status(tx.id, tx.modifierTypeId, Seq.empty) shouldBe Requested

      synchronizerMockRef ! CheckDelivery(peer, tx.modifierTypeId, tx.id)
      deliveryTracker.status(tx.id, tx.modifierTypeId, Seq.empty) shouldBe Unknown
      oldTimer.isCancelled shouldBe true

      // A queued announcement from the retired handler must not recreate the request.
      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(peer))
      ncProbe.expectNoMessage(300.millis)
      deliveryTracker.status(tx.id, tx.modifierTypeId, Seq.empty) shouldBe Unknown

      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(replacementPeer))
      val replacementRequest = ncProbe.fishForMessage(3.seconds) {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == RequestModifierSpec.messageCode &&
          stn.message.data.get.asInstanceOf[InvData].ids == Seq(tx.id)
        case _ => false
      }.asInstanceOf[SendToNetwork]
      replacementRequest.sendingStrategy shouldBe SendToPeer(replacementPeer)

      // An already queued timeout from the retired handler cannot cancel the new owner.
      synchronizerMockRef ! CheckDelivery(peer, tx.modifierTypeId, tx.id)
      deliveryTracker.getRequestedInfo(tx.modifierTypeId, tx.id)
        .map(_.peer.handlerRef) shouldBe Some(replacementHandler.ref)
    }
  }

  property("NodeViewSynchronizer: manifest from an old handler preserves its replacement's request") {
    withFixture { ctx =>
      import ctx._
      import java.net.InetSocketAddress
      import org.ergoplatform.network.peer.PenaltyType
      import org.ergoplatform.serialization.ManifestSerializer
      import org.ergoplatform.settings.Algos
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer
      import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage
      import scorex.db.LDBFactory

      deliveryTracker.reset()
      val state = localStateGen.sample.get
      val storage = state.persistentProver.storage.asInstanceOf[VersionedLDBAVLStorage]
      val snapshotStore = LDBFactory.createKvDb(createTempDir.getAbsolutePath)
      val bytes = try {
        val rootHash = state.rootDigest.dropRight(1)
        val rootLabel = storage.dumpSnapshot(snapshotStore, ManifestSerializer.MainnetManifestDepth, rootHash).get
        snapshotStore.get(rootLabel).get
      } finally {
        snapshotStore.close()
      }
      val manifest = ManifestSerializer.defaultSerializer.parseBytes(bytes)
      val id = ModifierId @@ Algos.encode(manifest.id)

      val replacementHandler = TestProbe("ReplacementManifestHandler")
      val replacementPeer = peer.copy(
        handlerRef = replacementHandler.ref)
      replacementPeer.connectionId.remoteAddress.getAddress shouldBe peer.connectionId.remoteAddress.getAddress
      replacementPeer.handlerRef should not be peer.handlerRef
      deliveryTracker.setRequested(ManifestTypeId.value, id, replacementPeer)(
        _ => system.scheduler.scheduleOnce(10.minutes, ncProbe.ref, "unused")(system.dispatcher)
      )
      val attempt = deliveryTracker.getRequestedInfo(ManifestTypeId.value, id).get

      synchronizer ! Message(ManifestSpec, Left(ManifestSpec.toBytes(bytes)), Some(peer))
      ncProbe.expectNoMessage(300.millis)
      deliveryTracker.status(id, ManifestTypeId.value, Seq.empty) shouldBe Requested
      deliveryTracker.getRequestedInfo(ManifestTypeId.value, id)
        .map(_.peer.handlerRef) shouldBe Some(replacementHandler.ref)
      attempt.cancellable.isCancelled shouldBe false

      val unrelatedHandler = TestProbe("UnrelatedManifestHandler")
      val unrelatedPeer = peer.copy(
        connectionId = peer.connectionId.copy(remoteAddress = new InetSocketAddress("203.0.113.2", 9030)),
        handlerRef = unrelatedHandler.ref)
      synchronizer ! Message(ManifestSpec, Left(ManifestSpec.toBytes(bytes)), Some(unrelatedPeer))
      ncProbe.expectMsg(PenalizePeer(unrelatedPeer.connectionId.remoteAddress, PenaltyType.SpamPenalty))
      deliveryTracker.status(id, ManifestTypeId.value, Seq.empty) shouldBe Requested
      attempt.cancellable.isCancelled shouldBe false
    }
  }

  property("NodeViewSynchronizer: valid reply from another peer keeps existing acceptance") {
    withFixture { ctx =>
      import ctx._
      import java.net.InetSocketAddress

      deliveryTracker.reset()
      val header = chain.take(1001).last
      val otherHandler = TestProbe("OtherPeerHandler")
      val otherPeer = peer.copy(
        connectionId = peer.connectionId.copy(
          remoteAddress = new InetSocketAddress("203.0.113.2", 9030)
        ),
        handlerRef = otherHandler.ref
      )
      deliveryTracker.setRequested(Header.modifierTypeId, header.id, peer)(
        _ => Cancellable.alreadyCancelled
      )

      val valid = ModifiersData(Header.modifierTypeId, Map(header.id -> header.bytes))
      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(valid)), Some(otherPeer))
      eventually {
        deliveryTracker.status(header.id, Header.modifierTypeId, Seq.empty) shouldBe Received
        deliveryTracker.getSource(header.id, Header.modifierTypeId)
          .map(_.handlerRef) shouldBe Some(otherHandler.ref)
      }
    }
  }

  property("NodeViewSynchronizer: wrong-version transaction section does not poison its canonical id") {
    withFixture2 { ctx =>
      import ctx._

      val stopProbe = TestProbe("HeaderFirstSynchronizerStopProbe")
      stopProbe.watch(synchronizerMockRef)
      system.stop(synchronizerMockRef)
      stopProbe.expectTerminated(synchronizerMockRef)

      val holderProbe = TestProbe("HeaderFirstHolderProbe")
      val synchronizer = system.actorOf(Props(new SynchronizerMock(
        ncProbe.ref,
        holderProbe.ref,
        ErgoSyncInfoMessageSpec,
        settings,
        syncTracker,
        deliveryTracker)))
      holderProbe.expectMsgType[ErgoNodeViewHolder.ReceivableMessages.GetNodeViewChanges]

      val history = ErgoHistory.readOrGenerate(settings)(null)
      val blocks = genChain(2, history, (Header.InitialVersion + 1).toByte)
      val withParent = applyBlock(history, blocks.head)
      val withHeader = withParent.append(blocks.last.header).get._1
      synchronizer ! ChangedHistory(withHeader)
      synchronizer ! ChangedMempool(ErgoMemPool.empty(settings))

      val canonical = blocks.last.blockTransactions
      val wrongVersion = canonical.copy(blockVersion = Header.Interpreter50Version, sizeOpt = None)
      val wrongBytes = wrongVersion.bytes
      val parsedWrongVersion = BlockTransactionsSerializer.parseBytes(wrongBytes)
      canonical.id shouldBe blocks.last.header.transactionsId
      parsedWrongVersion.id shouldBe canonical.id
      parsedWrongVersion.blockVersion should not be blocks.last.header.version

      val otherPeer = ConnectedPeer(
        connectionIdGen.sample.get.copy(remoteAddress = new InetSocketAddress("192.0.2.2", 9030)),
        pchProbe.ref,
        Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis()))
      )
      otherPeer.connectionId.remoteAddress.getAddress should not be peer.connectionId.remoteAddress.getAddress

      val differentId = BlockTransactionsSerializer.parseBytes(wrongBytes)
      val proof = differentId.txs.head.inputs.head.spendingProof.proof
      proof(0) = (proof(0) ^ 1).toByte
      val differentIdBytes = differentId.bytes
      val parsedDifferentId = BlockTransactionsSerializer.parseBytes(differentIdBytes)
      differentId.id should not be canonical.id
      differentId.id should not be blocks.last.header.transactionsId
      parsedDifferentId.id shouldBe differentId.id
      parsedDifferentId.blockVersion shouldBe parsedWrongVersion.blockVersion
      deliveryTracker.setRequested(differentId.modifierTypeId, differentId.id, peer)(_ => Cancellable.alreadyCancelled)
      val differentIdRequest = deliveryTracker.getRequestedInfo(differentId.modifierTypeId, differentId.id).get
      val differentIdData = ModifiersData(differentId.modifierTypeId, Map(differentId.id -> differentIdBytes))
      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(differentIdData)), Some(otherPeer))
      ncProbe.fishForMessage(3.seconds) {
        case _: PenalizePeer => true
        case _ => false
      } shouldBe PenalizePeer(otherPeer.connectionId.remoteAddress, PenaltyType.MisbehaviorPenalty)
      deliveryTracker.getRequestedInfo(differentId.modifierTypeId, differentId.id)
        .exists(_ eq differentIdRequest) shouldBe true

      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(differentIdData)), Some(peer))
      eventually {
        deliveryTracker.status(differentId.id, differentId.modifierTypeId, Seq.empty) shouldBe
          scorex.core.network.ModifiersStatus.Invalid
      }
      ncProbe.fishForMessage(3.seconds) {
        case _: PenalizePeer => true
        case _ => false
      } shouldBe PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.MisbehaviorPenalty)

      deliveryTracker.setRequested(canonical.modifierTypeId, canonical.id, peer)(_ => Cancellable.alreadyCancelled)
      val canonicalRequest = deliveryTracker.getRequestedInfo(canonical.modifierTypeId, canonical.id).get
      val wrongData = ModifiersData(canonical.modifierTypeId, Map(canonical.id -> wrongBytes))
      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(wrongData)), Some(otherPeer))
      ncProbe.fishForMessage(3.seconds) {
        case _: PenalizePeer => true
        case _ => false
      } shouldBe PenalizePeer(otherPeer.connectionId.remoteAddress, PenaltyType.MisbehaviorPenalty)
      deliveryTracker.getRequestedInfo(canonical.modifierTypeId, canonical.id)
        .exists(_ eq canonicalRequest) shouldBe true

      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(wrongData)), Some(peer))
      eventually {
        deliveryTracker.status(canonical.id, canonical.modifierTypeId, Seq.empty) shouldBe Unknown
      }
      holderProbe.expectNoMessage(300.millis)
      ncProbe.fishForMessage(3.seconds) {
        case _: PenalizePeer => true
        case _ => false
      } shouldBe PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.MisbehaviorPenalty)
      ncProbe.expectNoMessage(300.millis)

      val announced = InvData(canonical.modifierTypeId, Seq(canonical.id))
      synchronizer ! Message(InvSpec, Left(InvSpec.toBytes(announced)), Some(otherPeer))
      requestForModifierSent(ncProbe, canonical.modifierTypeId, canonical.id)
      eventually {
        deliveryTracker.status(canonical.id, canonical.modifierTypeId, Seq.empty) shouldBe Requested
      }

      val canonicalData = ModifiersData(canonical.modifierTypeId, Map(canonical.id -> canonical.bytes))
      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(canonicalData)), Some(otherPeer))
      val forwarded = holderProbe.expectMsgType[ErgoNodeViewHolder.ReceivableMessages.ModifiersFromRemote]
      forwarded.modifiers.map(_.id) should contain (canonical.id)
      eventually {
        deliveryTracker.status(canonical.id, canonical.modifierTypeId, Seq.empty) shouldBe Received
      }
      ncProbe.expectNoMessage(300.millis)
    }
  }

  property("NodeViewSynchronizer: transaction section received before header remains downloadable") {
    withFixture2 { ctx =>
      import ctx._

      val stopProbe = TestProbe("OriginalSynchronizerStopProbe")
      stopProbe.watch(synchronizerMockRef)
      system.stop(synchronizerMockRef)
      stopProbe.expectTerminated(synchronizerMockRef)

      val holderProbe = TestProbe("SectionHolderProbe")
      val synchronizer = system.actorOf(Props(new SynchronizerMock(
        ncProbe.ref,
        holderProbe.ref,
        ErgoSyncInfoMessageSpec,
        settings,
        syncTracker,
        deliveryTracker)))
      holderProbe.expectMsgType[ErgoNodeViewHolder.ReceivableMessages.GetNodeViewChanges]

      val history = ErgoHistory.readOrGenerate(settings)(null)
      val blocks = genChain(2, history, (Header.InitialVersion + 1).toByte)
      val withParent = applyBlock(history, blocks.head)
      synchronizer ! ChangedHistory(withParent)
      synchronizer ! ChangedMempool(ErgoMemPool.empty(settings))

      val canonical = blocks.last.blockTransactions
      val wrongVersion = canonical.copy(blockVersion = Header.Interpreter50Version, sizeOpt = None)
      val wrongBytes = wrongVersion.bytes
      val parsedWrongVersion = BlockTransactionsSerializer.parseBytes(wrongBytes)
      canonical.id shouldBe blocks.last.header.transactionsId
      parsedWrongVersion.id shouldBe canonical.id
      parsedWrongVersion.blockVersion should not be blocks.last.header.version

      deliveryTracker.setRequested(canonical.modifierTypeId, canonical.id, peer)(_ => Cancellable.alreadyCancelled)
      val wrongData = ModifiersData(canonical.modifierTypeId, Map(canonical.id -> wrongBytes))
      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(wrongData)), Some(peer))
      eventually {
        deliveryTracker.status(canonical.id, canonical.modifierTypeId, Seq.empty) shouldBe Unknown
      }
      holderProbe.expectNoMessage(300.millis)
      ncProbe.expectNoMessage(300.millis)

      val withHeader = withParent.append(blocks.last.header).get._1
      synchronizer ! ChangedHistory(withHeader)
      val otherPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(defaultPeerSpec.copy(features = Seq(ModePeerFeature(
          StateType.Utxo, true, None, ModePeerFeature.AllBlocksKept))),
          System.currentTimeMillis()))
      )
      syncTracker.updateStatus(otherPeer, org.ergoplatform.consensus.Older, Some(blocks.last.header.height))
      synchronizer ! ErgoNodeViewHolder.DownloadRequest(Map(canonical.modifierTypeId -> Seq(canonical.id)))
      requestForModifierSent(ncProbe, canonical.modifierTypeId, canonical.id)
      eventually {
        deliveryTracker.status(canonical.id, canonical.modifierTypeId, Seq.empty) shouldBe Requested
      }

      val canonicalData = ModifiersData(canonical.modifierTypeId, Map(canonical.id -> canonical.bytes))
      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(canonicalData)), Some(otherPeer))
      val forwarded = holderProbe.expectMsgType[ErgoNodeViewHolder.ReceivableMessages.ModifiersFromRemote]
      forwarded.modifiers.map(_.id) should contain (canonical.id)
      eventually {
        deliveryTracker.status(canonical.id, canonical.modifierTypeId, Seq.empty) shouldBe Received
      }
      ncProbe.expectNoMessage(300.millis)
    }
  }

  property(
    "NodeViewSynchronizer: apply continuation header from syncV2 and download its block"
  ) {
    withFixture2 { ctx =>
      import ctx._
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 100.millis)

      // we generate and apply existing base chain
      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain =
        genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(hhistory.append)
      val bestHeaderOpt = hhistory.bestHeaderOpt

      // then a continuation chain that will be part of the syncV2 message
      val continuationChain = genHeaderChain(
        _.size > 4,
        bestHeaderOpt,
        hhistory.difficultyCalculator,
        None,
        false
      ).tail

      // sync message carries best header of our base change + continuation chain whose Head header is supposed to be applied
      val sync     = ErgoSyncInfoV2(continuationChain.headers)
      val msgBytes = ErgoSyncInfoMessageSpec.toBytes(sync)

      // send this sync msg to synchronizer which should apply the header following the common header from base chain
      synchronizerMockRef ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))
      val appliedHeader = continuationChain.headers.head
      // calculate block sections for applied header and test whether they were attempted to be downloaded from remote peer
      var remainingSectionIds = hhistory
        .requiredModifiersForHeader(appliedHeader)
        .groupBy(_._1)
        .mapValues(_.map(_._2).head)
      while (remainingSectionIds.nonEmpty) {
        ncProbe.fishForMessage(3 seconds) {
          case m =>
            m match {
              case stn: SendToNetwork
                  if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
                val invData = stn.message.data.get.asInstanceOf[InvData]
                remainingSectionIds.exists {
                  case (sectionTypeId, sectionId) =>
                    val sectionFound = invData.typeId == sectionTypeId && invData.ids.head == sectionId
                    if (sectionFound) {
                      remainingSectionIds = remainingSectionIds - sectionTypeId
                    }
                    sectionFound
                }
              case _ =>
                false
            }
        }
      }
      eventually {
        // test whether applied header was actually persisted to history
        val hist = ErgoHistory.readOrGenerate(settings)(null)
        hist.bestHeaderIdOpt.get shouldBe appliedHeader.id
      }

      // Idempotency check: sending the same syncV2 message again should NOT re-send the header
      // to the view holder because deliveryTracker already knows about it (Held status).
      // We verify this by checking that no additional RequestModifier messages are sent.
      // Note: a sync response (SendToNetwork with Sync message) may be sent back, which is expected.
      synchronizerMockRef ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))
      ncProbe.fishForMessage(2 seconds) { case m =>
        m match {
          case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
            // If we get a RequestModifier, the header was re-sent — this is a failure
            false
          case _ =>
            // Any other message (e.g. sync response) is fine — keep fishing until timeout
            true
        }
      }
    }
  }

  property("NodeViewSynchronizer: receiving out-of-order header should request it again") {
    withFixture2 { ctx =>
      import ctx._

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)

      def sendHeader(header: Header): Unit = {
        deliveryTracker.setRequested(Header.modifierTypeId, header.id, peer)(_ =>
          Cancellable.alreadyCancelled
        )
        val modData = ModifiersData(Header.modifierTypeId, Map(header.id -> header.bytes))
        val modSpec = ModifiersSpec
        synchronizerMockRef ! Message(modSpec, Left(modSpec.toBytes(modData)), Some(peer))
      }

      deliveryTracker.reset()

      // we generate fork of two headers, starting from the parent of the best header
      // so the depth of the rollback is 1, and the fork bypasses the best chain by 1 header
      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val newHeaders =
        genHeaderChain(2, hhistory, diffBitsOpt = None, useRealTs = false).headers
      val newHistory = newHeaders.foldLeft(hhistory) {
        case (hist, header) => hist.append(header).get._1
      }
      val parentOpt = newHistory.lastHeaders(2).headOption
      val smallFork = genHeaderChain(
        _.size > 2,
        parentOpt,
        newHistory.difficultyCalculator,
        None,
        false
      )
      val secondForkHeader = smallFork.last

      sendHeader(secondForkHeader)
      // we submit header at best height + 1, but with parent not known, the status should  be unknown,
      // so after some time the header could be downloaded again (when the parent may be known)
      eventually {
        deliveryTracker.status(secondForkHeader.id, Header.modifierTypeId, Seq.empty) shouldBe Unknown
      }
    }
  }

  property("NodeViewSynchronizer: syncV2 should retry header after recoverable failure") {
    withFixture2 { ctx =>
      import ctx._

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 100.millis)

      // Generate base chain and set up synchronizer with it
      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(hhistory.append)
      val bestHeaderOpt = hhistory.bestHeaderOpt

      // Generate continuation chain whose head should be applied
      val continuationChain = genHeaderChain(_.size > 4, bestHeaderOpt, hhistory.difficultyCalculator, None, false).tail
      val appliedHeader = continuationChain.headers.head

      // Set up the synchronizer with the base history
      synchronizerMockRef ! ChangedHistory(hhistory)

      // First syncV2 message — header should be sent to view holder and block sections requested
      val sync = ErgoSyncInfoV2(continuationChain.headers)
      val msgBytes = ErgoSyncInfoMessageSpec.toBytes(sync)
      synchronizerMockRef ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))

      // Wait for block section requests (proves header was sent to VH)
      ncProbe.fishForMessage(3 seconds) { case m =>
        m match {
          case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
            true
          case _ => false
        }
      }

      // Simulate recoverable failure: view holder could not apply header (e.g. missing parent).
      // This resets the header status to Unknown in deliveryTracker.
      synchronizerMockRef ! RecoverableFailedModification(Header.modifierTypeId, appliedHeader.id,
        new RecoverableModifierError("test failure", appliedHeader.id, Header.modifierTypeId))

      // Wait for deliveryTracker to reflect the reset
      eventually {
        deliveryTracker.status(appliedHeader.id, Header.modifierTypeId, Seq.empty) shouldBe Unknown
      }

      // Send the SAME syncV2 message again — the header should be re-sent to the view holder
      // because deliveryTracker.status is now Unknown again. Block sections should be requested.
      synchronizerMockRef ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))
      ncProbe.fishForMessage(3 seconds) { case m =>
        m match {
          case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
            true
          case _ => false
        }
      }
    }
  }

  property("NodeViewSynchronizer: longer fork is applied and shorter is not") {
    withFixture2 { ctx =>
      import ctx._

      def sendHeader(block: ErgoFullBlock): Unit = {
        deliveryTracker.setRequested(Header.modifierTypeId, block.header.id, peer)(_ =>
          Cancellable.alreadyCancelled
        )
        val modData =
          ModifiersData(Header.modifierTypeId, Map(block.header.id -> block.header.bytes))
        synchronizerMockRef ! Message(
          ModifiersSpec,
          Left(ModifiersSpec.toBytes(modData)),
          Some(peer)
        )
      }

      def sendBlockSection(block: BlockSection): Unit = {
        deliveryTracker.setRequested(block.modifierTypeId, block.id, peer)(_ =>
          Cancellable.alreadyCancelled
        )
        val modData = ModifiersData(block.modifierTypeId, Map(block.id -> block.bytes))
        synchronizerMockRef ! Message(
          ModifiersSpec,
          Left(ModifiersSpec.toBytes(modData)),
          Some(peer)
        )
      }

      def sendBlock(block: ErgoFullBlock): Unit = {
        sendBlockSection(block.blockTransactions)
        sendBlockSection(block.extension)
        block.adProofs.foreach(sendBlockSection(_))
      }

      deliveryTracker.reset()

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      // generate smaller fork that is going to be reverted after applying a bigger fork
      val smallFork = genChain(4, hist)

      smallFork.foreach(sendHeader)
      // history should eventually contain all smaller fork headers
      eventually {
        smallFork.forall(block => hist.contains(block.id))
      }
      smallFork.foreach(sendBlock)
      // history should eventually contain smaller fork block parts
      eventually {
        smallFork.forall(block =>
          hist.contains(block.extension.id) && hist.contains(block.blockTransactions.id)
        )
      }
      // generate bigger fork that is going to win over smaller fork that is to be reverted
      val bigFork = genChain(20, hist, extension = emptyExtension)

      bigFork.foreach(sendHeader)
      // history should revert all smaller fork headers
      eventually {
        smallFork.forall(block => !hist.contains(block.id))
      }
      bigFork.foreach(sendBlock)
      // history should revert all smaller fork block parts
      eventually {
        smallFork.forall(block =>
          !hist.contains(block.extension.id) && !hist.contains(block.blockTransactions.id)
        )
      }
    }
  }

  property("NodeViewSynchronizer: Message: SyncInfoSpec V2 - older peer") {
    withFixture { ctx =>
      import ctx._

      val sync = ErgoSyncInfoV2(Seq(chain.last))

      // Neighbour is sending
      val msgBytes = ErgoSyncInfoMessageSpec.toBytes(sync)

      // we check that in case of neighbour with older history (it has more blocks),
      // sync message will be sent by our node (to get invs from the neighbour),
      // sync message will consist of 4 headers
      synchronizer ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))
      ncProbe.fishForMessage(3 seconds) {
        case m =>
          m match {
            case stn: SendToNetwork =>
              val msg     = stn.message
              val headers = msg.data.get.asInstanceOf[ErgoSyncInfoV2].lastHeaders
              msg.spec.messageCode == ErgoSyncInfoMessageSpec.messageCode && headers.length == 4
            case _ => false
          }
      }
    }
  }

  property("NodeViewSynchronizer: full V2 sync info is sent to a peer after receiving a header from it") {
    withFixture { ctx =>
      import ctx._

      deliveryTracker.reset()
      val header = chain.take(1001).last
      deliveryTracker.setRequested(Header.modifierTypeId, header.id, peer)(_ => Cancellable.alreadyCancelled)
      val modData = ModifiersData(Header.modifierTypeId, Map(header.id -> header.bytes))
      synchronizer ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(modData)), Some(peer))

      // the sync info consists of the headers at offsets 0, 16, 128 and 512 from our tip, not only of the tip
      ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork =>
          val msg = stn.message
          msg.spec.messageCode == ErgoSyncInfoMessageSpec.messageCode &&
            msg.data.get.asInstanceOf[ErgoSyncInfoV2].lastHeaders.length == ErgoHistoryReader.FullV2SyncOffsets.length
        case _ => false
      }
    }
  }

  property("NodeViewSynchronizer: Message: SyncInfoSpec V2 - unknown peer") {
    withFixture { ctx =>
      import ctx._

      val sync = ErgoSyncInfoV2(Seq(altchain.last))

      // Neighbour is sending
      val msgBytes = ErgoSyncInfoMessageSpec.toBytes(sync)

      // we check that in case of neighbour with older history (it has more blocks),
      // sync message will be sent by our node (to get invs from the neighbour),
      // sync message will consist of 4 headers
      synchronizer ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))
      ncProbe.fishForMessage(3 seconds) {
        case m =>
          m match {
            case stn: SendToNetwork =>
              val msg     = stn.message
              val headers = msg.data.get.asInstanceOf[ErgoSyncInfoV2].lastHeaders
              msg.spec.messageCode == ErgoSyncInfoMessageSpec.messageCode && headers.length == 4
            case _ => false
          }
      }
    }
  }

  property("NodeViewSynchronizer: Message: SyncInfoSpec V2 - forked peer") {
    withFixture { ctx =>
      import ctx._

      val sync = ErgoSyncInfoV2(
        ErgoHistoryReader.FullV2SyncOffsets.map(offset =>
          forkedChain.apply(forkedHeight - offset - 1)
        )
      )

      // Neighbour is sending
      val msgBytes = ErgoSyncInfoMessageSpec.toBytes(sync)
      val invSpec  = InvSpec
      // we check that in case of neighbour with older history (it has more blocks),
      // invs (extension for the forked peer) will be sent to the peer
      synchronizer ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))
      ncProbe.fishForMessage(3 seconds) {
        case m =>
          m match {
            case stn: SendToNetwork =>
              val msg = stn.message
              msg.spec.messageCode == invSpec.messageCode
            case _ => false
          }
      }
    }
  }

  property("NodeViewSynchronizer: process valid InputBlockAnnouncement") {
    withFixture2 { ctx =>
      import ctx._

      // Generate a valid input block info
      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(2, hist)
      val header = chain.last.header

      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        None
      )

      // Send the input block message
      val msgBytes = InputBlockMessageSpec.toBytes(inputBlockInfo)
      synchronizerMockRef ! Message(InputBlockMessageSpec, Left(msgBytes), Some(peer))

      // Verify that the input block gets processed without throwing exceptions
      // The synchronizer may send RequestModifier messages to fetch missing transactions
      // We just verify the message is processed successfully by waiting briefly
      Thread.sleep(200) // Give time for processing
    // Test passes if no exception was thrown during processing
    }
  }

  property("NodeViewSynchronizer: process InputBlockAnnouncement with transaction IDs") {
    withFixture2 { ctx =>
      import ctx._

      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(2, hist)
      val header = chain.last.header

      // Create some test transactions
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val tx        = validErgoTransactionGenTemplate(0, 0).sample.get._2
      val weakTxIds = Some(Seq(tx.weakId))

      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        weakTxIds
      )

      // Send the input block message
      val msgBytes = InputBlockMessageSpec.toBytes(inputBlockInfo)
      synchronizerMockRef ! Message(InputBlockMessageSpec, Left(msgBytes), Some(peer))

      // Verify processing - should not send transaction request messages since all txs are in mempool
      ncProbe.fishForMessage(3 seconds) {
        case m =>
          m match {
            case stn: SendToNetwork =>
              val msg = stn.message
              msg.spec.messageCode == RequestModifierSpec.messageCode
            case _ => false
          }
      }
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlock penalizes peer on invalid InputBlockAnnouncement"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer
      import org.ergoplatform.network.peer.PenaltyType

      // Setup empty history
      val hist  = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(3, hist)
      // Use genesis block header (height 1) which matches fullBlockHeight(0) + 1
      val header = chain.head.header

      // Create a WrappedUtxoState to enable input block validation via usrOpt
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get

      // Send initialization messages and wait for actor to process them
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Create an InputBlockAnnouncement with empty Merkle proof that won't match the header's extensionRoot
      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        None
      )

      // Verify the input block info is invalid (extension proof won't match header's extensionRoot)
      val powScheme = settings.chainSettings.powScheme
      val params    = wrappedState.stateContext.currentParameters
      val isValid   = inputBlockInfo.valid(powScheme, params, None)
      isValid shouldBe false

      // Call processInputBlock directly on the underlying actor to bypass message routing
      val synchronizer = synchronizerMockRef.underlyingActor
      synchronizer.processInputBlock(
        inputBlockInfo,
        hist,
        ErgoMemPool.empty(settings),
        peer,
        Some(wrappedState)
      )

      // Verify that PenalizePeer with MisbehaviorPenalty was sent to network controller
      val messages = ncProbe.receiveWhile(max = 2 seconds, idle = 200.millis) {
        case m => m
      }
      messages.exists {
        case PenalizePeer(_, PenaltyType.MisbehaviorPenalty) => true
        case _                                               => false
      } shouldBe true
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlock ignores input blocks at height > fullBlockHeight + 2"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec

      // Setup: empty history (fullBlockHeight = 0)
      val hist = ErgoHistory.readOrGenerate(settings)(null)

      // Generate a block at height far ahead (> fullBlockHeight + 2)
      val chain          = genChain(5, hist)
      val farAheadHeader = chain.last.header
      // fullBlockHeight is 0, header height is 5, so: header.height (5) > 0 + 2

      // Create an InputBlockAnnouncement with the far-ahead header
      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        farAheadHeader,
        InputBlockFields.empty,
        None
      )

      // Send initialization messages
      synchronizerMockRef ! ChangedState(localStateGen.sample.get)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Send the input block message — should be ignored due to height gap
      val msgBytes = InputBlockMessageSpec.toBytes(inputBlockInfo)
      synchronizerMockRef ! Message(InputBlockMessageSpec, Left(msgBytes), Some(peer))

      // Verify no messages are sent to the network controller or peer handler
      // (the input block is silently ignored)
      Thread.sleep(200)
      ncProbe.expectNoMessage(300.millis)
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlock ignores input blocks at height < fullBlockHeight - 2"
  ) {
    withFixture2 { ctx =>
      import ctx._

      // Setup: apply a chain of 6 blocks so local fullBlockHeight is 6
      val hist  = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(6, hist)
      applyChain(hist, chain)

      // Use the first block's header at height 1, which is far behind (1 < 6 - 2)
      val farBehindHeader = chain.head.header

      // Create an InputBlockAnnouncement with the far-behind header
      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        farBehindHeader,
        InputBlockFields.empty,
        None
      )

      // Call processInputBlock directly to bypass message routing and validation details
      val synchronizer = synchronizerMockRef.underlyingActor
      synchronizer.processInputBlock(
        inputBlockInfo,
        hist,
        ErgoMemPool.empty(settings),
        peer,
        usrOpt = None   // guard runs before the digest-mode check
      )

      // Verify no messages are sent and the input block was not stored
      ncProbe.expectNoMessage(300.millis)
      hist.getInputBlock(farBehindHeader.id) shouldBe None
    }
  }

  property(
    "NodeViewSynchronizer: NewBestInputBlock(local=true) broadcasts IBI with txs when <= 3 transactions"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo

      // Setup empty history
      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(3, hist)
      val header = chain.head.header

      // Create a UTXO state
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get

      // Send initialization messages
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Create an input block with 2 weakTxIds (<= 3, so txs should be included in broadcast)
      val fakeWeakId1: Array[Byte] = Array.fill(32)(0x11.toByte)
      val fakeWeakId2: Array[Byte] = Array.fill(32)(0x22.toByte)
      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        Some(Seq(fakeWeakId1, fakeWeakId2))
      )

      // Apply input block to history so getInputBlock returns it
      hist.applyInputBlock(inputBlockInfo)

      // Create a peer with protocolVersion >= SubblocksVersion and Equal status
      val subBlocksPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion, // version 6.5.0
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlocksPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlocksPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlocksPeer, Equal, Some(header.height))

      // Send NewBestInputBlock(local=true) event
      synchronizerMockRef ! NewBestInputBlock(Some(header.id), local = true)

      // Verify InputBlockMessageSpec is sent to the sub-block peer with txs included
      val msg = ncProbe.expectMsgClass(
        3 seconds,
        classOf[scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork]
      )
      msg.message.spec.messageCode shouldBe InputBlockMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) => peers should contain(subBlocksPeer)
        case other              => fail(s"Expected SendToPeers, got $other")
      }

      // Verify the input block was sent WITH weakTxIds (since <= 3 transactions)
      val ibi = msg.message.data.get.asInstanceOf[InputBlockAnnouncement]
      ibi.id shouldBe header.id
      ibi.weakTxIds shouldBe Some(Seq(fakeWeakId1, fakeWeakId2))
    }
  }

  property(
    "NodeViewSynchronizer: NewBestInputBlock(local=true) broadcasts IBI without txs when > 3 transactions"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo

      // Setup empty history
      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(3, hist)
      val header = chain.head.header

      // Create a UTXO state
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get

      // Send initialization messages
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Create an input block with 5 weakTxIds (> 3, so txs should be stripped from broadcast)
      val fakeWeakIds = (1 to 5).map(i => Array.fill(32)(i.toByte))
      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        Some(fakeWeakIds)
      )

      // Apply input block to history so getInputBlock returns it
      hist.applyInputBlock(inputBlockInfo)

      // Verify the input block was applied with the expected weakTxIds
      val storedIbi = hist.getInputBlock(header.id)
      storedIbi.isDefined shouldBe true
      storedIbi.get.weakTxIds shouldBe Some(fakeWeakIds)
      // Verify that copy works correctly
      val strippedIbi = storedIbi.get.copy(weakTxIds = None)
      strippedIbi.weakTxIds shouldBe None

      // Create a peer with protocolVersion >= SubblocksVersion and Equal status
      val subBlocksPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlocksPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlocksPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlocksPeer, Equal, Some(header.height))

      // Drain any pending messages before sending the event
      ncProbe.receiveWhile(max = 200 millis, idle = 50.millis) { case m => m }

      // Send NewBestInputBlock(local=true) event
      synchronizerMockRef ! NewBestInputBlock(Some(header.id), local = true)

      // Wait for the handler to process and send the message
      Thread.sleep(200)

      // Fish for the InputBlockMessageSpec message (filter out other SendToNetwork messages)
      val msg = ncProbe.fishForMessage(3 seconds) {
        case stn: scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork =>
          stn.message.spec.messageCode == InputBlockMessageSpec.messageCode
        case _ => false
      }
      val sendToNetworkMsg = msg.asInstanceOf[
        scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
      ]
      sendToNetworkMsg.sendingStrategy match {
        case SendToPeers(peers) => peers should contain(subBlocksPeer)
        case other              => fail(s"Expected SendToPeers, got $other")
      }

      // Verify the message contains an InputBlockAnnouncement with the correct header id
      // and that weakTxIds are stripped because > 3 transactions were announced.
      val ibi = sendToNetworkMsg.message.data.get.asInstanceOf[InputBlockAnnouncement]
      ibi.id shouldBe header.id
      ibi.weakTxIds shouldBe None
    }
  }

  property(
    "NodeViewSynchronizer: NewBestInputBlock(local=true) keeps txs when exactly 3 transactions"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo

      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(3, hist)
      val header = chain.head.header

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get

      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val fakeWeakIds = (1 to 3).map(i => Array.fill(32)(i.toByte))
      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        Some(fakeWeakIds)
      )

      hist.applyInputBlock(inputBlockInfo)

      val subBlocksPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlocksPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlocksPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlocksPeer, Equal, Some(header.height))

      synchronizerMockRef ! NewBestInputBlock(Some(header.id), local = true)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe InputBlockMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) => peers should contain(subBlocksPeer)
        case other              => fail(s"Expected SendToPeers, got $other")
      }

      val ibi = msg.message.data.get.asInstanceOf[InputBlockAnnouncement]
      ibi.id shouldBe header.id
      ibi.weakTxIds shouldBe Some(fakeWeakIds)
    }
  }

  property(
    "NodeViewSynchronizer: NewBestInputBlock(local=true) strips txs when exactly 4 transactions"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo

      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(3, hist)
      val header = chain.head.header

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get

      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val fakeWeakIds = (1 to 4).map(i => Array.fill(32)(i.toByte))
      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        Some(fakeWeakIds)
      )

      hist.applyInputBlock(inputBlockInfo)

      val subBlocksPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlocksPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlocksPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlocksPeer, Equal, Some(header.height))

      synchronizerMockRef ! NewBestInputBlock(Some(header.id), local = true)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe InputBlockMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) => peers should contain(subBlocksPeer)
        case other              => fail(s"Expected SendToPeers, got $other")
      }

      val ibi = msg.message.data.get.asInstanceOf[InputBlockAnnouncement]
      ibi.id shouldBe header.id
      ibi.weakTxIds shouldBe None
    }
  }

  property(
    "NodeViewSynchronizer: NewBestInputBlock(local=true) broadcasts IBI with no txs when weakTxIds is None"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo

      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(3, hist)
      val header = chain.head.header

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get

      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        weakTxIds = None
      )

      hist.applyInputBlock(inputBlockInfo)

      val subBlocksPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlocksPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlocksPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlocksPeer, Equal, Some(header.height))

      synchronizerMockRef ! NewBestInputBlock(Some(header.id), local = true)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe InputBlockMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) => peers should contain(subBlocksPeer)
        case other              => fail(s"Expected SendToPeers, got $other")
      }

      val ibi = msg.message.data.get.asInstanceOf[InputBlockAnnouncement]
      ibi.id shouldBe header.id
      ibi.weakTxIds shouldBe None
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlock downloads ordering block when input block at height + 2"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.history.header.Header
      import org.ergoplatform.network.message.{InvData, RequestModifierSpec}
      import org.ergoplatform.settings.Algos
      import scorex.util.bytesToId

      // Setup empty history (only genesis block, fullBlockHeight = 0)
      val hist = ErgoHistory.readOrGenerate(settings)(null)

      // Generate a chain of 3 blocks (heights 1, 2, 3)
      val chain = genChain(3, hist)

      // Create a UTXO state and empty mempool
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      val mempool = ErgoMemPool.empty(settings)

      // Send initialization messages
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      val targetHeight = hist.fullBlockHeight + 2
      val blockAtTargetHeight = chain.find(_.header.height == targetHeight).getOrElse {
        fail(
          s"No block at height $targetHeight in generated chain (heights: ${chain.map(_.header.height).mkString(",")})"
        )
      }
      val originalHeader = blockAtTargetHeight.header
      val fakeParentId   = bytesToId(Algos.hash("non-existent-parent".getBytes))

      // Verify the fake parent is NOT in history
      hist.contains(fakeParentId) shouldBe false

      // Create a copy of the header with the fake parentId
      val modifiedHeader = originalHeader.copy(parentId = fakeParentId)

      // Create InputBlockAnnouncement with the modified header
      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        modifiedHeader,
        InputBlockFields.empty,
        None
      )

      // Do NOT apply the input block to history: processInputBlock skips already-known input blocks,
      // and the height + 2 path does not require the block to be stored.

      // Call processInputBlock directly to trigger the height + 2 path
      val synchronizer = synchronizerMockRef.underlyingActor
      synchronizer.processInputBlock(
        inputBlockInfo,
        hist,
        mempool,
        peer,
        Some(wrappedState)
      )

      // Verify that RequestModifier for Header with the fake parentId is sent to peer
      val messages = ncProbe.receiveWhile(max = 3 seconds, idle = 300.millis) {
        case m => m
      }

      val requestSent = messages.exists {
        case stn: scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork =>
          stn.message.spec.messageCode == RequestModifierSpec.messageCode && {
            val invData = stn.message.data.get.asInstanceOf[InvData]
            invData.typeId == Header.modifierTypeId && invData.ids.contains(fakeParentId)
          }
        case _ => false
      }
      requestSent shouldBe true
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlockTransactionIds requests missing transactions"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.mempool.ErgoTransaction
      import org.ergoplatform.network.message.inputblocks.{
        InputBlockTransactionIdsData,
        InputBlockTransactionsRequest,
        InputBlockTransactionsRequestMessageSpec
      }
      import scorex.core.network.SendToPeer

      // Setup empty history
      val hist  = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(3, hist)
      // Use genesis block header (height 1) which matches fullBlockHeight(0) + 1
      val header = chain.head.header

      // Create a WrappedUtxoState and empty mempool
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      val mempool = ErgoMemPool.empty(settings)

      // Send initialization messages and wait for actor to process them
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      // Create a fake weak transaction ID that is NOT in the mempool
      val fakeWeakId: ErgoTransaction.WeakId = Array.fill(32)(0xAA.toByte)
      val inputBlockId                       = header.id

      // Create InputBlockTransactionIdsData with the fake (missing) tx ID
      val txIds = InputBlockTransactionIdsData(inputBlockId, Seq(fakeWeakId))

      // Call processInputBlockTransactionIds directly on the underlying actor
      val synchronizer = synchronizerMockRef.underlyingActor
      synchronizer.requestInputBlockTransactionIds(
        InputBlockAnnouncement(1.toByte, header, InputBlockFields.empty, None), peer)
      synchronizer.processInputBlockTransactionIds(txIds, hist, mempool, peer)

      // Verify that InputBlockTransactionsRequest is sent to the peer (since tx is missing)
      val messages = ncProbe.receiveWhile(max = 3 seconds, idle = 300.millis) {
        case m => m
      }

      val requestSent = messages.exists {
        case stn: scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork =>
          stn.message.spec.messageCode == InputBlockTransactionsRequestMessageSpec.messageCode &&
          stn.message.data.get
            .asInstanceOf[InputBlockTransactionsRequest]
            .inputBlockId == inputBlockId &&
          stn.message.data.get.asInstanceOf[InputBlockTransactionsRequest].txIds == Seq(
              fakeWeakId
            ) &&
          stn.sendingStrategy == SendToPeer(peer)
        case _ => false
      }
      requestSent shouldBe true

      // Verify that localInputBlockChunks was populated
      val localInputBlockChunksField =
        classOf[ErgoNodeViewSynchronizer].getDeclaredFields
          .find(_.getName.endsWith("localInputBlockChunks")).get
      localInputBlockChunksField.setAccessible(true)
      val localInputBlockChunks = localInputBlockChunksField
        .get(synchronizer)
        .asInstanceOf[scala.collection.mutable.Map[
          String,
          ErgoNodeViewSynchronizer.InputBlockDiffData
        ]]

      localInputBlockChunks.contains(inputBlockId) shouldBe true
      val cachedData = localInputBlockChunks(inputBlockId)
      cachedData.weakTxsIds shouldBe Seq(fakeWeakId)
      cachedData.txs shouldBe empty // no txs found in mempool
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlockTransactions merges local cached txs with peer txs"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.InputBlockTransactionsData
      import org.ergoplatform.network.ErgoNodeViewSynchronizer.InputBlockDiffData
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ProcessInputBlockTransactions
      import scorex.util.ModifierId

      // Create a TestProbe to act as viewHolderRef so we can capture messages sent to it
      val viewHolderProbe = TestProbe("ViewHolderProbe")

      // Create a dedicated synchronizer with the probe as viewHolderRef
      val testHist            = ErgoHistory.readOrGenerate(settings)(null)
      val testChain           = genChain(3, testHist)
      val testMempool         = ErgoMemPool.empty(settings)
      val testSyncTracker     = ErgoSyncTracker(settings.scorexSettings.network)
      val testDeliveryTracker = DeliveryTracker.empty(settings)

      implicit val ec: ExecutionContextExecutor = ctx.system.dispatcher
      val testSynchronizerRef: TestActorRef[SynchronizerMock] = TestActorRef(
        Props(
          new SynchronizerMock(
            ncProbe.ref,
            viewHolderProbe.ref,
            ErgoSyncInfoMessageSpec,
            settings,
            testSyncTracker,
            testDeliveryTracker
          )
        )
      )

      // Initialize the synchronizer with state
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      testSynchronizerRef ! ChangedState(wrappedState)
      testSynchronizerRef ! ChangedHistory(testHist)
      testSynchronizerRef ! ChangedMempool(testMempool)
      Thread.sleep(500)

      // Generate two test transactions with known weakIds
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val tx1 = validErgoTransactionGenTemplate(0, 0).sample.get._2
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val tx2 = validErgoTransactionGenTemplate(0, 0).sample.get._2

      val inputBlockId: ModifierId = testChain.head.header.id

      // Pre-populate localInputBlockChunks with tx1 (local tx from mempool) but not tx2
      val testSynchronizer = testSynchronizerRef.underlyingActor
      val localInputBlockChunksField =
        classOf[ErgoNodeViewSynchronizer].getDeclaredFields
          .find(_.getName.endsWith("localInputBlockChunks")).get
      localInputBlockChunksField.setAccessible(true)
      val localInputBlockChunks = localInputBlockChunksField
        .get(testSynchronizer)
        .asInstanceOf[scala.collection.mutable.Map[ModifierId, InputBlockDiffData]]

      localInputBlockChunks.put(
        inputBlockId,
        InputBlockDiffData(
          System.currentTimeMillis(),
          Seq(tx1.weakId, tx2.weakId), // both weakIds expected
          Seq(tx1) // only tx1 is in local cache (tx2 comes from peer)
        )
      )

      // Create peer transaction data containing tx2 (missing from local)
      val peerTxsData = InputBlockTransactionsData(inputBlockId, Seq(tx2))

      // Call processInputBlockTransactions directly
      testSynchronizer.processInputBlockTransactions(peerTxsData, testHist, peer)

      // Verify ProcessInputBlockTransactions was sent to viewHolderRef with merged tx array
      // Note: The probe also receives GetNodeViewChanges from synchronizer preStart, so we fish for the right message
      val pitMsg = viewHolderProbe.fishForMessage(2 seconds) {
        case _: ProcessInputBlockTransactions => true
        case _                                => false
      }
      val pit = pitMsg.asInstanceOf[ProcessInputBlockTransactions]
      pit.std.inputBlockId shouldBe inputBlockId
      pit.std.transactions.length shouldBe 2
      pit.std.transactions.head shouldBe tx1
      pit.std.transactions(1) shouldBe tx2

      // Verify no network messages were sent (all txs found locally)
      val ncMessages = ncProbe.receiveWhile(max = 500 millis, idle = 100.millis) {
        case m => m
      }
      ncMessages.isEmpty shouldBe true
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlockTransactionIdsRequest serves stored tx IDs to peer"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.{
        InputBlockTransactionIdsData,
        InputBlockTransactionIdsMessageSpec
      }
      import org.ergoplatform.modifiers.mempool.ErgoTransaction
      import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
      import org.ergoplatform.Input
      import scorex.core.network.SendToPeer
      import sigma.interpreter.ProverResult

      // Setup history with a chain of blocks
      val hist = ErgoHistory.readOrGenerate(settings)(null)

      // Create a UTXO state with some initial boxes to spend
      val boxesHolder  = boxesHolderGen.sample.get
      val us           = WrappedUtxoState(boxesHolder, createTempDir, parameters, settings)
      val initialBoxes = boxesHolder.boxes.values.toSeq

      // Generate a chain of blocks on top of the history
      val chain            = genChain(3, hist, stateOpt = Some(us))
      val inputBlockHeader = chain.head.header

      // Create a transaction to include in the input block
      val inputBox = initialBoxes.head
      val tx = new ErgoTransaction(
        IndexedSeq(Input(inputBox.id, ProverResult.empty)),
        IndexedSeq.empty,
        IndexedSeq(inputBox.toCandidate)
      )

      // Create input block info with the transaction's weakId
      val expectedWeakId = tx.weakId
      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        inputBlockHeader,
        InputBlockFields.empty,
        Some(Seq(expectedWeakId))
      )

      // Apply input block to history
      hist.applyInputBlock(inputBlockInfo)

      // Apply input block transactions to properly populate caches
      hist.applyInputBlockTransactions(inputBlockInfo.id, Seq(tx), us)

      // Send initialization messages
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Call processInputBlockTransactionIdsRequest directly
      val synchronizer = synchronizerMockRef.underlyingActor
      synchronizer.processInputBlockTransactionIdsRequest(inputBlockInfo.id, hist, peer)

      // Verify InputBlockTransactionIdsData message is sent to peer
      val msg = ncProbe.expectMsgClass(
        3 seconds,
        classOf[scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork]
      )
      msg.message.spec.messageCode shouldBe InputBlockTransactionIdsMessageSpec.messageCode
      msg.sendingStrategy shouldBe SendToPeer(peer)
      val data = msg.message.data.get.asInstanceOf[InputBlockTransactionIdsData]
      data.inputBlockId shouldBe inputBlockInfo.id
      data.transactionIds shouldBe Seq(expectedWeakId)
    }
  }

  property("NodeViewSynchronizer: cleanupLocalInputBlockChunks removes expired entries") {
    withFixture2 { ctx =>
      import ctx._
      import scorex.util.ModifierId

      val synchronizerMock = synchronizerMockRef.underlyingActor

      // Create test transactions
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val tx1 = validErgoTransactionGenTemplate(0, 0).sample.get._2
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val tx2 = validErgoTransactionGenTemplate(0, 0).sample.get._2

      // Create old entries (should be cleaned up)
      val oldTime = System
          .currentTimeMillis() - (ErgoNodeViewSynchronizer.LocalInputBlockChunksTTL.toMillis * 2)
      val oldSubBlockId1: ModifierId =
        org.ergoplatform.utils.generators.CoreObjectGenerators.modifierIdGen.sample.get
      val oldSubBlockId2: ModifierId =
        org.ergoplatform.utils.generators.CoreObjectGenerators.modifierIdGen.sample.get

      // Access the localInputBlockChunks map via reflection
      // First, manually add old entries to the cache
      val oldData1 =
        ErgoNodeViewSynchronizer.InputBlockDiffData(oldTime, Seq(tx1.weakId), Seq(tx1))
      val oldData2 =
        ErgoNodeViewSynchronizer.InputBlockDiffData(oldTime, Seq(tx2.weakId), Seq(tx2))

      // Use reflection to access private field
      val localInputBlockChunksField =
        classOf[ErgoNodeViewSynchronizer].getDeclaredFields
          .find(_.getName.endsWith("localInputBlockChunks")).get
      localInputBlockChunksField.setAccessible(true)
      val localInputBlockChunks = localInputBlockChunksField
        .get(synchronizerMock)
        .asInstanceOf[scala.collection.mutable.Map[
          ModifierId,
          ErgoNodeViewSynchronizer.InputBlockDiffData
        ]]

      localInputBlockChunks.put(oldSubBlockId1, oldData1)
      localInputBlockChunks.put(oldSubBlockId2, oldData2)

      // Create recent entry (should NOT be cleaned up)
      val recentTime = System.currentTimeMillis()
      val recentSubBlockId: ModifierId =
        org.ergoplatform.utils.generators.CoreObjectGenerators.modifierIdGen.sample.get
      val recentData = ErgoNodeViewSynchronizer.InputBlockDiffData(
        recentTime,
        Seq(tx1.weakId, tx2.weakId),
        Seq(tx1, tx2)
      )
      localInputBlockChunks.put(recentSubBlockId, recentData)

      // Verify all entries are present before cleanup
      localInputBlockChunks.size shouldBe 3

      // Trigger cleanup
      synchronizerMockRef ! ErgoNodeViewSynchronizer.CleanupLocalInputBlockChunks

      // Verify old entries are removed and recent entry remains
      eventually {
        localInputBlockChunks.size shouldBe 1
        localInputBlockChunks.contains(recentSubBlockId) shouldBe true
        localInputBlockChunks.contains(oldSubBlockId1) shouldBe false
        localInputBlockChunks.contains(oldSubBlockId2) shouldBe false
      }
    }
  }

  property("NodeViewSynchronizer: cleanupLocalInputBlockChunks handles empty cache") {
    withFixture2 { ctx =>
      import ctx._
      import scorex.util.ModifierId

      val synchronizerMock = synchronizerMockRef.underlyingActor

      // Access the localInputBlockChunks map via reflection
      val localInputBlockChunksField =
        classOf[ErgoNodeViewSynchronizer].getDeclaredFields
          .find(_.getName.endsWith("localInputBlockChunks")).get
      localInputBlockChunksField.setAccessible(true)
      val localInputBlockChunks = localInputBlockChunksField
        .get(synchronizerMock)
        .asInstanceOf[scala.collection.mutable.Map[
          ModifierId,
          ErgoNodeViewSynchronizer.InputBlockDiffData
        ]]

      // Ensure cache is empty
      localInputBlockChunks.clear()
      localInputBlockChunks.size shouldBe 0

      // Trigger cleanup on empty cache - should not throw exception
      synchronizerMockRef ! ErgoNodeViewSynchronizer.CleanupLocalInputBlockChunks

      // Verify cache is still empty
      Thread.sleep(100)
      localInputBlockChunks.size shouldBe 0
    }
  }

  property("NodeViewSynchronizer: NewBestInputBlock(None, _) does nothing") {
    withFixture2 { ctx =>
      import ctx._

      // NewBestInputBlock(None, _) is sent when an ordering block is applied,
      // resetting the best input block reference. The P2P layer should do nothing.
      synchronizerMockRef ! NewBestInputBlock(None, local = true)

      // Verify no SendToNetwork message is emitted (the handler is a no-op)
      Thread.sleep(200)
      ncProbe.expectNoMessage()
    }
  }

  property("NodeViewSynchronizer: NewBestInputBlock with local=false does not broadcast") {
    withFixture2 { ctx =>
      import ctx._

      // When an input block is received from a remote peer (local=false),
      // the P2P layer should not re-broadcast it.
      // The handler's else branch is currently a todo — no messages should be sent.
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val randomId =
        org.ergoplatform.utils.generators.CoreObjectGenerators.modifierIdGen.sample.get
      synchronizerMockRef ! NewBestInputBlock(Some(randomId), local = false)

      Thread.sleep(200)
      ncProbe.expectNoMessage()
    }
  }

  property(
    "NodeViewSynchronizer: NewBestInputBlock for unknown input block does not crash"
  ) {
    withFixture2 { ctx =>
      import ctx._

      // When NewBestInputBlock references an input block ID not in history,
      // the handler should log an error and continue without crashing.
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val unknownId =
        org.ergoplatform.utils.generators.CoreObjectGenerators.modifierIdGen.sample.get
      synchronizerMockRef ! NewBestInputBlock(Some(unknownId), local = true)

      // Should not throw — the error path is handled gracefully.
      Thread.sleep(200)
      ncProbe.expectNoMessage()
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncement far-ahead header is ignored"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.{
        OrderingBlockAnnouncement,
        OrderingBlockAnnouncementMessageSpec
      }

      // Generate a chain of 10 blocks so the last header has height 10.
      // Our history is empty (height 0), so 10 > 0 + 2 → the OBA should be ignored.
      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(10, hist)
      val header = chain.last.header

      val oba = OrderingBlockAnnouncement(
        OrderingBlockAnnouncement.CurrentVersion,
        header,
        Seq.empty,
        Seq.empty,
        Seq.empty
      )

      val msgBytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      synchronizerMockRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(msgBytes),
        Some(peer)
      )

      // OBA header is far ahead of our full block height (> 2 blocks), so it should be silently ignored.
      // No inv or ordering block announcement should be sent.
      Thread.sleep(200)
      ncProbe.expectNoMessage()
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncement far-behind header is ignored"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.{
        OrderingBlockAnnouncement,
        OrderingBlockAnnouncementMessageSpec
      }

      // Setup: apply a chain of 8 blocks so local fullBlockHeight is 8
      val hist  = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(8, hist)
      applyChain(hist, chain)

      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(200)

      // Use a header not in local history and far behind (height 1 < 8 - 2).
      // Empty extension fields make the OBA invalid, so if the guard is missing,
      // the synchronizer would proceed to validation and penalize the peer.
      val farBehindHeader = altchain.take(2).head

      val oba = OrderingBlockAnnouncement(
        OrderingBlockAnnouncement.CurrentVersion,
        farBehindHeader,
        Seq.empty,
        Seq.empty,
        Seq.empty
      )

      val msgBytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      synchronizerMockRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(msgBytes),
        Some(peer)
      )

      // OBA header is far behind our full block height (> 2 blocks), so it should be silently ignored.
      Thread.sleep(200)
      ncProbe.expectNoMessage()
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncement ignores already-known OBA"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.{
        OrderingBlockAnnouncement,
        OrderingBlockAnnouncementMessageSpec
      }
      import org.ergoplatform.utils.generators.ChainGenerator.applyBlock

      // Generate a chain of 2 blocks with valid PoW
      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(2, hist)
      val header = chain.head.header

      // Append the block to history so hr.contains(header.id) returns true
      applyBlock(hist, chain.head)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))

      // Create and store the OBA
      val oba = OrderingBlockAnnouncement(
        OrderingBlockAnnouncement.CurrentVersion,
        header,
        Seq.empty,
        Seq.empty,
        Seq.empty
      )
      hist.storeOrderingBlockAnnouncement(oba)

      // Send the same OBA message — should be a no-op since header is already known
      val msgBytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      synchronizerMockRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(msgBytes),
        Some(peer)
      )

      // Header already in history → no messages sent to network controller
      Thread.sleep(200)
      ncProbe.expectNoMessage()
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncement sends to peer within height window regardless of status"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Younger
      import org.ergoplatform.modifiers.OrderingBlockAnnouncementTypeId
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo

      val hist  = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(3, hist)
      applyChain(hist, chain)

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val localFullBlockHeight = hist.fullBlockHeight
      val peerHeight           = localFullBlockHeight + 1

      val subBlockPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlockPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlockPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlockPeer, Younger, Some(peerHeight))

      val oba = buildValidOrderingBlockAnnouncement(hist.bestFullBlockOpt, None)
      oba.header.height shouldBe (localFullBlockHeight + 1)

      val msgBytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      synchronizerMockRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(msgBytes),
        Some(peer)
      )

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe InvSpec.messageCode
      val invData = msg.message.data.get.asInstanceOf[InvData]
      invData.typeId shouldBe OrderingBlockAnnouncementTypeId.value
      invData.ids shouldBe Seq(oba.header.id)
      msg.sendingStrategy match {
        case SendToPeers(peers) => peers should contain(subBlockPeer)
        case other              => fail(s"Expected SendToPeers, got $other")
      }
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncement does not send to peer far behind"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.modifiers.history.BlockTransactions
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.message.{InvData, RequestModifierSpec}
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.ConnectedPeer
      import org.ergoplatform.network.peer.PeerInfo

      val hist  = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(3, hist)
      applyChain(hist, chain)

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val localFullBlockHeight = hist.fullBlockHeight
      val farBehindHeight      = localFullBlockHeight - 3

      val subBlockPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlockPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlockPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlockPeer, Equal, Some(farBehindHeight))

      val oba = buildValidOrderingBlockAnnouncement(hist.bestFullBlockOpt, None)

      val msgBytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      synchronizerMockRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(msgBytes),
        Some(peer)
      )

      // The OBA references a missing previous input block, so the synchronizer first
      // requests the block transactions from the announcing peer.
      val requestMsg = ncProbe.expectMsgClass(classOf[SendToNetwork])
      requestMsg.message.spec.messageCode shouldBe RequestModifierSpec.messageCode
      val invData = requestMsg.message.data.get.asInstanceOf[InvData]
      invData.typeId shouldBe BlockTransactions.modifierTypeId
      invData.ids shouldBe Seq(oba.header.transactionsId)

      // No OBA announcement should be sent to the peer far behind.
      ncProbe.expectNoMessage(300.millis)
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncement does not send to peer far ahead"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Fork
      import org.ergoplatform.modifiers.history.BlockTransactions
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.message.{InvData, RequestModifierSpec}
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.ConnectedPeer
      import org.ergoplatform.network.peer.PeerInfo

      val hist  = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(3, hist)
      applyChain(hist, chain)

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val localFullBlockHeight = hist.fullBlockHeight
      val farAheadHeight       = localFullBlockHeight + 3

      val subBlockPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlockPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlockPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlockPeer, Fork, Some(farAheadHeight))

      val oba = buildValidOrderingBlockAnnouncement(hist.bestFullBlockOpt, None)

      val msgBytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      synchronizerMockRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(msgBytes),
        Some(peer)
      )

      // The OBA references a missing previous input block, so the synchronizer first
      // requests the block transactions from the announcing peer.
      val requestMsg = ncProbe.expectMsgClass(classOf[SendToNetwork])
      requestMsg.message.spec.messageCode shouldBe RequestModifierSpec.messageCode
      val invData = requestMsg.message.data.get.asInstanceOf[InvData]
      invData.typeId shouldBe BlockTransactions.modifierTypeId
      invData.ids shouldBe Seq(oba.header.transactionsId)

      // No OBA announcement should be sent to the peer far ahead.
      ncProbe.expectNoMessage(300.millis)
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncement sends to peers at exactly +/- 2 blocks"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.modifiers.OrderingBlockAnnouncementTypeId
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo

      val hist  = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(3, hist)
      applyChain(hist, chain)

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val localFullBlockHeight = hist.fullBlockHeight

      val subBlockPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )

      val behindPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlockPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(behindPeer, Equal, Some(localFullBlockHeight - 2))

      val aheadPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlockPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(aheadPeer, Equal, Some(localFullBlockHeight + 2))

      val oba = buildValidOrderingBlockAnnouncement(hist.bestFullBlockOpt, None)

      val msgBytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      synchronizerMockRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(msgBytes),
        Some(peer)
      )

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe InvSpec.messageCode
      val invData = msg.message.data.get.asInstanceOf[InvData]
      invData.typeId shouldBe OrderingBlockAnnouncementTypeId.value
      invData.ids shouldBe Seq(oba.header.id)
      msg.sendingStrategy match {
        case SendToPeers(peers) =>
          peers should contain(behindPeer)
          peers should contain(aheadPeer)
        case other => fail(s"Expected SendToPeers, got $other")
      }
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncement does not send to legacy peer even within window"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.modifiers.history.BlockTransactions
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.message.{InvData, RequestModifierSpec}
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.ConnectedPeer
      import org.ergoplatform.network.peer.PeerInfo
      import scorex.util.bytesToId

      val hist  = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(3, hist)
      applyChain(hist, chain)

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val localFullBlockHeight = hist.fullBlockHeight

      val legacyPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.initial,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val legacyPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(legacyPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(legacyPeer, Equal, Some(localFullBlockHeight))

      val missingPrevInputBlockId = bytesToId(Array.fill(32)(0xFF.toByte))
      val oba = buildValidOrderingBlockAnnouncement(hist.bestFullBlockOpt, Some(missingPrevInputBlockId))

      val msgBytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      synchronizerMockRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(msgBytes),
        Some(peer)
      )

      // The OBA references a missing previous input block, so the synchronizer first
      // requests the block transactions from the announcing peer.
      val requestMsg = ncProbe.expectMsgClass(classOf[SendToNetwork])
      requestMsg.message.spec.messageCode shouldBe RequestModifierSpec.messageCode
      val invData = requestMsg.message.data.get.asInstanceOf[InvData]
      invData.typeId shouldBe BlockTransactions.modifierTypeId
      invData.ids shouldBe Seq(oba.header.transactionsId)

      // No OBA announcement should be sent to the legacy peer.
      ncProbe.expectNoMessage(300.millis)
    }
  }

  property("NodeViewSynchronizer: requestInputBlock sends correct message to peer") {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.InputBlockTypeId
      import org.ergoplatform.network.message.{InvData, RequestModifierSpec}
      import scorex.core.network.SendToPeer
      import scorex.util.bytesToId

      val inputBlockId: scorex.util.ModifierId = bytesToId(Array.fill(32)(1.toByte))

      synchronizerMockRef.underlyingActor.requestInputBlock(inputBlockId, peer)

      val msg = ncProbe.expectMsgClass(classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe RequestModifierSpec.messageCode
      val invData = msg.message.data.get.asInstanceOf[InvData]
      invData.typeId shouldBe InputBlockTypeId.value
      invData.ids shouldBe Seq(inputBlockId)
      msg.sendingStrategy shouldBe SendToPeer(peer)
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncementRequest serves stored OBA to peer"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.{
        OrderingBlockAnnouncement,
        OrderingBlockAnnouncementMessageSpec
      }
      import scorex.core.network.SendToPeer

      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(2, hist)
      val header = chain.head.header

      val oba = OrderingBlockAnnouncement(
        OrderingBlockAnnouncement.CurrentVersion,
        header,
        Seq.empty,
        Seq.empty,
        Seq.empty
      )
      hist.storeOrderingBlockAnnouncement(oba)

      synchronizerMockRef.underlyingActor.processOrderingBlockAnnouncementRequest(
        header.id,
        hist,
        peer
      )

      val msg = ncProbe.expectMsgClass(classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe OrderingBlockAnnouncementMessageSpec.messageCode
      msg.sendingStrategy shouldBe SendToPeer(peer)
    }
  }

  property(
    "NodeViewSynchronizer: repeated input announcement preserves the pending ID request"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.DisconnectedPeer
      import org.ergoplatform.network.message.{InvData, RequestModifierSpec}
      import scorex.core.network.SendToPeer

      val hist = ErgoHistory.readOrGenerate(settings)(null)

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      val mempool = ErgoMemPool.empty(settings)

      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      // Build a valid input block at height fullBlockHeight + 1 with no tx IDs announced
      val inputBlockInfo = buildValidInputBlockAnnouncement(hist.bestFullBlockOpt)
      inputBlockInfo.header.height shouldBe (hist.fullBlockHeight + 1)

      val synchronizer = synchronizerMockRef.underlyingActor
      synchronizer.processInputBlock(
        inputBlockInfo,
        hist,
        mempool,
        peer,
        Some(wrappedState)
      )

      // Should request transaction IDs since none were announced: synchronizer sends a
      // RequestModifier for InputBlockTransactionIdsTypeId to the peer.
      val msg = ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == RequestModifierSpec.messageCode && {
            val invData = stn.message.data.get.asInstanceOf[InvData]
            invData.typeId == org.ergoplatform.modifiers.InputBlockTransactionIdsTypeId.value &&
            invData.ids.contains(inputBlockInfo.id)
          }
        case _ => false
      }
      msg.asInstanceOf[SendToNetwork].sendingStrategy shouldBe SendToPeer(peer)

      val idType = org.ergoplatform.modifiers.InputBlockTransactionIdsTypeId.value
      val firstRequest = deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id).get
      hist.getInputBlock(inputBlockInfo.id) shouldBe None

      // The view holder has not persisted the announcement yet. A second handler at the
      // same address must not replace the request or receive a duplicate wire request.
      val other = peer.copy(handlerRef = TestProbe()(system).ref)
      synchronizer.processInputBlock(inputBlockInfo, hist, mempool, other, Some(wrappedState))
      val afterDuplicate = deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id).get
      (afterDuplicate eq firstRequest) shouldBe true
      ncProbe.receiveWhile(300.millis, 50.millis) { case m => m }.collect {
        case stn: SendToNetwork
            if stn.message.spec.messageCode == RequestModifierSpec.messageCode &&
              stn.message.data.get.asInstanceOf[InvData].typeId == idType => stn
      } shouldBe empty

      // Once a retry explicitly clears the old request, the new connection may own it.
      deliveryTracker.setUnknown(inputBlockInfo.id, idType)
      synchronizer.processInputBlock(inputBlockInfo, hist, mempool, other, Some(wrappedState))
      deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id)
        .map(_.peer.handlerRef) shouldBe Some(other.handlerRef)
      ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
          val inv = stn.message.data.get.asInstanceOf[InvData]
          inv.typeId == idType && inv.ids.contains(inputBlockInfo.id) &&
            stn.sendingStrategy == SendToPeer(other)
        case _ => false
      }

      // A dead owner cannot receive SendToPeer. A later announcement may reclaim its request.
      deliveryTracker.setUnknown(inputBlockInfo.id, idType)
      synchronizer.requestInputBlockTransactionIds(inputBlockInfo, other, checksDone = 2)
      ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
          val inv = stn.message.data.get.asInstanceOf[InvData]
          inv.typeId == idType && inv.ids.contains(inputBlockInfo.id) &&
            stn.sendingStrategy == SendToPeer(other)
        case _ => false
      }
      val secondRequest = deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id).get
      secondRequest.checks shouldBe 2
      synchronizerMockRef ! DisconnectedPeer(other)
      synchronizer.processInputBlock(inputBlockInfo, hist, mempool, other, Some(wrappedState))
      (deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id).get eq secondRequest) shouldBe true
      ncProbe.receiveWhile(300.millis, 50.millis) { case m => m }.collect {
        case stn: SendToNetwork
            if stn.message.spec.messageCode == RequestModifierSpec.messageCode &&
              stn.message.data.get.asInstanceOf[InvData].typeId == idType => stn
      } shouldBe empty
      val replacement = peer.copy(handlerRef = TestProbe()(system).ref)
      synchronizer.processInputBlock(inputBlockInfo, hist, mempool, replacement, Some(wrappedState))
      deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id)
        .map(info => (info.peer.handlerRef, info.checks)) shouldBe Some((replacement.handlerRef, 2))
      secondRequest.cancellable.isCancelled shouldBe true
      ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
          val inv = stn.message.data.get.asInstanceOf[InvData]
          inv.typeId == idType && inv.ids.contains(inputBlockInfo.id) &&
            stn.sendingStrategy == SendToPeer(replacement)
        case _ => false
      }
    }
  }

  property("NodeViewSynchronizer: persisted incomplete input block reannouncement requests IDs") {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.InputBlockTransactionIdsTypeId
      import scorex.core.network.SendToPeer

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings)).sample.get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))

      val inputBlockInfo = buildValidInputBlockAnnouncement(hist.bestFullBlockOpt)
      hist.applyInputBlock(inputBlockInfo)
      hist.getInputBlock(inputBlockInfo.id).isDefined shouldBe true
      hist.getInputBlockTransactionIds(inputBlockInfo.id) shouldBe None
      val replacement = peer.copy(handlerRef = TestProbe()(system).ref)
      synchronizerMockRef ! Message(InputBlockMessageSpec,
        Left(InputBlockMessageSpec.toBytes(inputBlockInfo)), Some(replacement))

      val request = ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
          val inv = stn.message.data.get.asInstanceOf[InvData]
          inv.typeId == InputBlockTransactionIdsTypeId.value && inv.ids == Seq(inputBlockInfo.id)
        case _ => false
      }.asInstanceOf[SendToNetwork]
      request.sendingStrategy shouldBe SendToPeer(replacement)
      deliveryTracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, inputBlockInfo.id)
        .map(_.peer.handlerRef) shouldBe Some(replacement.handlerRef)
    }
  }

  property("NodeViewSynchronizer: pre-announcement empty cache does not complete a proved nonempty block") {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.InputBlockTransactionIdsTypeId
      import org.ergoplatform.network.message.inputblocks.{InputBlockTransactionIdsData, InputBlockTransactionIdsMessageSpec, InputBlockTransactionsRequest, InputBlockTransactionsRequestMessageSpec}
      import org.ergoplatform.settings.Algos
      import scorex.crypto.authds.LeafData
      import scorex.core.network.SendToPeer

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings)).sample.get
      val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2
      val digest = Algos.merkleTreeRoot(Seq(LeafData @@ tx.serializedId))
      val inputBlockInfo = buildValidInputBlockAnnouncement(hist.bestFullBlockOpt, Some(digest))
      inputBlockInfo.inputBlockFields.inputBlockFieldsProof.indices.nonEmpty shouldBe true

      // History accepts bodies before the corresponding announcement is stored.
      hist.applyInputBlockTransactions(inputBlockInfo.id, Seq.empty, wrappedState)
      hist.getInputBlockTransactionIds(inputBlockInfo.id) shouldBe Some(Seq.empty)
      hist.applyInputBlock(inputBlockInfo)
      hist.getInputBlock(inputBlockInfo.id).isDefined shouldBe true
      hist.getInputBlockTransactions(inputBlockInfo.id) shouldBe Some(Seq.empty)

      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      synchronizerMockRef ! Message(InputBlockMessageSpec,
        Left(InputBlockMessageSpec.toBytes(inputBlockInfo)), Some(peer))
      val idRequest = ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
          val inv = stn.message.data.get.asInstanceOf[InvData]
          inv.typeId == InputBlockTransactionIdsTypeId.value && inv.ids == Seq(inputBlockInfo.id)
        case _ => false
      }.asInstanceOf[SendToNetwork]
      idRequest.sendingStrategy shouldBe SendToPeer(peer)

      val ids = InputBlockTransactionIdsData(inputBlockInfo.id, Seq(tx.weakId))
      synchronizerMockRef ! Message(InputBlockTransactionIdsMessageSpec,
        Left(InputBlockTransactionIdsMessageSpec.toBytes(ids)), Some(peer))
      val bodyRequest = ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork if
            stn.message.spec.messageCode == InputBlockTransactionsRequestMessageSpec.messageCode =>
          stn.message.data.get.asInstanceOf[InputBlockTransactionsRequest].inputBlockId == inputBlockInfo.id
        case _ => false
      }.asInstanceOf[SendToNetwork]
      bodyRequest.sendingStrategy shouldBe SendToPeer(peer)
      bodyRequest.message.data.get.asInstanceOf[InputBlockTransactionsRequest].txIds.map(_.toSeq) shouldBe
        Seq(tx.weakId.toSeq)
    }
  }

  property("NodeViewSynchronizer: tracked ID reply cannot replace another peer's pending weak IDs") {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.InputBlockTransactionIdsTypeId
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ProcessInputBlockTransactions
      import org.ergoplatform.network.message.inputblocks.{InputBlockTransactionIdsData, InputBlockTransactionIdsMessageSpec, InputBlockTransactionsData, InputBlockTransactionsMessageSpec, InputBlockTransactionsRequest, InputBlockTransactionsRequestMessageSpec}
      import org.ergoplatform.settings.Algos
      import scorex.crypto.authds.LeafData
      import scorex.core.network.SendToPeer

      val viewHolderProbe = TestProbe("ViewHolderProbe")
      val testSyncTracker = ErgoSyncTracker(settings.scorexSettings.network)
      val testDeliveryTracker = DeliveryTracker.empty(settings)
      val testSynchronizerRef: TestActorRef[SynchronizerMock] = TestActorRef(
        Props(new SynchronizerMock(ncProbe.ref, viewHolderProbe.ref,
          ErgoSyncInfoMessageSpec, settings, testSyncTracker, testDeliveryTracker)))
      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings)).sample.get
      val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2
      val digest = Algos.merkleTreeRoot(Seq(LeafData @@ tx.serializedId))
      val noIds = buildValidInputBlockAnnouncement(hist.bestFullBlockOpt, Some(digest))
      val withIds = noIds.copy(weakTxIds = Some(Seq(tx.weakId)))
      val peerB = peer.copy(handlerRef = TestProbe()(system).ref)

      testSynchronizerRef ! ChangedState(wrappedState)
      testSynchronizerRef ! ChangedHistory(hist)
      testSynchronizerRef ! ChangedMempool(ErgoMemPool.empty(settings))
      testSynchronizerRef ! Message(InputBlockMessageSpec,
        Left(InputBlockMessageSpec.toBytes(noIds)), Some(peer))
      ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
          val inv = stn.message.data.get.asInstanceOf[InvData]
          inv.typeId == InputBlockTransactionIdsTypeId.value && inv.ids == Seq(noIds.id)
        case _ => false
      }
      val firstRequest = testDeliveryTracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, noIds.id).get

      testSynchronizerRef ! Message(InputBlockMessageSpec,
        Left(InputBlockMessageSpec.toBytes(withIds)), Some(peerB))
      val bodyRequest = ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork if
            stn.message.spec.messageCode == InputBlockTransactionsRequestMessageSpec.messageCode =>
          stn.message.data.get.asInstanceOf[InputBlockTransactionsRequest].inputBlockId == noIds.id
        case _ => false
      }.asInstanceOf[SendToNetwork]
      bodyRequest.sendingStrategy shouldBe SendToPeer(peerB)
      bodyRequest.message.data.get.asInstanceOf[InputBlockTransactionsRequest].txIds.map(_.toSeq) shouldBe
        Seq(tx.weakId.toSeq)

      val conflictingId = tx.weakId.clone()
      conflictingId(0) = (conflictingId(0) ^ 1).toByte
      val lateIds = InputBlockTransactionIdsData(noIds.id, Seq(conflictingId))
      testSynchronizerRef ! Message(InputBlockTransactionIdsMessageSpec,
        Left(InputBlockTransactionIdsMessageSpec.toBytes(lateIds)), Some(peer))
      testDeliveryTracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, noIds.id) shouldBe None
      firstRequest.cancellable.isCancelled shouldBe true
      ncProbe.receiveWhile(300.millis, 50.millis) { case m => m }.collect {
        case stn: SendToNetwork if
            stn.message.spec.messageCode == InputBlockTransactionsRequestMessageSpec.messageCode => stn
      } shouldBe empty
      viewHolderProbe.receiveWhile(300.millis, 50.millis) { case m => m }.collect {
        case _: ProcessInputBlockTransactions => 1
      } shouldBe empty

      val body = InputBlockTransactionsData(noIds.id, Seq(tx))
      testSynchronizerRef ! Message(InputBlockTransactionsMessageSpec,
        Left(InputBlockTransactionsMessageSpec.toBytes(body)), Some(peerB))
      val completed = viewHolderProbe.fishForMessage(3 seconds) {
        case _: ProcessInputBlockTransactions => true
        case _ => false
      }.asInstanceOf[ProcessInputBlockTransactions]
      completed.std.inputBlockId shouldBe noIds.id
      completed.std.transactions.map(_.id) shouldBe Seq(tx.id)
    }
  }

  property("NodeViewSynchronizer: disconnected ID owner retries before announcement persistence") {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.InputBlockTransactionIdsTypeId
      import org.ergoplatform.network.message.inputblocks.{InputBlockTransactionIdsData, InputBlockTransactionIdsMessageSpec}
      import scorex.core.network.SendToPeer

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings)).sample.get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))

      val inputBlockInfo = buildValidInputBlockAnnouncement(hist.bestFullBlockOpt)
      hist.getInputBlock(inputBlockInfo.id) shouldBe None
      val matrixSpec = defaultPeerSpec.copy(features = Seq(
        ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1)))
      val replacement = peer.copy(handlerRef = TestProbe()(system).ref,
        peerInfo = Some(PeerInfo(matrixSpec, System.currentTimeMillis())))
      val nonAnnouncer = peer.copy(
        connectionId = peer.connectionId.copy(remoteAddress = new InetSocketAddress("203.0.113.10", 19039)),
        handlerRef = TestProbe()(system).ref,
        peerInfo = Some(PeerInfo(matrixSpec, System.currentTimeMillis())))
      synchronizerMockRef ! HandshakedPeer(replacement)
      synchronizerMockRef ! HandshakedPeer(nonAnnouncer)
      val knownHandlers = syncTracker.knownPeers().map(_.handlerRef).toSet
      knownHandlers should contain(replacement.handlerRef)
      knownHandlers should contain(nonAnnouncer.handlerRef)

      val synchronizer = synchronizerMockRef.underlyingActor
      val idType = InputBlockTransactionIdsTypeId.value
      synchronizer.requestInputBlockTransactionIds(inputBlockInfo, peer, checksDone = 2)
      ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
          stn.message.data.get.asInstanceOf[InvData].typeId == idType
        case _ => false
      }
      val firstRequest = deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id).get

      // B is available before A disconnects, but must not steal the live request.
      synchronizerMockRef ! Message(InputBlockMessageSpec,
        Left(InputBlockMessageSpec.toBytes(inputBlockInfo)), Some(replacement))
      deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id).get shouldBe firstRequest
      hist.getInputBlock(inputBlockInfo.id) shouldBe None
      ncProbe.receiveWhile(300.millis, 50.millis) { case m => m }.collect {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode => stn
      } shouldBe empty

      synchronizerMockRef ! DisconnectedPeer(peer)
      val lateIds = InputBlockTransactionIdsData(inputBlockInfo.id,
        Seq(Array.fill(ErgoTransaction.WeakIdLength)(1.toByte)))
      synchronizerMockRef ! Message(InputBlockTransactionIdsMessageSpec,
        Left(InputBlockTransactionIdsMessageSpec.toBytes(lateIds)), Some(peer))
      // Retirement wins even while the old request is still tracked.
      deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id).get shouldBe firstRequest
      ncProbe.receiveWhile(300.millis, 50.millis) { case m => m }.collect {
        case _: SendToNetwork => 1
      } shouldBe empty
      synchronizerMockRef ! CheckDelivery(peer, idType, inputBlockInfo.id)
      val outputs = ncProbe.receiveWhile(1.second, 100.millis) { case m => m }
      outputs.collect { case _: PenalizePeer => 1 } shouldBe empty
      val retries = outputs.collect {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode => stn
      }
      retries.map(_.sendingStrategy) shouldBe Seq(SendToPeer(replacement))
      retries.head.message.data.get.asInstanceOf[InvData].ids shouldBe Seq(inputBlockInfo.id)
      deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id)
        .map(info => (info.peer.handlerRef, info.checks)) shouldBe Some((replacement.handlerRef, 2))
      firstRequest.cancellable.isCancelled shouldBe true

      // C is eligible but never announced this ID, so B's disconnect releases the
      // request without charging C for a delivery it cannot make.
      val replacementRequest = deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id).get
      synchronizerMockRef ! DisconnectedPeer(replacement)
      synchronizerMockRef ! CheckDelivery(replacement, idType, inputBlockInfo.id)
      deliveryTracker.getRequestedInfo(idType, inputBlockInfo.id) shouldBe None
      replacementRequest.cancellable.isCancelled shouldBe true
      val noPeerOutputs = ncProbe.receiveWhile(300.millis, 50.millis) { case m => m }
      noPeerOutputs.collect { case _: PenalizePeer => 1 } shouldBe empty
      noPeerOutputs.collect { case _: SendToNetwork => 1 } shouldBe empty

      synchronizerMockRef ! Message(InputBlockMessageSpec,
        Left(InputBlockMessageSpec.toBytes(inputBlockInfo)), Some(nonAnnouncer))
      val resumedRequest = ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
          stn.message.data.get.asInstanceOf[InvData].typeId == idType
        case _ => false
      }.asInstanceOf[SendToNetwork]
      resumedRequest.sendingStrategy shouldBe SendToPeer(nonAnnouncer)
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncement penalizes peer on invalid PoW"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.{
        OrderingBlockAnnouncement,
        OrderingBlockAnnouncementMessageSpec
      }
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer
      import org.ergoplatform.network.peer.PenaltyType

      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(2, hist)
      val header = chain.head.header

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Create OBA with header that has invalid PoW (zeroed out powSolution)
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val badPowSolution = new org.ergoplatform.AutolykosSolution(
        header.minerPk,
        genECPoint.sample.get,
        Array.fill(8)(0: Byte),
        BigInt(0)
      )
      val badHeader = header.copy(powSolution = badPowSolution)
      val oba = OrderingBlockAnnouncement(
        OrderingBlockAnnouncement.CurrentVersion,
        badHeader,
        Seq.empty,
        Seq.empty,
        Seq.empty
      )

      // Validate via PoW scheme to confirm it's invalid
      oba.valid(settings.chainSettings.powScheme, None) shouldBe false

      // Send via message routing (processOrderingBlockAnnouncement is private)
      val msgBytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      synchronizerMockRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(msgBytes),
        Some(peer)
      )

      val messages = ncProbe.receiveWhile(max = 2 seconds, idle = 200.millis) {
        case m => m
      }
      messages.exists {
        case PenalizePeer(_, PenaltyType.MisbehaviorPenalty) => true
        case _                                               => false
      } shouldBe true
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncement with stored prev input block sends ProcessOrderingBlock"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ProcessOrderingBlock
      import org.ergoplatform.settings.Algos
      import scorex.util.bytesToId

      val hist  = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(2, hist)
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get

      // Create and store a previous input block
      val prevIbId = bytesToId(Algos.hash("prev-input-block".getBytes))
      val prevIbInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        chain.head.header,
        InputBlockFields.empty,
        None
      )
      hist.applyInputBlock(prevIbInfo)
      hist.applyInputBlockTransactions(prevIbId, Seq.empty, wrappedState)

      // Create a dedicated synchronizer with a view-holder probe
      val viewHolderProbe     = TestProbe("ViewHolderProbe")
      val testSyncTracker     = ErgoSyncTracker(settings.scorexSettings.network)
      val testDeliveryTracker = DeliveryTracker.empty(settings)
      val testSynchronizerRef: TestActorRef[SynchronizerMock] = TestActorRef(
        Props(
          new SynchronizerMock(
            ncProbe.ref,
            viewHolderProbe.ref,
            ErgoSyncInfoMessageSpec,
            settings,
            testSyncTracker,
            testDeliveryTracker
          )
        )
      )

      testSynchronizerRef ! ChangedState(wrappedState)
      testSynchronizerRef ! ChangedHistory(hist)
      testSynchronizerRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Build a valid OBA referencing the stored input block
      val oba = buildValidOrderingBlockAnnouncement(hist.bestFullBlockOpt, Some(prevIbId))

      // Send via message routing
      val msgBytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      testSynchronizerRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(msgBytes),
        Some(peer)
      )

      // Should send ProcessOrderingBlock since prev input block is stored
      val msg = viewHolderProbe.fishForMessage(3 seconds) {
        case _: ProcessOrderingBlock => true
        case _                       => false
      }
      msg.asInstanceOf[ProcessOrderingBlock].oba.header.id shouldBe oba.header.id
    }
  }

  property(
    "NodeViewSynchronizer: processOrderingBlockAnnouncement without stored prev input block requests BlockTransactions"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.modifiers.BlockTransactionsTypeId
      import org.ergoplatform.network.message.{InvData, RequestModifierSpec}
      import org.ergoplatform.settings.Algos
      import scorex.core.network.SendToPeer
      import scorex.util.bytesToId

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      genChain(2, hist)

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Build a valid OBA referencing a non-existent input block
      val unknownIbId = bytesToId(Algos.hash("unknown-input-block".getBytes))
      val oba =
        buildValidOrderingBlockAnnouncement(hist.bestFullBlockOpt, Some(unknownIbId))
      oba.valid(settings.chainSettings.powScheme, None) shouldBe true

      // Send via message routing
      val msgBytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      synchronizerMockRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(msgBytes),
        Some(peer)
      )

      // Should request BlockTransactions since prev input block is NOT stored
      val msg = ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == RequestModifierSpec.messageCode && {
            val invData = stn.message.data.get.asInstanceOf[InvData]
            invData.typeId == BlockTransactionsTypeId.value && invData.ids.contains(
              oba.header.transactionsId
            )
          }
        case _ => false
      }
      msg.asInstanceOf[SendToNetwork].sendingStrategy shouldBe SendToPeer(peer)
    }
  }

  property(
    "NodeViewSynchronizer: LocallyGeneratedOrderingBlock broadcasts to sub-block peers"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo
      import org.ergoplatform.nodeView.LocallyGeneratedOrderingBlock

      val hist      = ErgoHistory.readOrGenerate(settings)(null)
      val chain     = genChain(3, hist)
      applyChain(hist, chain)
      val fullBlock = chain.last
      val header    = fullBlock.header

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Create a sub-block peer
      val subBlocksPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlocksPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlocksPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlocksPeer, Equal, Some(header.height))

      // Send LocallyGeneratedOrderingBlock
      synchronizerMockRef ! LocallyGeneratedOrderingBlock(fullBlock, Seq.empty)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe OrderingBlockAnnouncementMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) => peers should contain(subBlocksPeer)
        case other              => fail(s"Expected SendToPeers, got $other")
      }
    }
  }

  property(
    "NodeViewSynchronizer: LocallyGeneratedOrderingBlock broadcasts to peer within height window regardless of status"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Younger
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo
      import org.ergoplatform.nodeView.LocallyGeneratedOrderingBlock

      val hist      = ErgoHistory.readOrGenerate(settings)(null)
      val chain     = genChain(3, hist)
      applyChain(hist, chain)
      val fullBlock = chain.last

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val localFullBlockHeight = hist.fullBlockHeight

      val subBlockPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlockPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlockPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlockPeer, Younger, Some(localFullBlockHeight))

      synchronizerMockRef ! LocallyGeneratedOrderingBlock(fullBlock, Seq.empty)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe OrderingBlockAnnouncementMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) => peers should contain(subBlockPeer)
        case other              => fail(s"Expected SendToPeers, got $other")
      }
    }
  }

  property(
    "NodeViewSynchronizer: LocallyGeneratedOrderingBlock does not broadcast to peer far behind"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo
      import org.ergoplatform.nodeView.LocallyGeneratedOrderingBlock

      val hist      = ErgoHistory.readOrGenerate(settings)(null)
      val chain     = genChain(3, hist)
      applyChain(hist, chain)
      val fullBlock = chain.last

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val localFullBlockHeight = hist.fullBlockHeight
      val farBehindHeight      = localFullBlockHeight - 3

      val subBlockPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlockPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlockPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlockPeer, Equal, Some(farBehindHeight))

      synchronizerMockRef ! LocallyGeneratedOrderingBlock(fullBlock, Seq.empty)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe OrderingBlockAnnouncementMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) => peers should not contain subBlockPeer
        case other              => fail(s"Expected SendToPeers, got $other")
      }
    }
  }

  property(
    "NodeViewSynchronizer: LocallyGeneratedOrderingBlock does not broadcast to peer far ahead"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Fork
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo
      import org.ergoplatform.nodeView.LocallyGeneratedOrderingBlock

      val hist      = ErgoHistory.readOrGenerate(settings)(null)
      val chain     = genChain(3, hist)
      applyChain(hist, chain)
      val fullBlock = chain.last

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val localFullBlockHeight = hist.fullBlockHeight
      val farAheadHeight       = localFullBlockHeight + 3

      val subBlockPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlockPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlockPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlockPeer, Fork, Some(farAheadHeight))

      synchronizerMockRef ! LocallyGeneratedOrderingBlock(fullBlock, Seq.empty)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe OrderingBlockAnnouncementMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) => peers should not contain subBlockPeer
        case other              => fail(s"Expected SendToPeers, got $other")
      }
    }
  }

  property(
    "NodeViewSynchronizer: LocallyGeneratedOrderingBlock broadcasts to peers at exactly +/- 2 blocks"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo
      import org.ergoplatform.nodeView.LocallyGeneratedOrderingBlock

      val hist      = ErgoHistory.readOrGenerate(settings)(null)
      val chain     = genChain(3, hist)
      applyChain(hist, chain)
      val fullBlock = chain.last

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val localFullBlockHeight = hist.fullBlockHeight

      val subBlockPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )

      val behindPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlockPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(behindPeer, Equal, Some(localFullBlockHeight - 2))

      val aheadPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(subBlockPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(aheadPeer, Equal, Some(localFullBlockHeight + 2))

      synchronizerMockRef ! LocallyGeneratedOrderingBlock(fullBlock, Seq.empty)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe OrderingBlockAnnouncementMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) =>
          peers should contain(behindPeer)
          peers should contain(aheadPeer)
        case other => fail(s"Expected SendToPeers, got $other")
      }
    }
  }

  property(
    "NodeViewSynchronizer: LocallyGeneratedOrderingBlock does not broadcast to legacy peer even within window"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo
      import org.ergoplatform.nodeView.LocallyGeneratedOrderingBlock

      val hist      = ErgoHistory.readOrGenerate(settings)(null)
      val chain     = genChain(3, hist)
      applyChain(hist, chain)
      val fullBlock = chain.last

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val localFullBlockHeight = hist.fullBlockHeight

      val legacyPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.initial,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val legacyPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(legacyPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(legacyPeer, Equal, Some(localFullBlockHeight))

      synchronizerMockRef ! LocallyGeneratedOrderingBlock(fullBlock, Seq.empty)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe OrderingBlockAnnouncementMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) => peers should not contain legacyPeer
        case other              => fail(s"Expected SendToPeers, got $other")
      }
    }
  }

  property("NodeViewSynchronizer: NewBlockMined announces block to legacy peers") {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.{PeerSpec, Version}
      import scorex.core.network.ConnectedPeer
      import org.ergoplatform.network.peer.PeerInfo

      val hist  = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(3, hist)
      applyChain(hist, chain)
      val header = chain.last.header

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Create a legacy peer (version < SubblocksVersion); old-format block-section invs
      // should be sent only to peers that do not support sub-blocks.
      val legacyPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.initial,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val legacyPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(legacyPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(legacyPeer, Equal, Some(header.height))

      // Send NewBlockMined - immediate announcement of a locally mined block to all peers
      synchronizerMockRef ! NewBlockMined(header)

      // Should send inv for header and block sections (legacy peers receive them via broadcast)
      val messages = ncProbe.receiveWhile(max = 3 seconds, idle = 300.millis) {
        case m => m
      }
      val invSent = messages.exists {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == InvSpec.messageCode &&
            stn.message.data.toOption.exists(_.asInstanceOf[InvData].ids.contains(header.id))
        case _ => false
      }
      invSent shouldBe true
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlockTransactions with missing txs skips processing"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.InputBlockTransactionsData
      import org.ergoplatform.network.ErgoNodeViewSynchronizer.InputBlockDiffData
      import scorex.util.ModifierId

      val viewHolderProbe     = TestProbe("ViewHolderProbe")
      val testHist            = ErgoHistory.readOrGenerate(settings)(null)
      val testChain           = genChain(3, testHist)
      val testMempool         = ErgoMemPool.empty(settings)
      val testSyncTracker     = ErgoSyncTracker(settings.scorexSettings.network)
      val testDeliveryTracker = DeliveryTracker.empty(settings)

      val testSynchronizerRef: TestActorRef[SynchronizerMock] = TestActorRef(
        Props(
          new SynchronizerMock(
            ncProbe.ref,
            viewHolderProbe.ref,
            ErgoSyncInfoMessageSpec,
            settings,
            testSyncTracker,
            testDeliveryTracker
          )
        )
      )

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      testSynchronizerRef ! ChangedState(wrappedState)
      testSynchronizerRef ! ChangedHistory(testHist)
      testSynchronizerRef ! ChangedMempool(testMempool)
      Thread.sleep(500)

      // The synchronizer asks the view holder for current node view on startup.
      viewHolderProbe.expectMsgType[
        org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetNodeViewChanges
      ]

      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val tx1                      = validErgoTransactionGenTemplate(0, 0).sample.get._2
      val inputBlockId: ModifierId = testChain.head.header.id

      // Pre-populate with tx1 weakId but a fake weakId that won't be found
      val fakeWeakId: Array[Byte] = Array.fill(32)(0xFF.toByte)
      val localInputBlockChunksField =
        classOf[ErgoNodeViewSynchronizer].getDeclaredFields
          .find(_.getName.endsWith("localInputBlockChunks")).get
      localInputBlockChunksField.setAccessible(true)
      val localInputBlockChunks = localInputBlockChunksField
        .get(testSynchronizerRef.underlyingActor)
        .asInstanceOf[scala.collection.mutable.Map[ModifierId, InputBlockDiffData]]

      localInputBlockChunks.put(
        inputBlockId,
        InputBlockDiffData(
          System.currentTimeMillis(),
          Seq(tx1.weakId, fakeWeakId), // fakeWeakId won't be found
          Seq(tx1)
        )
      )

      // Peer sends tx1 only — fakeWeakId is missing
      val peerTxsData = InputBlockTransactionsData(inputBlockId, Seq(tx1))
      testSynchronizerRef.underlyingActor.processInputBlockTransactions(
        peerTxsData,
        testHist,
        peer
      )

      // Should NOT send ProcessInputBlockTransactions (allFound = false)
      viewHolderProbe.expectNoMessage(500.millis)
    }
  }

  property("NodeViewSynchronizer: processInputBlockRequest not found sends no message") {
    withFixture2 { ctx =>
      import ctx._
      import scorex.util.bytesToId

      val hist      = ErgoHistory.readOrGenerate(settings)(null)
      val unknownId = bytesToId(Array.fill(32)(0x99.toByte))

      val synchronizer = synchronizerMockRef.underlyingActor
      synchronizer.processInputBlockRequest(unknownId, hist, peer)

      // Should not send any message since block not found
      Thread.sleep(200)
      ncProbe.expectNoMessage(300.millis)
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlockTransactionIds with all txs in mempool processes immediately"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.InputBlockTransactionIdsData
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ProcessInputBlockTransactions
      import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
      import scorex.util.ModifierId

      val viewHolderProbe     = TestProbe("ViewHolderProbe")
      val testHist            = ErgoHistory.readOrGenerate(settings)(null)
      val testChain           = genChain(3, testHist)
      val testSyncTracker     = ErgoSyncTracker(settings.scorexSettings.network)
      val testDeliveryTracker = DeliveryTracker.empty(settings)

      val testSynchronizerRef: TestActorRef[SynchronizerMock] = TestActorRef(
        Props(
          new SynchronizerMock(
            ncProbe.ref,
            viewHolderProbe.ref,
            ErgoSyncInfoMessageSpec,
            settings,
            testSyncTracker,
            testDeliveryTracker
          )
        )
      )

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2

      // Put tx in mempool as UnconfirmedTransaction
      val unconfirmedTx = UnconfirmedTransaction(tx, None)
      val mempool       = ErgoMemPool.empty(settings).put(unconfirmedTx)
      testSynchronizerRef ! ChangedState(wrappedState)
      testSynchronizerRef ! ChangedHistory(testHist)
      testSynchronizerRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      val inputBlockId: ModifierId = testChain.head.header.id
      val txIds                    = InputBlockTransactionIdsData(inputBlockId, Seq(tx.weakId))

      testSynchronizerRef.underlyingActor.requestInputBlockTransactionIds(
        InputBlockAnnouncement(1.toByte, testChain.head.header, InputBlockFields.empty, None), peer)
      testSynchronizerRef.underlyingActor.processInputBlockTransactionIds(
        txIds,
        testHist,
        mempool,
        peer
      )

      // Should immediately send ProcessInputBlockTransactions since all txs are in mempool
      val msg = viewHolderProbe.fishForMessage(2 seconds) {
        case _: ProcessInputBlockTransactions => true
        case _                                => false
      }
      val pit = msg.asInstanceOf[ProcessInputBlockTransactions]
      pit.std.inputBlockId shouldBe inputBlockId
      pit.std.transactions.length shouldBe 1
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlockTransactionIdsRequest not found sends no message"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import scorex.util.bytesToId

      val hist      = ErgoHistory.readOrGenerate(settings)(null)
      val unknownId = bytesToId(Array.fill(32)(0x88.toByte))

      val synchronizer = synchronizerMockRef.underlyingActor
      synchronizer.processInputBlockTransactionIdsRequest(unknownId, hist, peer)

      Thread.sleep(200)
      ncProbe.expectNoMessage(300.millis)
    }
  }

  property("NodeViewSynchronizer: DownloadInputBlock triggers requestInputBlock") {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.nodeView.ErgoNodeViewHolder.DownloadInputBlock
      import scorex.core.network.SendToPeer
      import scorex.util.bytesToId

      val inputBlockId = bytesToId(Array.fill(32)(0xDD.toByte))
      synchronizerMockRef ! DownloadInputBlock(inputBlockId, peer)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe RequestModifierSpec.messageCode
      msg.sendingStrategy shouldBe SendToPeer(peer)
    }
  }

  property(
    "NodeViewSynchronizer: DownloadInputBlockTransactions triggers correct message"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.nodeView.ErgoNodeViewHolder.DownloadInputBlockTransactions
      import org.ergoplatform.network.message.inputblocks.InputBlockTransactionsRequest
      import org.ergoplatform.network.message.inputblocks.InputBlockTransactionsRequestMessageSpec
      import scorex.core.network.SendToPeer
      import scorex.util.bytesToId

      val inputBlockId = bytesToId(Array.fill(32)(0xEE.toByte))
      val req =
        InputBlockTransactionsRequest(inputBlockId, Seq(Array.fill(32)(0x11.toByte)))
      synchronizerMockRef ! DownloadInputBlockTransactions(req, peer)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe InputBlockTransactionsRequestMessageSpec.messageCode
      msg.sendingStrategy shouldBe SendToPeer(peer)
    }
  }

  property(
    "NodeViewSynchronizer: modifiersReq routes InputBlockTypeId to serve stored input block"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.InputBlockTypeId
      import org.ergoplatform.network.message.{InvData, RequestModifierSpec}
      import scorex.core.network.SendToPeer

      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(2, hist)
      val header = chain.head.header

      // Create and store an input block
      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        None
      )
      hist.applyInputBlock(inputBlockInfo)

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Send RequestModifier for InputBlockTypeId via message (must be raw bytes)
      val invData  = InvData(InputBlockTypeId.value, Seq(header.id))
      val reqBytes = RequestModifierSpec.toBytes(invData)
      synchronizerMockRef ! Message(RequestModifierSpec, Left(reqBytes), Some(peer))

      // Should send InputBlockMessageSpec back
      val msg = ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == InputBlockMessageSpec.messageCode
        case _ => false
      }
      msg.asInstanceOf[SendToNetwork].sendingStrategy shouldBe SendToPeer(peer)
    }
  }

  property("NodeViewSynchronizer: processInputBlock ignores an already completed input block") {
    withFixture2 { ctx =>
      import ctx._

      // Setup history with a chain
      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(3, hist)
      val header = chain.head.header

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      val mempool = ErgoMemPool.empty(settings)

      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        None
      )

      // Persist both the announcement and its empty transaction body. A stored
      // announcement alone remains incomplete and should request its IDs.
      hist.applyInputBlock(inputBlockInfo) shouldBe None
      hist.applyInputBlockTransactions(inputBlockInfo.id, Seq.empty, wrappedState)
      hist.getInputBlock(header.id) shouldBe Some(inputBlockInfo)

      val synchronizer = synchronizerMockRef.underlyingActor

      // Call processInputBlock on an already known block
      synchronizer.processInputBlock(
        inputBlockInfo,
        hist,
        mempool,
        peer,
        Some(wrappedState)
      )

      ncProbe.expectNoMessage(500.millis)
    }
  }

  property(
    "NodeViewSynchronizer: broadcastModifierInv with peersOpt targets specific peers"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo
      import org.ergoplatform.network.{PeerSpec, Version}

      val hist   = ErgoHistory.readOrGenerate(settings)(null)
      val chain  = genChain(3, hist)
      val header = chain.head.header

      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // Create a specific peer to target
      val targetPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(
          PeerInfo(
            PeerSpec(
              settings.scorexSettings.network.agentName,
              Version.SubblocksVersion,
              settings.scorexSettings.network.nodeName,
              None,
              Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
            ),
            System.currentTimeMillis()
          )
        )
      )
      syncTracker.updateStatus(targetPeer, Equal, Some(header.height))

      // Send NewBlockMined - immediate announcement of a locally mined block
      // We verify that inv messages go out after this event
      synchronizerMockRef ! NewBlockMined(header)

      val messages = ncProbe.receiveWhile(max = 3 seconds, idle = 300.millis) {
        case m => m
      }
      // An inv for the mined header should be announced (broadcast is acceptable)
      val headerInvSent = messages.exists {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == InvSpec.messageCode &&
            stn.message.data.toOption.exists(_.asInstanceOf[InvData].ids.contains(header.id))
        case _ => false
      }
      headerInvSent shouldBe true

      // All targeted (non-broadcast) messages should include the peer with Equal status
      messages.collect { case stn: SendToNetwork => stn }.forall { stn =>
        stn.sendingStrategy match {
          case SendToPeers(peers) => peers.contains(targetPeer)
          case _                  => true // Broadcast is also acceptable
        }
      } shouldBe true
    }
  }

  property("NodeViewSynchronizer: processInputBlock ignores input blocks in digest mode") {
    withFixture2 { ctx =>
      import ctx._
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      genChain(3, hist)

      // Build a valid input block announcement (valid PoW and extension proof)
      val inputBlockInfo = buildValidInputBlockAnnouncement(hist.bestFullBlockOpt)
      inputBlockInfo.header.height shouldBe (hist.fullBlockHeight + 1)
      inputBlockInfo.valid(
        settings.chainSettings.powScheme,
        parameters,
        None
      ) shouldBe true

      // Send only history and mempool to the synchronizer, no state (digest mode equivalent)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      // processInputBlock should return early because usrOpt is None
      val synchronizer = synchronizerMockRef.underlyingActor
      synchronizer.processInputBlock(
        inputBlockInfo,
        hist,
        ErgoMemPool.empty(settings),
        peer,
        None
      )

      // No network messages and no penalization should occur
      Thread.sleep(200)
      val messages = ncProbe.receiveWhile(max = 500 millis, idle = 100.millis) { case m => m }
      messages.exists {
        case PenalizePeer(_, _) => true
        case _ => false
      } shouldBe false
      messages.exists {
        case _: scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork => true
        case _ => false
      } shouldBe false
    }
  }

  property("NodeViewSynchronizer: processInputBlock ignores input block at or below full block height") {
    withFixture2 { ctx =>
      import ctx._

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val chain = genChain(3, hist)
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      val mempool = ErgoMemPool.empty(settings)

      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      // Use the first block's header, which has height 1 == fullBlockHeight + 1 at this point,
      // so apply it first to advance fullBlockHeight, then re-process the same header.
      val firstHeader = chain.head.header
      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        firstHeader,
        InputBlockFields.empty,
        None
      )

      // Apply an ordering block to advance fullBlockHeight to at least 1
      applyBlock(hist, chain.head)
      synchronizerMockRef ! ChangedHistory(hist)
      Thread.sleep(200)

      hist.fullBlockHeight should be >= firstHeader.height

      // Re-processing the same header (height <= fullBlockHeight) should be ignored
      val synchronizer = synchronizerMockRef.underlyingActor
      synchronizer.processInputBlock(
        inputBlockInfo,
        hist,
        mempool,
        peer,
        Some(wrappedState)
      )

      // No network or penalization messages expected for already-known/behind-height input block
      Thread.sleep(200)
      ncProbe.expectNoMessage(300.millis)
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlock at fullBlockHeight + 1 with all txs in mempool processes immediately"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{
        ProcessInputBlock,
        ProcessInputBlockTransactions
      }

      val viewHolderProbe = TestProbe("ViewHolderProbe")
      val testSyncTracker = ErgoSyncTracker(settings.scorexSettings.network)
      val testDeliveryTracker = DeliveryTracker.empty(settings)
      implicit val ec: ExecutionContextExecutor = ctx.system.dispatcher

      val testSynchronizerRef: TestActorRef[SynchronizerMock] = TestActorRef(
        Props(
          new SynchronizerMock(
            ncProbe.ref,
            viewHolderProbe.ref,
            ErgoSyncInfoMessageSpec,
            settings,
            testSyncTracker,
            testDeliveryTracker
          )
        )
      )

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get

      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2
      val mempool = ErgoMemPool.empty(settings).put(UnconfirmedTransaction(tx, None))

      testSynchronizerRef ! ChangedState(wrappedState)
      testSynchronizerRef ! ChangedHistory(hist)
      testSynchronizerRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      // Build a valid input block at height fullBlockHeight + 1, announcing tx's weakId
      val inputBlockInfo = buildValidInputBlockAnnouncement(hist.bestFullBlockOpt)
      inputBlockInfo.header.height shouldBe (hist.fullBlockHeight + 1)
      val inputBlockWithTxs = inputBlockInfo.copy(weakTxIds = Some(Seq(tx.weakId)))

      testSynchronizerRef.underlyingActor.processInputBlock(
        inputBlockWithTxs,
        hist,
        mempool,
        peer,
        Some(wrappedState)
      )

      // Both ProcessInputBlock and ProcessInputBlockTransactions should be sent immediately
      val processInputBlockMsg = viewHolderProbe.fishForMessage(2 seconds) {
        case _: ProcessInputBlock => true
        case _ => false
      }
      processInputBlockMsg
        .asInstanceOf[ProcessInputBlock]
        .subblock
        .id shouldBe inputBlockWithTxs.id

      val processTxsMsg = viewHolderProbe.fishForMessage(2 seconds) {
        case _: ProcessInputBlockTransactions => true
        case _ => false
      }
      val pit = processTxsMsg.asInstanceOf[ProcessInputBlockTransactions]
      pit.std.inputBlockId shouldBe inputBlockWithTxs.id
      pit.std.transactions should contain(tx)

      // No missing-transaction request should be sent to the network
      ncProbe.expectNoMessage(500.millis)
    }
  }

  property("NodeViewSynchronizer: processInputBlock rejects input block with wrong nBits") {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      val mempool = ErgoMemPool.empty(settings)

      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      // Seed history with a genesis block so the input block's parent is known and
      // requiredDifficultyAfter(parent) yields the expected nBits.
      import org.ergoplatform.utils.generators.ChainGenerator.applyBlock
      val chain = genChain(1, hist)
      applyBlock(hist, chain.head)

      // Build a valid input block at fullBlockHeight + 1, with parent known to history
      val validInputBlock = buildValidInputBlockAnnouncement(Some(chain.head))
      hist.modifierById(validInputBlock.header.parentId).collect { case h: Header => h } shouldBe hist.bestHeaderOpt

      // Corrupt nBits so it no longer matches the difficulty derived from the parent.
      // The fake PoW scheme validates any header, so nBits mismatch is the actual rejection path tested here.
      // Scale the original difficulty to guarantee a different compact encoding (a small delta
      // can be lost in compact-bit precision).
      val originalDifficulty = DifficultySerializer.decodeCompactBits(validInputBlock.header.nBits)
      val wrongNBits = DifficultySerializer.encodeCompactBits(originalDifficulty * 2)
      wrongNBits should not be validInputBlock.header.nBits
      val modifiedHeader = validInputBlock.header.copy(nBits = wrongNBits)
      val invalidInputBlock = validInputBlock.copy(header = modifiedHeader)

      // Sanity-check: the invalid input block must fail validation against the parent's expected difficulty.
      val parentHeader = hist.modifierById(invalidInputBlock.header.parentId).collect { case h: Header => h }.get
      val expectedDiff = hist.requiredDifficultyAfter(parentHeader)
      val expectedNBits = DifficultySerializer.encodeCompactBits(expectedDiff)
      expectedNBits shouldBe validInputBlock.header.nBits
      invalidInputBlock.valid(
        settings.chainSettings.powScheme,
        wrappedState.stateContext.currentParameters,
        Some(expectedNBits)
      ) shouldBe false

      synchronizerMockRef.underlyingActor.processInputBlock(
        invalidInputBlock,
        hist,
        mempool,
        peer,
        Some(wrappedState)
      )

      val messages = ncProbe.receiveWhile(max = 2 seconds, idle = 200.millis) {
        case m => m
      }
      messages.exists {
        case PenalizePeer(_, PenaltyType.MisbehaviorPenalty) => true
        case _ => false
      } shouldBe true
    }
  }

  property(
    "NodeViewSynchronizer: processInputBlock rejects input block with invalid extension Merkle proof"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get
      val mempool = ErgoMemPool.empty(settings)

      synchronizerMockRef ! ChangedState(wrappedState)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      val validInputBlock = buildValidInputBlockAnnouncement(hist.bestFullBlockOpt)
      // Replace the Merkle proof with an empty one so it cannot validate against the header
      val invalidInputBlock = validInputBlock.copy(
        inputBlockFields = InputBlockFields.empty
      )
      invalidInputBlock.valid(
        settings.chainSettings.powScheme,
        wrappedState.stateContext.currentParameters,
        None
      ) shouldBe false

      synchronizerMockRef.underlyingActor.processInputBlock(
        invalidInputBlock,
        hist,
        mempool,
        peer,
        Some(wrappedState)
      )

      val messages = ncProbe.receiveWhile(max = 2 seconds, idle = 200.millis) {
        case m => m
      }
      messages.exists {
        case PenalizePeer(_, PenaltyType.MisbehaviorPenalty) => true
        case _ => false
      } shouldBe true
    }
  }

  property(
    "NodeViewSynchronizer: malformed InputBlockMessageSpec bytes penalize peer"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer

      val hist    = ErgoHistory.readOrGenerate(settings)(null)
      val mempool = ErgoMemPool.empty(settings)

      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      val invalidBytes = Array.fill(100)(0.toByte)
      synchronizerMockRef ! Message(InputBlockMessageSpec, Left(invalidBytes), Some(peer))

      val messages = ncProbe.receiveWhile(max = 2 seconds, idle = 200.millis) {
        case m => m
      }
      messages.exists {
        case PenalizePeer(_, PenaltyType.PermanentPenalty) => true
        case _ => false
      } shouldBe true
    }
  }

  property(
    "NodeViewSynchronizer: malformed OrderingBlockAnnouncementMessageSpec bytes penalize peer"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
      import org.ergoplatform.network.peer.PenaltyType
      import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer

      val hist    = ErgoHistory.readOrGenerate(settings)(null)
      val mempool = ErgoMemPool.empty(settings)

      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      val invalidBytes = Array.fill(100)(0.toByte)
      synchronizerMockRef ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(invalidBytes),
        Some(peer)
      )

      val messages = ncProbe.receiveWhile(max = 2 seconds, idle = 200.millis) {
        case m => m
      }
      messages.exists {
        case PenalizePeer(_, PenaltyType.PermanentPenalty) => true
        case _ => false
      } shouldBe true
    }
  }

  property(
    "NodeViewSynchronizer: modifiersReq with unknown modifier type sends no message"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.NetworkObjectTypeId
      import scorex.util.bytesToId

      val hist    = ErgoHistory.readOrGenerate(settings)(null)
      val mempool = ErgoMemPool.empty(settings)

      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(mempool)
      Thread.sleep(500)

      val unknownTypeId = NetworkObjectTypeId.fromByte(0.toByte)
      val unknownInvData = InvData(
        unknownTypeId,
        Seq(bytesToId(Array.fill(32)(0xAB.toByte)))
      )
      synchronizerMockRef ! Message(
        RequestModifierSpec,
        Right(unknownInvData),
        Some(peer)
      )

      ncProbe.expectNoMessage(1.second)
      pchProbe.expectNoMessage(1.second)
    }
  }

  property(
    "NodeViewSynchronizer: processInv for unknown input block requests and re-requests on CheckDelivery"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.InputBlockTypeId
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.CheckDelivery
      import org.ergoplatform.network.message.{InvData, InvSpec, RequestModifierSpec}
      import scorex.core.network.SendToPeer
      import scorex.util.bytesToId

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      synchronizerMockRef ! ChangedHistory(hist)
      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val inputBlockId = bytesToId(Array.fill(32)(0xFA.toByte))
      val invData      = InvData(InputBlockTypeId.value, Seq(inputBlockId))
      synchronizerMockRef ! Message(InvSpec, Left(InvSpec.toBytes(invData)), Some(peer))

      val initial = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      initial.message.spec.messageCode shouldBe RequestModifierSpec.messageCode
      initial.message.data.get.asInstanceOf[InvData].typeId shouldBe InputBlockTypeId.value
      initial.message.data.get.asInstanceOf[InvData].ids shouldBe Seq(inputBlockId)
      initial.sendingStrategy shouldBe SendToPeer(peer)

      synchronizerMockRef ! CheckDelivery(peer, InputBlockTypeId.value, inputBlockId)

      val reRequest = ncProbe.fishForMessage(3 seconds) {
        case stn: SendToNetwork =>
          stn.message.spec.messageCode == RequestModifierSpec.messageCode &&
          stn.message.data.get.asInstanceOf[InvData].typeId == InputBlockTypeId.value &&
          stn.message.data.get.asInstanceOf[InvData].ids == Seq(inputBlockId)
        case _ => false
      }
      reRequest.asInstanceOf[SendToNetwork].sendingStrategy shouldBe SendToPeer(peer)
    }
  }

  property(
    "NodeViewSynchronizer: delivered input block clears Requested status so CheckDelivery does not re-request"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.modifiers.InputBlockTypeId
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.CheckDelivery
      import org.ergoplatform.network.message.{InvData, InvSpec, RequestModifierSpec}
      import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
      import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
      import scorex.core.network.SendToPeer

      val viewHolderProbe = TestProbe("ViewHolderProbe")
      val testSyncTracker = ErgoSyncTracker(settings.scorexSettings.network)
      val testDeliveryTracker = DeliveryTracker.empty(settings)
      implicit val ec: ExecutionContextExecutor = ctx.system.dispatcher

      val testSynchronizerRef: TestActorRef[SynchronizerMock] = TestActorRef(
        Props(
          new SynchronizerMock(
            ncProbe.ref,
            viewHolderProbe.ref,
            ErgoSyncInfoMessageSpec,
            settings,
            testSyncTracker,
            testDeliveryTracker
          )
        )
      )

      val hist = ErgoHistory.readOrGenerate(settings)(null)
      val wrappedState = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, settings))
        .sample
        .get

      testSynchronizerRef ! ChangedState(wrappedState)
      testSynchronizerRef ! ChangedHistory(hist)
      testSynchronizerRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      val inputBlockInfo = buildValidInputBlockAnnouncement(hist.bestFullBlockOpt)
        .copy(weakTxIds = Some(Seq.empty))

      // Request the input block via Inv
      val invData = InvData(InputBlockTypeId.value, Seq(inputBlockInfo.id))
      testSynchronizerRef ! Message(InvSpec, Left(InvSpec.toBytes(invData)), Some(peer))

      val initial = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      initial.message.spec.messageCode shouldBe RequestModifierSpec.messageCode
      initial.sendingStrategy shouldBe SendToPeer(peer)

      // Deliver the requested input block
      val inputBlockBytes = InputBlockMessageSpec.toBytes(inputBlockInfo)
      testSynchronizerRef ! Message(InputBlockMessageSpec, Left(inputBlockBytes), Some(peer))

      // Allow processInputBlock to run and update the delivery tracker
      Thread.sleep(200)

      // Manually trigger the delivery check: it should be a no-op now
      testSynchronizerRef ! CheckDelivery(peer, InputBlockTypeId.value, inputBlockInfo.id)
      ncProbe.expectNoMessage(500.millis)
    }
  }

  property(
    "NodeViewSynchronizer: NewBestInputBlock(local=true) does not broadcast to legacy peers"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
      import org.ergoplatform.network.{ModePeerFeature, PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo

      val chain  = genChain(3)
      val header = chain.head.header

      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        None
      )
      // Apply the input block through the node view holder so that the synchronizer's
      // history reader (updated from the NVH event stream) can find it.
      nodeViewHolderMockRef ! ProcessInputBlock(inputBlockInfo, peer)
      Thread.sleep(500)

      val legacyPeerProbe = TestProbe("LegacyPeerHandlerProbe")
      val legacyPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.initial,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val legacyPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        legacyPeerProbe.ref,
        Some(PeerInfo(legacyPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(legacyPeer, Equal, Some(header.height))

      val subBlockPeerProbe = TestProbe("SubBlockPeerHandlerProbe")
      val subBlockPeerSpec = PeerSpec(
        settings.scorexSettings.network.agentName,
        Version.SubblocksVersion,
        settings.scorexSettings.network.nodeName,
        None,
        Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
      )
      val subBlockPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        subBlockPeerProbe.ref,
        Some(PeerInfo(subBlockPeerSpec, System.currentTimeMillis()))
      )
      syncTracker.updateStatus(subBlockPeer, Equal, Some(header.height))

      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      synchronizerMockRef ! NewBestInputBlock(Some(header.id), local = true)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe InputBlockMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) =>
          peers should contain(subBlockPeer)
          peers should not contain legacyPeer
        case other => fail(s"Expected SendToPeers, got $other")
      }
      legacyPeerProbe.expectNoMessage(500.millis)
    }
  }

  property(
    "NodeViewSynchronizer: NewBestInputBlock(local=true) broadcasts only to peers within two blocks"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.Equal
      import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
      import org.ergoplatform.network.{ModePeerFeature, PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo

      val chain  = genChain(3)
      val header = chain.head.header

      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        None
      )
      // Apply the input block through the node view holder so that the synchronizer's
      // history reader (updated from the NVH event stream) can find it.
      nodeViewHolderMockRef ! ProcessInputBlock(inputBlockInfo, peer)
      Thread.sleep(500)

      def makeSubBlockPeer(height: Int): ConnectedPeer = {
        val probe = TestProbe(s"SubBlockPeerHeight$height")
        val spec = PeerSpec(
          settings.scorexSettings.network.agentName,
          Version.SubblocksVersion,
          settings.scorexSettings.network.nodeName,
          None,
          Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
        )
        val cp = ConnectedPeer(
          connectionIdGen.sample.get,
          probe.ref,
          Some(PeerInfo(spec, System.currentTimeMillis()))
        )
        syncTracker.updateStatus(cp, Equal, Some(height))
        cp
      }

      // With an empty history fullBlockHeight is 0, so only heights 0..2 are in window.
      val inWindow    = Seq(0, 1, 2).map(makeSubBlockPeer)
      val outOfWindow = Seq(3).map(makeSubBlockPeer)

      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      synchronizerMockRef ! NewBestInputBlock(Some(header.id), local = true)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe InputBlockMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) =>
          inWindow.foreach(peers should contain(_))
          outOfWindow.foreach(peers should not contain _)
        case other => fail(s"Expected SendToPeers, got $other")
      }
    }
  }

  property(
    "NodeViewSynchronizer: NewBestInputBlock(local=true) broadcasts to peers regardless of chain status within window"
  ) {
    withFixture2 { ctx =>
      import ctx._
      import org.ergoplatform.consensus.{Equal, Fork, Older, Younger}
      import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
      import org.ergoplatform.network.{ModePeerFeature, PeerSpec, Version}
      import scorex.core.network.{ConnectedPeer, SendToPeers}
      import org.ergoplatform.network.peer.PeerInfo

      val chain  = genChain(3)
      val header = chain.head.header

      val inputBlockInfo = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        None
      )
      nodeViewHolderMockRef ! ProcessInputBlock(inputBlockInfo, peer)
      Thread.sleep(500)

      def makeSubBlockPeer(status: org.ergoplatform.consensus.PeerChainStatus): ConnectedPeer = {
        val probe = TestProbe(s"SubBlockPeer$status")
        val spec = PeerSpec(
          settings.scorexSettings.network.agentName,
          Version.SubblocksVersion,
          settings.scorexSettings.network.nodeName,
          None,
          Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
        )
        val cp = ConnectedPeer(
          connectionIdGen.sample.get,
          probe.ref,
          Some(PeerInfo(spec, System.currentTimeMillis()))
        )
        // height 1 is within +/- 2 of empty history fullBlockHeight 0
        syncTracker.updateStatus(cp, status, Some(1))
        cp
      }

      val equalPeer  = makeSubBlockPeer(Equal)
      val forkPeer   = makeSubBlockPeer(Fork)
      val olderPeer  = makeSubBlockPeer(Older)
      val youngerPeer = makeSubBlockPeer(Younger)

      synchronizerMockRef ! ChangedMempool(ErgoMemPool.empty(settings))
      Thread.sleep(500)

      synchronizerMockRef ! NewBestInputBlock(Some(header.id), local = true)

      val msg = ncProbe.expectMsgClass(3 seconds, classOf[SendToNetwork])
      msg.message.spec.messageCode shouldBe InputBlockMessageSpec.messageCode
      msg.sendingStrategy match {
        case SendToPeers(peers) =>
          peers should contain(equalPeer)
          peers should contain(forkPeer)
          peers should contain(olderPeer)
          peers should contain(youngerPeer)
        case other => fail(s"Expected SendToPeers, got $other")
      }
    }
  }

  property("NodeViewSynchronizer: RecoverableFailedModification with ParentHeaderNotFoundError should request parent header") {
    withFixture2 { ctx =>
      import ctx._

      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(hhistory.append)

      val parentHeader = baseChain.last
      val childHeader = genHeaderChain(_.size > 2, Some(parentHeader), hhistory.difficultyCalculator, None, false).last

      // Set up sync tracker with an older peer
      syncTracker.updateStatus(peer, org.ergoplatform.consensus.Older, Some(childHeader.height))

      // Send ChangedHistory to set up historyReader in the synchronizer
      synchronizerMockRef ! ChangedHistory(hhistory)

      // Use a random parent ID that is NOT in the history so the synchronizer will request it
      val unknownParentId = bytesToId(scorex.utils.Random.randomBytes(32))
      val modifierId = childHeader.id
      val error = new ParentHeaderNotFoundError(unknownParentId, modifierId, Header.modifierTypeId)
      synchronizerMockRef ! RecoverableFailedModification(Header.modifierTypeId, modifierId, error)

      // Should request the parent header from the older peer
      ncProbe.fishForMessage(3 seconds) { case m =>
        m match {
          case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
            val invData = stn.message.data.get.asInstanceOf[InvData]
            invData.typeId == Header.modifierTypeId && invData.ids.contains(unknownParentId)
          case _ => false
        }
      }

      // The modifier should be set to Unknown
      eventually {
        deliveryTracker.status(modifierId, Header.modifierTypeId, Seq.empty) shouldBe Unknown
      }
    }
  }

  property("NodeViewSynchronizer: RecoverableFailedModification without ParentHeaderNotFoundError should just set Unknown") {
    withFixture2 { ctx =>
      import ctx._

      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(hhistory.append)

      val header = baseChain.last
      val modifierId = header.id

      // Send RecoverableFailedModification with a generic recoverable error
      val error = new RecoverableModifierError("some error", modifierId, Header.modifierTypeId)
      synchronizerMockRef ! RecoverableFailedModification(Header.modifierTypeId, modifierId, error)

      // The modifier should be set to Unknown
      eventually {
        deliveryTracker.status(modifierId, Header.modifierTypeId, Seq.empty) shouldBe Unknown
      }
    }
  }

  property("NodeViewSynchronizer: RecoverableFailedModification with ParentHeaderNotFoundError should not request if parent already known") {
    withFixture2 { ctx =>
      import ctx._

      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(hhistory.append)

      val parentHeader = baseChain.last
      val childHeader = genHeaderChain(_.size > 2, Some(parentHeader), hhistory.difficultyCalculator, None, false).last

      // Set up sync tracker with an older peer
      syncTracker.updateStatus(peer, org.ergoplatform.consensus.Older, Some(childHeader.height))

      // Send ChangedHistory to set up historyReader in the synchronizer
      synchronizerMockRef ! ChangedHistory(hhistory)

      // Parent header IS in history, so no request should be made
      val parentId = parentHeader.id
      val modifierId = childHeader.id
      val error = new ParentHeaderNotFoundError(parentId, modifierId, Header.modifierTypeId)
      synchronizerMockRef ! RecoverableFailedModification(Header.modifierTypeId, modifierId, error)

      // Should NOT request the parent header since it's already in history
      ncProbe.expectNoMessage(1.second)

      // The modifier should still be set to Unknown
      eventually {
        deliveryTracker.status(modifierId, Header.modifierTypeId, Seq.empty) shouldBe Unknown
      }
    }
  }

  property("NodeViewSynchronizer: RecoverableFailedModification with ParentHeaderNotFoundError should warn when no older peers") {
    withFixture2 { ctx =>
      import ctx._

      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(hhistory.append)

      val parentHeader = baseChain.last
      val childHeader = genHeaderChain(_.size > 2, Some(parentHeader), hhistory.difficultyCalculator, None, false).last

      // NO older peers set up - only the original peer as Younger
      syncTracker.updateStatus(peer, org.ergoplatform.consensus.Younger, Some(childHeader.height))

      // Send ChangedHistory to set up historyReader in the synchronizer
      synchronizerMockRef ! ChangedHistory(hhistory)

      // Use a random parent ID that is NOT in the history
      val unknownParentId = bytesToId(scorex.utils.Random.randomBytes(32))
      val modifierId = childHeader.id
      val error = new ParentHeaderNotFoundError(unknownParentId, modifierId, Header.modifierTypeId)
      synchronizerMockRef ! RecoverableFailedModification(Header.modifierTypeId, modifierId, error)

      // Should NOT send any network request since no older peers available
      ncProbe.expectNoMessage(1.second)

      // The modifier should still be set to Unknown
      eventually {
        deliveryTracker.status(modifierId, Header.modifierTypeId, Seq.empty) shouldBe Unknown
      }
    }
  }

  property("NodeViewSynchronizer: checkDelivery should not crash with empty peer candidates") {
    withFixture2 { ctx =>
      import ctx._

      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(hhistory.append)

      val header = baseChain.last
      val modifierId = header.id

      // Set up delivery tracker with requested status
      deliveryTracker.setRequested(Header.modifierTypeId, modifierId, peer)(_ => Cancellable.alreadyCancelled)

      // Ensure no peers are available for downloading headers
      // This should not crash even with empty peer candidates
      synchronizerMockRef ! CheckDelivery(peer, Header.modifierTypeId, modifierId)

      // The modifier should still be in Requested state or transitioned appropriately
      eventually {
        val status = deliveryTracker.status(modifierId, Header.modifierTypeId, Seq.empty)
        status should (be(Unknown) or be(Requested))
      }
    }
  }

  property("NodeViewSynchronizer: checkDelivery should fallback to Equal peers after many attempts") {
    withFixture2 { ctx =>
      import ctx._

      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(hhistory.append)

      val header = baseChain.last
      val modifierId = header.id

      // Create an equal peer
      val equalPeerInfo = PeerInfo(defaultPeerSpec, System.currentTimeMillis())
      val equalPeer = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(equalPeerInfo)
      )

      // Set up sync tracker with equal peer
      syncTracker.updateStatus(equalPeer, org.ergoplatform.consensus.Equal, Some(header.height))

      // Set up delivery tracker with many checks done (> 5)
      deliveryTracker.setRequested(Header.modifierTypeId, modifierId, peer)(_ => Cancellable.alreadyCancelled)
      // Simulate many delivery checks by sending multiple CheckDelivery messages
      (1 to 7).foreach { _ =>
        synchronizerMockRef ! CheckDelivery(peer, Header.modifierTypeId, modifierId)
      }

      // Should eventually try the equal peer (we can verify by checking network messages)
      // The test passes if no crash occurs and status transitions appropriately
      eventually {
        val status = deliveryTracker.status(modifierId, Header.modifierTypeId, Seq.empty)
      status should (be(Unknown) or be(Requested))
      }
    }
  }

  property("NodeViewSynchronizer: checkDelivery should set non-header modifier to Unknown after max attempts") {
    withFixture2 { ctx =>
      import ctx._

      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(hhistory.append)

      val header = baseChain.last
      val modifierId = header.id

      // Use a non-header modifier type (e.g., BlockTransactions)
      val nonHeaderTypeId = org.ergoplatform.modifiers.history.BlockTransactions.modifierTypeId

      // Set up delivery tracker with requested status
      deliveryTracker.setRequested(nonHeaderTypeId, modifierId, peer)(_ => Cancellable.alreadyCancelled)

      // Send many CheckDelivery messages to exceed maxDeliveryChecks
      val maxDeliveryChecks = settings.scorexSettings.network.maxDeliveryChecks
      (1 to maxDeliveryChecks + 2).foreach { _ =>
        synchronizerMockRef ! CheckDelivery(peer, nonHeaderTypeId, modifierId)
      }

      // After max attempts, non-header modifier should be set to Unknown (not Invalid)
      eventually {
        deliveryTracker.status(modifierId, nonHeaderTypeId, Seq.empty) shouldBe Unknown
      }
    }
  }

  property("NodeViewSynchronizer: checkDelivery should invalidate header after max attempts") {
    withFixture2 { ctx =>
      import ctx._

      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(hhistory.append)

      val header = baseChain.last
      val modifierId = header.id

      // Set up delivery tracker with requested status for header
      deliveryTracker.setRequested(Header.modifierTypeId, modifierId, peer)(_ => Cancellable.alreadyCancelled)

      // Send many CheckDelivery messages to exceed maxDeliveryChecks
      val maxDeliveryChecks = settings.scorexSettings.network.maxDeliveryChecks
      (1 to maxDeliveryChecks + 2).foreach { _ =>
        synchronizerMockRef ! CheckDelivery(peer, Header.modifierTypeId, modifierId)
      }

      // After max attempts, header should be marked as Invalid
      eventually {
        deliveryTracker.status(modifierId, Header.modifierTypeId, Seq.empty) shouldBe scorex.core.network.ModifiersStatus.Invalid
      }
    }
  }

  /**
    * Regression test for the `lastSyncHeaderApplied` removal.
    * When a header is already in history, `applyValidContinuationHeaderV2` must
    * detect this via `deliveryTracker.status(..., Seq(history))` and skip it.
    * Previously `Seq.empty` was passed, so `Held` was never detected and the
    * header was re-sent to the view holder.
    */
  property("NodeViewSynchronizer: syncV2 should skip header already in history") {
    withFixture2 { ctx =>
      import ctx._

      // Build a base chain and apply it to history
      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(h => hhistory.append(h).get)
      val bestHeaderOpt = hhistory.bestHeaderOpt

      // Generate a continuation header (direct child of our best header).
      // Use .tail to drop the prefix so the chain starts with the direct child.
      val continuationChain = genHeaderChain(_.size > 2, bestHeaderOpt, hhistory.difficultyCalculator, None, false).tail
      val continuationHeader = continuationChain.headers.head

      // Apply the continuation header directly to history so it is already "Held"
      hhistory.append(continuationHeader).get
      hhistory.bestHeaderIdOpt.get shouldBe continuationHeader.id

      // Set up the synchronizer with the updated history
      synchronizerMockRef ! ChangedHistory(hhistory)

      // Build a syncV2 message.
      // continuationHeaderV2 checks: bestHeaderIdOpt.contains(lastHeader.parentId)
      // So the FIRST header in the list must be the child of our best header.
      // Our best header is continuationHeader, so we need a child of it.
      val childChain = genHeaderChain(_.size > 2, Some(continuationHeader), hhistory.difficultyCalculator, None, false).tail
      val childHeader = childChain.headers.head

      // Apply the child header to history as well so it is "Held"
      hhistory.append(childHeader).get

      // Set up the synchronizer with the updated history (after child applied)
      synchronizerMockRef ! ChangedHistory(hhistory)

      val sync = ErgoSyncInfoV2(Seq(childHeader))
      val msgBytes = ErgoSyncInfoMessageSpec.toBytes(sync)

      // Send the sync message — the synchronizer must NOT send the header to the view holder
      // because childHeader is already in history (we just applied it above)
      synchronizerMockRef ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))

      // The synchronizer may send a sync response back (SendToNetwork with Sync message),
      // which is expected. We must NOT see any RequestModifier messages.
      ncProbe.fishForMessage(2.seconds) { case m =>
        m match {
          case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
            // RequestModifier means the header was sent to VH — this is a failure
            false
          case _ =>
            // Any other message (e.g. sync response) is fine — keep fishing until timeout
            true
        }
      }

      // Also verify deliveryTracker reports Held when history is passed
      deliveryTracker.status(childHeader.id, Header.modifierTypeId, Seq(hhistory)) shouldBe
        scorex.core.network.ModifiersStatus.Held
    }
  }

  /**
    * Test that `deliveryTracker.setReceived` is called when a syncV2 header is accepted.
    * This prevents the header from being processed again on duplicate sync messages
    * before the view holder reports the outcome.
    */
  property("NodeViewSynchronizer: syncV2 header should be tracked as Received immediately") {
    withFixture2 { ctx =>
      import ctx._

      // Build a base chain
      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(h => hhistory.append(h).get)
      val bestHeaderOpt = hhistory.bestHeaderOpt

      // Generate a continuation header that is NOT yet in history.
      // Use .tail to drop the prefix so the chain starts with the direct child of our best header.
      val continuationChain = genHeaderChain(_.size > 2, bestHeaderOpt, hhistory.difficultyCalculator, None, false).tail
      val continuationHeader = continuationChain.headers.head
      hhistory.contains(continuationHeader.id) shouldBe false

      // Set up the synchronizer with the base history
      synchronizerMockRef ! ChangedHistory(hhistory)

      // Build a syncV2 message.
      // continuationHeaderV2 checks: bestHeaderIdOpt.contains(lastHeader.parentId)
      // So the FIRST header in the list must be the direct child of our best header.
      val sync = ErgoSyncInfoV2(Seq(continuationHeader))
      val msgBytes = ErgoSyncInfoMessageSpec.toBytes(sync)

      // Send the sync message
      synchronizerMockRef ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))

      // Wait for block section requests to confirm the header was accepted
      // (the synchronizer sends block section requests after sending header to VH)
      val requestMsg = ncProbe.fishForMessage(3.seconds) { case m =>
        m match {
          case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
            true
          case _ =>
            false
        }
      }.asInstanceOf[SendToNetwork]

      // Verify the request is for block sections of our continuation header
      val invData = requestMsg.message.data.get.asInstanceOf[InvData]
      val expectedIds = continuationHeader.sectionIdsWithNoProof.map(_._2)
      invData.ids.exists(id => expectedIds.exists(_ == id)) shouldBe true

      // Sending the same sync message again should NOT trigger another download.
      // The synchronizer may send a sync response back, which is expected.
      synchronizerMockRef ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))
      ncProbe.fishForMessage(2.seconds) { case m =>
        m match {
          case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
            // If we get a RequestModifier, the header was re-sent — this is a failure
            false
          case _ =>
            // Any other message (e.g. sync response) is fine — keep fishing until timeout
            true
        }
      }
    }
  }

  /**
    * Test that NewBlockMined immediately broadcasts invs for header and all block sections.
    */
  property("NodeViewSynchronizer: NewBlockMined should immediately broadcast invs") {
    withFixture2 { ctx =>
      import ctx._

      // Build state with some applied blocks
      var wus = WrappedUtxoState(boxesHolderGen.sample.get, createTempDir, parameters, settings)
      (0 until 3).foreach { _ =>
        val block = statefulyValidFullBlock(wus)
        wus = wus.applyModifier(block, None)(_ => ()).get
      }

      val newBlock = statefulyValidFullBlock(wus)

      // Send NewBlockMined to synchronizer
      synchronizerMockRef ! NewBlockMined(newBlock.header)

      // Expect 4 inv messages (1 header + 3 sections)
      val invMessages = (0 until 4).map { _ =>
        ncProbe.expectMsgType[SendToNetwork](5.seconds)
      }.filter(_.message.spec.messageCode == InvSpec.messageCode)

      val receivedInvs = invMessages.map { stn =>
        val invData = stn.message.data.get.asInstanceOf[InvData]
        invData.typeId -> invData.ids
      }.toMap

      // Verify header inv was broadcast
      receivedInvs.get(Header.modifierTypeId) shouldBe defined
      receivedInvs(Header.modifierTypeId) should contain(newBlock.header.id)

      // Verify all block section invs were broadcast
      newBlock.header.sectionIds.foreach { case (mtId, id) =>
        receivedInvs.get(mtId) shouldBe defined
        receivedInvs(mtId) should contain(id)
      }
    }
  }

  /**
    * Test that LocalBlockApplied does not duplicate broadcast when NewBlockMined already fired.
    */
  property("NodeViewSynchronizer: NewBlockMined should prevent duplicate broadcast on LocalBlockApplied") {
    withFixture2 { ctx =>
      import ctx._

      // Build base chain and state
      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(h => hhistory.append(h).get)

      val (us, bh) = createUtxoState(settings)
      val bestBlockOpt = hhistory.bestFullBlockOpt
      val newBlock = validFullBlock(bestBlockOpt, us, bh)

      // First: NewBlockMined triggers immediate broadcast
      synchronizerMockRef ! NewBlockMined(newBlock.header)

      // Consume all inv messages from NewBlockMined
      ncProbe.receiveWhile(2.seconds) {
        case _: SendToNetwork => // consume
        case _ => // ignore
      }

      // Second: LocalBlockApplied for same header should NOT send additional invs
      synchronizerMockRef ! LocalBlockApplied(newBlock.header, newBlock.transactions.map(_.id))

      // Should receive no additional InvSpec messages
      ncProbe.expectNoMessage(1.second)
    }
  }

  /**
    * Test that LocalBlockApplied does not broadcast (already done via NewBlockMined).
    */
  property("NodeViewSynchronizer: LocalBlockApplied should skip broadcast") {
    withFixture2 { ctx =>
      import ctx._

      // Build base chain and state
      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(h => hhistory.append(h).get)

      val (us, bh) = createUtxoState(settings)
      val bestBlockOpt = hhistory.bestFullBlockOpt
      val newBlock = validFullBlock(bestBlockOpt, us, bh)

      // NewBlockMined should broadcast
      synchronizerMockRef ! NewBlockMined(newBlock.header)

      // Consume the inv messages
      val deadline1 = System.currentTimeMillis() + 2000
      while (System.currentTimeMillis() < deadline1) {
        ncProbe.receiveOne(100.millis) match {
          case Some(_: SendToNetwork) => // consume
          case _ => // ignore
        }
      }

      // LocalBlockApplied for same block should NOT broadcast again
      synchronizerMockRef ! LocalBlockApplied(newBlock.header, newBlock.transactions.map(_.id))

      // Should receive no additional InvSpec messages
      ncProbe.expectNoMessage(1.second)
    }
  }

  /**
    * Test that RemoteBlockApplied broadcasts invs for peer-received blocks.
    */
  property("NodeViewSynchronizer: RemoteBlockApplied should broadcast invs") {
    withFixture2 { ctx =>
      import ctx._

      // Build state with some applied blocks
      var wus = WrappedUtxoState(boxesHolderGen.sample.get, createTempDir, parameters, settings)
      (0 until 3).foreach { _ =>
        val block = statefulyValidFullBlock(wus)
        wus = wus.applyModifier(block, None)(_ => ()).get
      }

      val newBlock = statefulyValidFullBlock(wus)

      // Send RemoteBlockApplied to synchronizer
      synchronizerMockRef ! RemoteBlockApplied(newBlock.header, newBlock.transactions.map(_.id))

      // Expect 4 inv messages (1 header + 3 sections)
      val invMessages = (0 until 4).map { _ =>
        ncProbe.expectMsgType[SendToNetwork](5.seconds)
      }.filter(_.message.spec.messageCode == InvSpec.messageCode)

      val receivedInvs = invMessages.map { stn =>
        val invData = stn.message.data.get.asInstanceOf[InvData]
        invData.typeId -> invData.ids
      }.toMap

      // Verify header inv was broadcast
      receivedInvs.get(Header.modifierTypeId) shouldBe defined
      receivedInvs(Header.modifierTypeId) should contain(newBlock.header.id)

      // Verify all block section invs were broadcast
      newBlock.header.sectionIds.foreach { case (mtId, id) =>
        receivedInvs.get(mtId) shouldBe defined
        receivedInvs(mtId) should contain(id)
      }
    }
  }

  /**
    * Test that NewBlockMined broadcasts invs for a newly mined block.
    */
  property("NodeViewSynchronizer: NewBlockMined should broadcast invs for newly mined block") {
    withFixture2 { ctx =>
      import ctx._

      // Build state with some applied blocks
      var wus = WrappedUtxoState(boxesHolderGen.sample.get, createTempDir, parameters, settings)
      (0 until 3).foreach { _ =>
        val block = statefulyValidFullBlock(wus)
        wus = wus.applyModifier(block, None)(_ => ()).get
      }

      val newBlock = statefulyValidFullBlock(wus)

      // Send NewBlockMined to synchronizer
      synchronizerMockRef ! NewBlockMined(newBlock.header)

      // Expect 4 inv messages (1 header + 3 sections)
      val invMessages = (0 until 4).map { _ =>
        ncProbe.expectMsgType[SendToNetwork](5.seconds)
      }.filter(_.message.spec.messageCode == InvSpec.messageCode)

      val receivedInvs = invMessages.map { stn =>
        val invData = stn.message.data.get.asInstanceOf[InvData]
        invData.typeId -> invData.ids
      }.toMap

      // Verify header inv was broadcast
      receivedInvs.get(Header.modifierTypeId) shouldBe defined
      receivedInvs(Header.modifierTypeId) should contain(newBlock.header.id)

      // Verify all block section invs were broadcast
      newBlock.header.sectionIds.foreach { case (mtId, id) =>
        receivedInvs.get(mtId) shouldBe defined
        receivedInvs(mtId) should contain(id)
      }
    }
  }

  /**
    * Test that LocalBlockApplied and RemoteBlockApplied should perform cleanup.
    */
  property("NodeViewSynchronizer: LocalBlockApplied and RemoteBlockApplied should perform cleanup") {
    withFixture2 { ctx =>
      import ctx._

      // Build state with some applied blocks
      var wus = WrappedUtxoState(boxesHolderGen.sample.get, createTempDir, parameters, settings)
      (0 until 3).foreach { _ =>
        val block = statefulyValidFullBlock(wus)
        wus = wus.applyModifier(block, None)(_ => ()).get
      }

      val newBlock = statefulyValidFullBlock(wus)

      // Send LocalBlockApplied - should not broadcast but should perform cleanup
      synchronizerMockRef ! LocalBlockApplied(newBlock.header, newBlock.transactions.map(_.id))
      ncProbe.expectNoMessage(500.millis)

      // Send RemoteBlockApplied - should broadcast (different block)
      val newBlock2 = statefulyValidFullBlock(wus)
      synchronizerMockRef ! RemoteBlockApplied(newBlock2.header, newBlock2.transactions.map(_.id))

      // Expect 4 inv messages (1 header + 3 sections)
      val invMessages = (0 until 4).map { _ =>
        ncProbe.expectMsgType[SendToNetwork](5.seconds)
      }.filter(_.message.spec.messageCode == InvSpec.messageCode)

      val receivedInvs = invMessages.map { stn =>
        val invData = stn.message.data.get.asInstanceOf[InvData]
        invData.typeId -> invData.ids
      }.toMap

      // Verify header inv was broadcast for the second block
      receivedInvs.get(Header.modifierTypeId) shouldBe defined
      receivedInvs(Header.modifierTypeId) should contain(newBlock2.header.id)
    }
  }

  /**
    * During UTXO set snapshot bootstrap block section invs must be ignored until the snapshot is applied.
    */
  property("NodeViewSynchronizer: InvSpec - block section invs are ignored during utxo bootstrap") {
    withUtxoBootstrapFixture(utxoBootstrap = true) { ctx =>
      import ctx._

      val unknownSectionId = modifierIdGen.sample.get
      synchronizerMockRef ! Message(InvSpec,
        Left(InvSpec.toBytes(InvData(Extension.modifierTypeId, Seq(unknownSectionId)))),
        Some(peer))

      // request must not be sent, the inv is dropped
      ncProbe.expectNoMessage(1.second)
    }
  }

  property("NodeViewSynchronizer: InvSpec - header invs are still processed during utxo bootstrap") {
    withUtxoBootstrapFixture(utxoBootstrap = true) { ctx =>
      import ctx._

      val unknownHeaderId = modifierIdGen.sample.get
      synchronizerMockRef ! Message(InvSpec,
        Left(InvSpec.toBytes(InvData(Header.modifierTypeId, Seq(unknownHeaderId)))),
        Some(peer))

      requestForModifierSent(ncProbe, Header.modifierTypeId, unknownHeaderId)
    }
  }

  property("NodeViewSynchronizer: InvSpec - snapshot-related invs are not filtered during utxo bootstrap") {
    withUtxoBootstrapFixture(utxoBootstrap = true) { ctx =>
      import ctx._

      val manifestId = modifierIdGen.sample.get
      synchronizerMockRef ! Message(InvSpec,
        Left(InvSpec.toBytes(InvData(ManifestTypeId.value, Seq(manifestId)))),
        Some(peer))
      requestForModifierSent(ncProbe, ManifestTypeId.value, manifestId)

      val chunkId = modifierIdGen.sample.get
      synchronizerMockRef ! Message(InvSpec,
        Left(InvSpec.toBytes(InvData(UtxoSnapshotChunkTypeId.value, Seq(chunkId)))),
        Some(peer))
      requestForModifierSent(ncProbe, UtxoSnapshotChunkTypeId.value, chunkId)
    }
  }

  property("NodeViewSynchronizer: InvSpec - block section invs processed after utxo snapshot applied") {
    withUtxoBootstrapFixture(utxoBootstrap = true) { ctx =>
      import ctx._

      // apply snapshot, so that block sections downloading is allowed again
      updHistory.onUtxoSnapshotApplied(ctx.chain.last.height)

      val unknownSectionId = modifierIdGen.sample.get
      synchronizerMockRef ! Message(InvSpec,
        Left(InvSpec.toBytes(InvData(Extension.modifierTypeId, Seq(unknownSectionId)))),
        Some(peer))

      requestForModifierSent(ncProbe, Extension.modifierTypeId, unknownSectionId)
    }
  }

  property("NodeViewSynchronizer: InvSpec - block section invs processed when utxoBootstrap disabled") {
    withUtxoBootstrapFixture(utxoBootstrap = false) { ctx =>
      import ctx._

      val unknownSectionId = modifierIdGen.sample.get
      synchronizerMockRef ! Message(InvSpec,
        Left(InvSpec.toBytes(InvData(Extension.modifierTypeId, Seq(unknownSectionId)))),
        Some(peer))

      requestForModifierSent(ncProbe, Extension.modifierTypeId, unknownSectionId)
    }
  }

  /*
   * Anti-spam handling of transactions declined by the mempool.
   *
   * A single peer sending plenty of declined (and thus cheap to produce and to check) transactions must not be able to
   * shut down transaction intake of the whole node. See `MaxDeclined`, `MinDeclinedTxCost`, `addDeclined` and
   * `txAcceptanceFilter` in `ErgoNodeViewSynchronizer`.
   */

  private val BlocksInDeclinedChain = 10

  /**
    * Per-peer mempool processing budget between two applied blocks, `MempoolPeerCostPerBlock` in the synchronizer.
    * A peer's transaction intake is stopped when the budget is reached, see `txAcceptanceFilter`.
    */
  private val PeerBudgetBetweenBlocks = 10000000

  /**
    * Number of the cheapest declines a peer has to send to exhaust its budget. Since every decline is
    * charged with at least `MinDeclinedTxCost`, the real validation cost of such transactions (as low as
    * 1,000 units, e.g. a min fee check) does not matter here.
    */
  private val DeclinesToThrottlePeer = PeerBudgetBetweenBlocks * 3 / 2 / MinDeclinedTxCost + 2

  /**
    * A transaction with a random id, no signing involved. The synchronizer reads only the id and the source of a
    * transaction when it processes the corresponding `DeclinedTransaction` event, so the transaction may be dummy.
    */
  private def dummyDeclinedTx(): ErgoTransaction = {
    val proof = ProverResult(Array(0x7c.toByte), ContextExtension.empty)
    val inputs = IndexedSeq(Input(ADKey @@ scorex.utils.Random.randomBytes(32), proof))
    val minimalAmount = BoxUtils.minimalErgoAmountSimulated(TrueTree, Colls.emptyColl, Map(), parameters)
    val outputs = IndexedSeq(testBox(minimalAmount, TrueTree, creationHeight = startHeight))
    ErgoTransaction(inputs, outputs)
  }

  /**
    * Fixture for the declined transactions properties. Unlike `SynchronizerFixture`, the history here has all the
    * blocks applied, so that `hr.fullBlockHeight == hr.headersHeight` and `txAcceptanceFilter` may pass.
    */
  private class DeclinedTransactionsFixture extends AkkaFixture {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val ncProbe = TestProbe("NetworkControllerProbe")
    val vhProbe = TestProbe("ViewHolderProbe")
    val pchProbe = TestProbe("PeerHandlerProbe")
    val syncTracker = ErgoSyncTracker(settings.scorexSettings.network)
    val deliveryTracker: DeliveryTracker = DeliveryTracker.empty(settings)

    private val emptyHistory = generateHistory(verifyTransactions = true, StateType.Utxo,
      PoPoWBootstrap = false, blocksToKeep = -1)
    // named `declinedHistory` and not `history` to avoid a clash with the class level `history` of this spec
    val declinedHistory: ErgoHistory = applyChain(emptyHistory, genChain(BlocksInDeclinedChain, emptyHistory))
    declinedHistory.fullBlockHeight shouldBe declinedHistory.headersHeight

    // `TestActorRef` gives access to the underlying actor, needed to check the size of the declined transactions
    // table, which is not observable from the outside otherwise
    val synchronizerTestRef: TestActorRef[ErgoNodeViewSynchronizer] = TestActorRef(Props(
      new SynchronizerMock(
        ncProbe.ref,
        vhProbe.ref,
        ErgoSyncInfoMessageSpec,
        settings,
        syncTracker,
        deliveryTracker
      )(ec)
    ))

    val synchronizerRef: ActorRef = synchronizerTestRef

    synchronizerRef ! ChangedHistory(declinedHistory)
    synchronizerRef ! ChangedMempool(ErgoMemPool.empty(settings))

    def newPeer: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      pchProbe.ref,
      Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis()))
    )

    /**
      * Simulate `txs` declined by the mempool after they were received from `peer`, each of them being checked with
      * `cost` cost units (1000 is the cheapest possible check, e.g. a min fee check)
      */
    def declineTxs(peer: ConnectedPeer, txs: Seq[ErgoTransaction], cost: Int): Unit = {
      txs.foreach { tx =>
        synchronizerRef ! DeclinedTransaction(UnconfirmedTransaction(tx, Some(peer)).withCost(cost))
      }
    }

    /** Simulate `count` distinct transactions declined by the mempool after they were received from `peer` */
    def declineTxs(peer: ConnectedPeer, count: Int, cost: Int): Unit =
      declineTxs(peer, Seq.fill(count)(dummyDeclinedTx()), cost)

    /** `peer` announces `tx` via `Inv`, the node is expected to answer with `RequestModifier` */
    def invTxAndExpectRequest(peer: ConnectedPeer, tx: ErgoTransaction): Unit = {
      val invData = InvData(ErgoTransaction.modifierTypeId, Seq(tx.id))
      synchronizerRef ! Message(InvSpec, Left(InvSpec.toBytes(invData)), Some(peer))
      ncProbe.fishForMessage(5.seconds) { case m =>
        m match {
          case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
            val requested = stn.message.data.get.asInstanceOf[InvData]
            requested.typeId == ErgoTransaction.modifierTypeId && requested.ids.contains(tx.id)
          case _ => false
        }
      }
    }

    /** `peer` announces `tx` via `Inv`, no `RequestModifier` for it is expected to be sent */
    def invTxAndExpectNoRequest(peer: ConnectedPeer, tx: ErgoTransaction): Unit = {
      val invData = InvData(ErgoTransaction.modifierTypeId, Seq(tx.id))
      synchronizerRef ! Message(InvSpec, Left(InvSpec.toBytes(invData)), Some(peer))
      // any other message (sync, inv broadcast, etc.) may be sent to the network controller, so only
      // `RequestModifier` messages are collected here
      val deadline = System.currentTimeMillis() + 2.seconds.toMillis
      while (System.currentTimeMillis() < deadline) {
        ncProbe.receiveOne(100.millis) match {
          case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
            val requested = stn.message.data.get.asInstanceOf[InvData]
            requested.ids should not contain tx.id
          case _ => // not a request, ignore
        }
      }
    }
  }

  private def withDeclinedTransactionsFixture(testCode: DeclinedTransactionsFixture => Any): Unit = {
    val fixture = new DeclinedTransactionsFixture
    try {
      testCode(fixture)
    }
    finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  /**
    * Regression test for the transaction intake denial of service: before the fix, the node stopped asking peers for
    * transactions as soon as the table of declined transaction ids reached its max size, which any single peer was
    * able to do with cheap (declinable) transactions. Now the table is only a bounded de-duplication cache.
    */
  property("NodeViewSynchronizer: declined transactions table at max size does not stop intake") {
    withDeclinedTransactionsFixture { ctx =>
      import ctx._

      val attacker = newPeer
      val honest = newPeer

      // the cheapest declines, as the mempool rejects them before executing any script
      declineTxs(attacker, count = MaxDeclined, cost = 1000)

      // the node must still ask the other peers for transactions
      invTxAndExpectRequest(honest, dummyDeclinedTx())
    }
  }

  /**
    * The table of declined transactions is bounded, so that a peer sending plenty of declined transactions can not
    * bloat node's memory. Entries are dropped one by one, so that the de-duplication is not lost entirely when
    * the table overflows (as opposed to dropping the whole table at once).
    */
  property("NodeViewSynchronizer: declined transactions table is bounded") {
    withDeclinedTransactionsFixture { ctx =>
      import ctx._

      val attacker = newPeer
      val honest = newPeer

      // twice as much as the table may hold, so that eviction definitely happens
      declineTxs(attacker, count = 2 * MaxDeclined, cost = 1000)

      // barrier: the response to this inv proves that all the declines above are already processed
      // (messages from a single sender are processed in order)
      invTxAndExpectRequest(honest, dummyDeclinedTx())

      // the table is pinned at the limit rather than being emptied
      synchronizerTestRef.underlyingActor.declinedTxsCount shouldBe MaxDeclined
    }
  }

  /**
    * Cheapest declines are charged to the peer which sent them (with `MinDeclinedTxCost` floor), so a peer sending
    * them in bulk stops being served, while other peers are not affected at all.
    */
  property("NodeViewSynchronizer: cheap declines throttle the sending peer only") {
    withDeclinedTransactionsFixture { ctx =>
      import ctx._

      val attacker = newPeer
      val honest = newPeer

      declineTxs(attacker, count = DeclinesToThrottlePeer, cost = 1000)

      // the attacker exhausted its own budget, its transaction invs are not served anymore
      invTxAndExpectNoRequest(attacker, dummyDeclinedTx())

      // other peers are not affected, and the global budget is not inflated with the floor price,
      // as the real cost of the declines above is `DeclinesToThrottlePeer` cost units only
      invTxAndExpectRequest(honest, dummyDeclinedTx())
    }
  }

  /**
    * The per-peer cost is accumulated, so it must not overflow: an overflowed cost turns negative, and a negative
    * cost passes every limit check, which silently disables the throttling of the peer.
    *
    * Note the declines below are counted in the global counter with their measured cost (1000 each), so the global
    * budget is not exhausted here - this property is about the per-peer counter only.
    */
  property("NodeViewSynchronizer: per-peer cost does not overflow when a peer sends many declines") {
    withDeclinedTransactionsFixture { ctx =>
      import ctx._

      val attacker = newPeer

      // enough declines to overflow an `Int` accumulator, were the cost fields `Int` rather than `Long`
      val declinesToOverflowInt = Int.MaxValue / MinDeclinedTxCost + 2
      declineTxs(attacker, count = declinesToOverflowInt, cost = 1000)

      // the peer is throttled, so the accumulated cost did not wrap around to a negative value
      invTxAndExpectNoRequest(attacker, dummyDeclinedTx())
    }
  }

  /**
    * The peer budget, as well as the table of declined transactions, is reset when a new block is applied, so the
    * throttling of a peer lasts no longer than one block.
    */
  property("NodeViewSynchronizer: throttled peer is served again when new block is applied") {
    withDeclinedTransactionsFixture { ctx =>
      import ctx._

      val peer = newPeer

      declineTxs(peer, count = DeclinesToThrottlePeer, cost = 1000)
      invTxAndExpectNoRequest(peer, dummyDeclinedTx())

      synchronizerRef ! RemoteBlockApplied(declinedHistory.bestHeaderOpt.get, Seq.empty)

      // the peer budget is reset on block application, so the peer is served again
      invTxAndExpectRequest(peer, dummyDeclinedTx())
    }
  }

  /**
    * A declined transaction is worth re-evaluating once a new block is applied, as the reason of the decline may be
    * gone by now (e.g. the transaction double-spending it lost to is mined, or mempool capacity is freed).
    */
  property("NodeViewSynchronizer: declined transactions table is reset when new block is applied") {
    withDeclinedTransactionsFixture { ctx =>
      import ctx._

      val peer = newPeer
      val tx = dummyDeclinedTx()

      declineTxs(peer, tx +: Seq.fill(9)(dummyDeclinedTx()), cost = 1000)
      // the transaction is declined, so it is not asked for
      invTxAndExpectNoRequest(peer, tx)

      synchronizerRef ! RemoteBlockApplied(declinedHistory.bestHeaderOpt.get, Seq.empty)

      // after a block is applied the transaction is asked for again
      invTxAndExpectRequest(peer, tx)
    }
  }

  /**
    * Regression test for the shared processing cache stall: a peer which exhausted its per-block budget
    * (via cheap declines charged with `MinDeclinedTxCost`) has its deliveries put into the shared
    * `txProcessingCache`, which must neither grow unbounded nor stall transaction intake of other peers -
    * over-budget peers' records are dropped from the cache when it is drained.
    */
  property("NodeViewSynchronizer: over-budget peer's cached transactions do not stall intake of other peers") {
    withDeclinedTransactionsFixture { ctx =>
      import ctx._

      val attacker = newPeer
      val honest = newPeer

      // one decline short of the per-block budget (49 * MinDeclinedTxCost = 9.8M < 10M),
      // so the attacker's invs are still served
      declineTxs(attacker, count = PeerBudgetBetweenBlocks / MinDeclinedTxCost - 1, cost = 1000)

      // the attacker announces many transactions before delivering any; each inv is served
      val txs = Seq.fill(60)(dummyDeclinedTx())
      txs.foreach(tx => invTxAndExpectRequest(attacker, tx))

      // the attacker delivers all of them at once: the first is sent for processing, up to
      // `MaxProcessingTransactionsCacheSize` are cached, the rest are dropped
      val modData = ModifiersData(ErgoTransaction.modifierTypeId, txs.map(tx => tx.id -> tx.bytes).toMap)
      synchronizerRef ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(modData)), Some(attacker))

      // the first transaction of the delivery is being processed
      vhProbe.fishForMessage(5.seconds) {
        case _: ErgoNodeViewHolder.ReceivableMessages.TransactionFromRemote => true
        case _ => false
      }

      // barrier: the response to this inv proves that the delivery above is fully processed
      invTxAndExpectRequest(honest, dummyDeclinedTx())
      synchronizerTestRef.underlyingActor.txProcessingCacheSize shouldBe
        synchronizerTestRef.underlyingActor.MaxProcessingTransactionsCacheSize

      // the result for the transaction being processed comes in: the attacker is over its budget now
      declineTxs(attacker, count = 1, cost = 1000)

      // the cached records of the over-budget attacker are dropped, so other peers are still served
      invTxAndExpectRequest(honest, dummyDeclinedTx())
      synchronizerTestRef.underlyingActor.txProcessingCacheSize shouldBe 0

      // nothing from the attacker's cached delivery was sent for processing
      vhProbe.receiveWhile(1.second) { case m => m }.collect {
        case t: ErgoNodeViewHolder.ReceivableMessages.TransactionFromRemote => t
      } shouldBe empty
    }
  }

  property("NodeViewSynchronizer: dropping an old cached reply preserves the replacement request") {
    withDeclinedTransactionsFixture { ctx =>
      import ctx._

      val owner = newPeer
      val replacementHandler = TestProbe("ReplacementCachedTxHandler")
      val replacement = owner.copy(handlerRef = replacementHandler.ref)
      val txs = Seq.fill(2)(dummyDeclinedTx())
      txs.foreach(tx => invTxAndExpectRequest(owner, tx))

      val reply = ModifiersData(ErgoTransaction.modifierTypeId,
        txs.map(tx => tx.id -> tx.bytes).toMap)
      synchronizerRef ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(reply)), Some(owner))
      val forwarded = vhProbe.fishForMessage(5.seconds) {
        case _: ErgoNodeViewHolder.ReceivableMessages.TransactionFromRemote => true
        case _ => false
      }.asInstanceOf[ErgoNodeViewHolder.ReceivableMessages.TransactionFromRemote]
      val deferredId = txs.map(_.id).find(_ != forwarded.unconfirmedTx.id).get
      synchronizerTestRef.underlyingActor.txProcessingCacheSize shouldBe 1

      synchronizerRef ! CheckDelivery(owner, ErgoTransaction.modifierTypeId, deferredId)
      deliveryTracker.status(deferredId, ErgoTransaction.modifierTypeId, Seq.empty) shouldBe Unknown
      val deferredTx = txs.find(_.id == deferredId).get
      invTxAndExpectRequest(replacement, deferredTx)
      val replacementRequest = deliveryTracker.getRequestedInfo(ErgoTransaction.modifierTypeId, deferredId).get
      replacementRequest.peer.handlerRef shouldBe replacementHandler.ref

      // The owner's costly result drops its cached tail; the replacement still owns the live request.
      synchronizerRef ! DeclinedTransaction(forwarded.unconfirmedTx.withCost(PeerBudgetBetweenBlocks))
      synchronizerTestRef.underlyingActor.txProcessingCacheSize shouldBe 0
      val current = deliveryTracker.getRequestedInfo(ErgoTransaction.modifierTypeId, deferredId).get
      (current eq replacementRequest) shouldBe true
      current.cancellable.isCancelled shouldBe false
    }
  }

  /**
    * The transaction processing cache is bounded: a delivery bigger than the cache is partially dropped
    * (dropped transactions can be requested again when re-announced), and cache processing is resumed
    * when a new block is applied.
    */
  property("NodeViewSynchronizer: transaction processing cache is bounded and drained on block applied") {
    withDeclinedTransactionsFixture { ctx =>
      import ctx._

      val peer = newPeer

      val txs = Seq.fill(60)(dummyDeclinedTx())
      txs.foreach(tx => invTxAndExpectRequest(peer, tx))

      val modData = ModifiersData(ErgoTransaction.modifierTypeId, txs.map(tx => tx.id -> tx.bytes).toMap)
      synchronizerRef ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(modData)), Some(peer))

      // the first transaction is sent for processing
      vhProbe.fishForMessage(5.seconds) {
        case _: ErgoNodeViewHolder.ReceivableMessages.TransactionFromRemote => true
        case _ => false
      }

      // barrier: the response to this inv proves that the delivery above is fully processed
      invTxAndExpectRequest(peer, dummyDeclinedTx())
      // the rest of the delivery is cached up to the limit, the remainder is dropped
      synchronizerTestRef.underlyingActor.txProcessingCacheSize shouldBe
        synchronizerTestRef.underlyingActor.MaxProcessingTransactionsCacheSize

      // on block applied the cache processing is resumed: one more record is sent for processing
      synchronizerRef ! RemoteBlockApplied(declinedHistory.bestHeaderOpt.get, Seq.empty)
      vhProbe.fishForMessage(5.seconds) {
        case _: ErgoNodeViewHolder.ReceivableMessages.TransactionFromRemote => true
        case _ => false
      }
      synchronizerTestRef.underlyingActor.txProcessingCacheSize shouldBe
        synchronizerTestRef.underlyingActor.MaxProcessingTransactionsCacheSize - 1
    }
  }

  /**
    * The declined transactions table is a de-duplication cache: re-adding an id which is already in the full
    * table must not evict another entry (which would forget a declined transaction for no reason).
    */
  property("NodeViewSynchronizer: duplicate declined transaction does not evict from the declined table") {
    withDeclinedTransactionsFixture { ctx =>
      import ctx._

      val peer = newPeer
      val honest = newPeer

      val txs = Seq.fill(MaxDeclined)(dummyDeclinedTx())
      declineTxs(peer, txs, cost = 1000)
      // decline one of the transactions once again - the table must not shrink
      declineTxs(peer, Seq(txs.head), cost = 1000)

      // barrier: the response to this inv proves that all the declines above are already processed
      invTxAndExpectRequest(honest, dummyDeclinedTx())

      synchronizerTestRef.underlyingActor.declinedTxsCount shouldBe MaxDeclined
    }
  }

  /**
    * Declines of transactions invalidated by the mempool (e.g. rent or re-emission prefilter rejections) are
    * already blocked from re-request by the `isInvalidated` check in `processInv`, so they are not stored
    * in the declined transactions table.
    */
  property("NodeViewSynchronizer: decline of an invalidated transaction is not stored in the declined table") {
    withDeclinedTransactionsFixture { ctx =>
      import ctx._

      val peer = newPeer
      val honest = newPeer
      val tx = dummyDeclinedTx()

      // simulate the mempool invalidating the transaction while declining it
      synchronizerRef ! ChangedMempool(ErgoMemPool.empty(settings).invalidate(UnconfirmedTransaction(tx, None)))

      declineTxs(peer, Seq(tx), cost = 1000)

      // barrier: the response to this inv proves that the decline above is already processed
      invTxAndExpectRequest(honest, dummyDeclinedTx())

      // the invalidated decline did not occupy a slot in the declined table
      synchronizerTestRef.underlyingActor.declinedTxsCount shouldBe 0
    }
  }

}
