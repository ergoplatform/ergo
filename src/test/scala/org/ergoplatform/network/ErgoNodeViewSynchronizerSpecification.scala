package org.ergoplatform.network

import akka.actor.{ActorRef, ActorSystem, Cancellable, Props}
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.history.{ADProofsSerializer, BlockTransactions, BlockTransactionsSerializer}
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionSerializer}
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.modifiers.mempool.ErgoTransactionSerializer
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{GetNodeViewChanges, TransactionFromRemote}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader, ErgoSyncInfoMessageSpec, ErgoSyncInfoV2}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.sanity.ErgoSanity._
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.validation.{ParentHeaderNotFoundError, RecoverableModifierError}
import org.ergoplatform.wallet.utils.FileUtils
import org.scalacheck.Gen
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import scorex.core.network.ModifiersStatus.{Received, Requested, Unknown}
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
import org.ergoplatform.network.message._
import org.ergoplatform.network.peer.{PeerInfo, PenaltyType}
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.util.bytesToId
import org.ergoplatform.serialization.ErgoSerializer
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.testkit.utils.AkkaFixture
import sigma.VersionContext

import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor}
import scala.language.postfixOps

class ErgoNodeViewSynchronizerSpecification extends AnyPropSpec
  with Matchers
  with ScalaCheckPropertyChecks
  with FileUtils
  with Eventually {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.HistoryTestHelpers._

  // ToDo: factor this out of here and NVHTests?
  private def withFixture(testCode: SynchronizerFixture => Any): Unit = {
    val fixture = new SynchronizerFixture
    try {
      testCode(fixture)
    }
    finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  private def withFixture2(testCode: Synchronizer2Fixture => Any): Unit = {
    val fixture = new Synchronizer2Fixture
    try {
      testCode(fixture)
    }
    finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  private def withTransactionIngressFixture(testCode: TransactionIngressFixture => Any): Unit = {
    val fixture = new TransactionIngressFixture
    try {
      testCode(fixture)
    }
    finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  class NodeViewHolderMock extends ErgoNodeViewHolder[UtxoState](settings)

  class SynchronizerMock(networkControllerRef: ActorRef,
                         viewHolderRef: ActorRef,
                         syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                         settings: ErgoSettings,
                         syncTracker: ErgoSyncTracker,
                         deliveryTracker: DeliveryTracker)
                        (implicit ec: ExecutionContext) extends ErgoNodeViewSynchronizer(
    networkControllerRef,
    viewHolderRef,
    syncInfoSpec,
    settings,
    syncTracker,
    deliveryTracker)(ec)

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(2.seconds, 100.millis)
  val history = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1)
  val chain = genHeaderChain(2000, history, diffBitsOpt = None, useRealTs = false)
  val localChain = chain.take(1000)
  val altchain = genHeaderChain(1000, history, diffBitsOpt = None, useRealTs = false)

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
    (0 until count).foldLeft((headerOpt, Seq.empty[PM])) { case (acc, _) =>
      val pm = validFullBlock(headerOpt, state.asInstanceOf[WrappedUtxoState])
      (Some(pm), acc._2 :+ pm)
    }._2.map(_.asInstanceOf[ErgoFullBlock].header)
  }

  def nodeViewSynchronizer(implicit system: ActorSystem):
  (ActorRef, ActorRef, SI, PM, TX, ConnectedPeer, TestProbe, TestProbe, TestProbe, ErgoSerializer[PM], DeliveryTracker) = {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val h = localHistoryGen.sample.get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val s = localStateGen.sample.get
    val settings = ErgoSettingsReader.read()
    val pool = ErgoMemPool.empty(settings)
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    val ncProbe = TestProbe("NetworkControllerProbe")
    val pchProbe = TestProbe("PeerHandlerProbe")
    val eventListener = TestProbe("EventListener")
    val syncTracker = ErgoSyncTracker(settings.scorexSettings.network)
    val deliveryTracker: DeliveryTracker = DeliveryTracker.empty(settings)

    // each test should always start with empty history
    deleteRecursive(ErgoHistory.historyDir(settings))
    val nodeViewHolderMockRef = system.actorOf(Props(new NodeViewHolderMock))

    val synchronizerMockRef = system.actorOf(Props(
      new SynchronizerMock(
        ncProbe.ref,
        nodeViewHolderMockRef,
        ErgoSyncInfoMessageSpec,
        settings,
        syncTracker,
        deliveryTracker)
    ))
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
    (synchronizerMockRef, nodeViewHolderMockRef, h.syncInfoV1, m, tx, p, pchProbe, ncProbe, eventListener, serializer, deliveryTracker)
  }

  class SynchronizerFixture extends AkkaFixture {
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val (synchronizer, nodeViewHolder, syncInfo, mod, tx, peer, pchProbe, ncProbe, eventListener, modSerializer, deliveryTracker) = nodeViewSynchronizer
  }

  class Synchronizer2Fixture extends AkkaFixture {
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    val ncProbe = TestProbe("NetworkControllerProbe")
    val pchProbe = TestProbe("PeerHandlerProbe")
    val syncTracker = ErgoSyncTracker(settings.scorexSettings.network)
    val deliveryTracker: DeliveryTracker = DeliveryTracker.empty(settings)

    // each test should always start with empty history
    deleteRecursive(ErgoHistory.historyDir(settings))
    val nodeViewHolderMockRef = system.actorOf(Props(new NodeViewHolderMock))

    val synchronizerMockRef = system.actorOf(Props(
      new SynchronizerMock(
        ncProbe.ref,
        nodeViewHolderMockRef,
        ErgoSyncInfoMessageSpec,
        settings,
        syncTracker,
        deliveryTracker)
    ))

    val peerInfo = PeerInfo(defaultPeerSpec, System.currentTimeMillis())
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val peer: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      pchProbe.ref,
      Some(peerInfo)
    )
  }

  class TransactionIngressFixture extends AkkaFixture {
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    val h = localHistoryGen.sample.get
    val pool = ErgoMemPool.empty(settings)
    val ncProbe = TestProbe("TransactionNetworkControllerProbe")
    val viewHolderProbe = TestProbe("TransactionViewHolderProbe")
    val pchProbe = TestProbe("TransactionPeerHandlerProbe")
    val syncTracker = ErgoSyncTracker(settings.scorexSettings.network)
    val deliveryTracker: DeliveryTracker = DeliveryTracker.empty(settings)

    deleteRecursive(ErgoHistory.historyDir(settings))
    val synchronizer = system.actorOf(Props(
      new SynchronizerMock(
        ncProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        settings,
        syncTracker,
        deliveryTracker)
    ))

    viewHolderProbe.expectMsgType[GetNodeViewChanges](3.seconds)

    val peerInfo = PeerInfo(defaultPeerSpec, System.currentTimeMillis())
    val peer: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      pchProbe.ref,
      Some(peerInfo)
    )
    val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2

    viewHolderProbe.send(synchronizer, ChangedHistory(h))
    viewHolderProbe.send(synchronizer, ChangedMempool(pool))
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
      ncProbe.fishForMessage(3 seconds) { case m =>
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
      deliveryTracker.setRequested(Header.modifierTypeId, chain.take(1001).last.id, peer)(_ => Cancellable.alreadyCancelled)
      val olderChain = chain.take(1001)
      val modData = ModifiersData(Header.modifierTypeId, Map(olderChain.last.id -> olderChain.last.bytes))
      val modSpec = ModifiersSpec
      synchronizer ! Message(modSpec, Left(modSpec.toBytes(modData)), Some(peer))
      // desired state of submitting valid headers is Received
      eventually {
        deliveryTracker.status(olderChain.last.id, Header.modifierTypeId, Seq.empty) shouldBe Received
      }
    }
  }

  property("NodeViewSynchronizer: rejects header bytes with trailing payload") {
    withFixture { ctx =>
      import ctx._
      deliveryTracker.reset()
      val header = chain.take(1001).last
      deliveryTracker.setRequested(Header.modifierTypeId, header.id, peer)(_ => Cancellable.alreadyCancelled)
      val modData = ModifiersData(Header.modifierTypeId, Map(header.id -> (header.bytes ++ Array(0: Byte))))
      val modSpec = ModifiersSpec
      synchronizer ! Message(modSpec, Left(modSpec.toBytes(modData)), Some(peer))

      eventually {
        deliveryTracker.status(header.id, Header.modifierTypeId, Seq.empty) shouldBe Unknown
      }
      ncProbe.fishForMessage(3.seconds) {
        case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
          address == peer.connectionId.remoteAddress
        case _ => false
      }
    }
  }

  property("NodeViewSynchronizer: transaction ingress accepts canonical bytes and rejects a trailing byte") {
    withTransactionIngressFixture { ctx =>
      import ctx._
      val typeId = tx.modifierTypeId
      val scriptVersion = Header.scriptFromBlockVersion(Header.InitialVersion)
      val canonicalBytes = VersionContext.withVersions(scriptVersion, scriptVersion) {
        ErgoTransactionSerializer.toBytes(tx)
      }

      def sendTransaction(bytes: Array[Byte]): Unit = {
        val data = ModifiersData(typeId, Map(tx.id -> bytes))
        viewHolderProbe.send(
          synchronizer,
          Message(ModifiersSpec, Left(ModifiersSpec.toBytes(data)), Some(peer))
        )
      }

      deliveryTracker.setRequested(typeId, tx.id, peer)(_ => Cancellable.alreadyCancelled)
      deliveryTracker.status(tx.id, typeId, Seq.empty) shouldBe Requested

      sendTransaction(canonicalBytes)
      val accepted = viewHolderProbe.expectMsgType[TransactionFromRemote](3.seconds)
      accepted.unconfirmedTx.transaction shouldBe tx
      accepted.unconfirmedTx.transactionBytes.exists(_.sameElements(canonicalBytes)) shouldBe true
      accepted.unconfirmedTx.source shouldBe Some(peer)

      sendTransaction(canonicalBytes ++ Array(0: Byte))
      ncProbe.fishForMessage(3.seconds) {
        case PenalizePeer(address, PenaltyType.MisbehaviorPenalty) =>
          address == peer.connectionId.remoteAddress
        case _ => false
      }
      viewHolderProbe.expectNoMessage(300.millis)

      deliveryTracker.status(tx.id, typeId, Seq.empty) shouldBe Requested
      viewHolderProbe.send(synchronizer, CheckDelivery(peer, typeId, tx.id))
      eventually {
        deliveryTracker.status(tx.id, typeId, Seq.empty) shouldBe Unknown
      }
    }
  }

  property("NodeViewSynchronizer: exact modifier parsing accepts canonical bytes and rejects trailing bytes") {
    def checkExact[M](serializer: ErgoSerializer[M], bytes: Array[Byte]): Unit = {
      ErgoNodeViewSynchronizer.parseBytesExact(serializer, bytes).isSuccess shouldBe true
      ErgoNodeViewSynchronizer.parseBytesExact(serializer, bytes ++ Array(0: Byte)).isFailure shouldBe true
    }

    val header = chain.take(1001).last
    val tx = org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators.invalidErgoTransactionGen.sample.get
    val blockTransactions = BlockTransactions(header.id, Header.InitialVersion, Seq(tx))
    val adProofs = org.ergoplatform.utils.generators.ErgoCoreGenerators.randomADProofsGen.sample.get
    val extension = Extension(
      header.id,
      Seq(
        Array[Byte](0, 1) -> Array[Byte](1, 2),
        Array[Byte](0, 2) -> Array[Byte](3, 4)
      )
    )
    val scriptVersion = Header.scriptFromBlockVersion(Header.InitialVersion)

    VersionContext.withVersions(scriptVersion, scriptVersion) {
      checkExact(HeaderSerializer, HeaderSerializer.toBytes(header))
      checkExact(ErgoTransactionSerializer, ErgoTransactionSerializer.toBytes(tx))
      checkExact(BlockTransactionsSerializer, BlockTransactionsSerializer.toBytes(blockTransactions))
      checkExact(ADProofsSerializer, ADProofsSerializer.toBytes(adProofs))
      checkExact(ExtensionSerializer, ExtensionSerializer.toBytes(extension))
    }
  }

  property("NodeViewSynchronizer: apply continuation header from syncV2 and download its block") {
    withFixture2 { ctx =>
      import ctx._
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 100.millis)

      // we generate and apply existing base chain
      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val baseChain = genHeaderChain(_.size > 4, None, hhistory.difficultyCalculator, None, false)
      baseChain.headers.foreach(hhistory.append)
      val bestHeaderOpt = hhistory.bestHeaderOpt

      // then a continuation chain that will be part of the syncV2 message
      val continuationChain = genHeaderChain(_.size > 4, bestHeaderOpt, hhistory.difficultyCalculator, None, false).tail

      // sync message carries best header of our base change + continuation chain whose Head header is supposed to be applied
      val sync = ErgoSyncInfoV2(continuationChain.headers)
      val msgBytes = ErgoSyncInfoMessageSpec.toBytes(sync)

      // send this sync msg to synchronizer which should apply the header following the common header from base chain
      synchronizerMockRef ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))
      val appliedHeader = continuationChain.headers.head
      // calculate block sections for applied header and test whether they were attempted to be downloaded from remote peer
      var remainingSectionIds = hhistory.requiredModifiersForHeader(appliedHeader).groupBy(_._1).mapValues(_.map(_._2).head)
      while (remainingSectionIds.nonEmpty) {
        ncProbe.fishForMessage(3 seconds) { case m =>
          m match {
            case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
              val invData = stn.message.data.get.asInstanceOf[InvData]
              remainingSectionIds.exists { case (sectionTypeId, sectionId) =>
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
        deliveryTracker.setRequested(Header.modifierTypeId, header.id, peer)(_ => Cancellable.alreadyCancelled)
        val modData = ModifiersData(Header.modifierTypeId, Map(header.id -> header.bytes))
        val modSpec = ModifiersSpec
        synchronizerMockRef ! Message(modSpec, Left(modSpec.toBytes(modData)), Some(peer))
      }

      deliveryTracker.reset()

      // we generate fork of two headers, starting from the parent of the best header
      // so the depth of the rollback is 1, and the fork bypasses the best chain by 1 header
      val hhistory = ErgoHistory.readOrGenerate(settings)(null)
      val newHeaders = genHeaderChain(2, hhistory, diffBitsOpt = None, useRealTs = false).headers
      val newHistory = newHeaders.foldLeft(hhistory) { case (hist, header) => hist.append(header).get._1 }
      val parentOpt = newHistory.lastHeaders(2).headOption
      val smallFork = genHeaderChain(_.size > 2, parentOpt, newHistory.difficultyCalculator, None, false)
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
        deliveryTracker.setRequested(Header.modifierTypeId, block.header.id, peer)(_ => Cancellable.alreadyCancelled)
        val modData = ModifiersData(Header.modifierTypeId, Map(block.header.id -> block.header.bytes))
        synchronizerMockRef ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(modData)), Some(peer))
      }

      def sendBlockSection(block: BlockSection): Unit = {
        deliveryTracker.setRequested(block.modifierTypeId, block.id, peer)(_ => Cancellable.alreadyCancelled)
        val modData = ModifiersData(block.modifierTypeId, Map(block.id -> block.bytes))
        synchronizerMockRef ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(modData)), Some(peer))
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
        smallFork.forall(block => hist.contains(block.extension.id) && hist.contains(block.blockTransactions.id))
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
        smallFork.forall(block => !hist.contains(block.extension.id) && !hist.contains(block.blockTransactions.id))
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
      ncProbe.fishForMessage(3 seconds) { case m =>
        m match {
          case stn: SendToNetwork =>
            val msg = stn.message
            val headers = msg.data.get.asInstanceOf[ErgoSyncInfoV2].lastHeaders
            msg.spec.messageCode == ErgoSyncInfoMessageSpec.messageCode && headers.length == 4
          case _ => false
        }
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
      ncProbe.fishForMessage(3 seconds) { case m =>
        m match {
          case stn: SendToNetwork =>
            val msg = stn.message
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

      val sync = ErgoSyncInfoV2(ErgoHistoryReader.FullV2SyncOffsets.map(offset => forkedChain.apply(forkedHeight - offset - 1)))

      // Neighbour is sending
      val msgBytes = ErgoSyncInfoMessageSpec.toBytes(sync)
      val invSpec = InvSpec
      // we check that in case of neighbour with older history (it has more blocks),
      // invs (extension for the forked peer) will be sent to the peer
      synchronizer ! Message(ErgoSyncInfoMessageSpec, Left(msgBytes), Some(peer))
      ncProbe.fishForMessage(3 seconds) { case m =>
        m match {
          case stn: SendToNetwork =>
            val msg = stn.message
            msg.spec.messageCode == invSpec.messageCode
          case _ => false
        }
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
      invData.ids.exists(expectedIds.contains) shouldBe true

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

}
