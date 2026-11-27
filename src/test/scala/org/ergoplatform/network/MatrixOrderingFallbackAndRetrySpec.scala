package org.ergoplatform.network

import akka.actor.{ActorRef, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.AutolykosSolution
import org.ergoplatform.consensus.Equal
import org.ergoplatform.mining.{AutolykosPowScheme, InputBlockFields}
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.{
  ErgoFullBlock,
  InputBlockTransactionIdsTypeId,
  InputBlockTypeId,
  OrderingBlockAnnouncementTypeId
}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.network.message.inputblocks.{
  InputBlockTransactionIdsData,
  InputBlockTransactionIdsMessageSpec,
  InputBlockTransactionsData,
  InputBlockTransactionsMessageSpec,
  OrderingBlockAnnouncement,
  OrderingBlockAnnouncementMessageSpec
}
import org.ergoplatform.network.message.{InvData, InvSpec, Message, RequestModifierSpec}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.{ConnectedPeer, DeliveryTracker, ModifiersStatus, SendToPeer}
import scorex.crypto.hash.Digest32
import scorex.testkit.utils.AkkaFixture
import scorex.util.bytesToId

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}

class MatrixOrderingFallbackAndRetrySpec extends AnyPropSpec with Matchers with FileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._

  class SynchronizerUnderTest(
    networkControllerRef: ActorRef,
    viewHolderRef: ActorRef,
    settings: ErgoSettings,
    syncTracker: ErgoSyncTracker,
    deliveryTracker: DeliveryTracker
  )(implicit ec: ExecutionContextExecutor)
    extends ErgoNodeViewSynchronizer(
      networkControllerRef,
      viewHolderRef,
      ErgoSyncInfoMessageSpec,
      settings,
      syncTracker,
      deliveryTracker
    )(ec)

  class Fixture(sendUtxoState: Boolean = true, requestTimeout: FiniteDuration = 30.seconds)
    extends AkkaFixture {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val historySettings: ErgoSettings = settings.copy(directory = createTempDir.getAbsolutePath)
    private val cs                    = historySettings.chainSettings
    val realPowScheme                 = new AutolykosPowScheme(cs.powScheme.k, cs.powScheme.n)
    val synchronizerSettings: ErgoSettings = historySettings.copy(
      chainSettings = cs.copy(powScheme = realPowScheme),
      scorexSettings = historySettings.scorexSettings.copy(
        network = historySettings.scorexSettings.network.copy(deliveryTimeout = requestTimeout)
      )
    )

    val ncProbe         = TestProbe("NetworkControllerProbe")
    val viewHolderProbe = TestProbe("ViewHolderProbe")
    val pchProbe        = TestProbe("PeerHandlerProbe")
    val syncTracker     = ErgoSyncTracker(synchronizerSettings.scorexSettings.network)
    val deliveryTracker = DeliveryTracker.empty(synchronizerSettings)

    val synchronizer: TestActorRef[SynchronizerUnderTest] = TestActorRef(
      Props(
        new SynchronizerUnderTest(
          ncProbe.ref,
          viewHolderProbe.ref,
          synchronizerSettings,
          syncTracker,
          deliveryTracker
        )
      )
    )

    val peer: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      pchProbe.ref,
      Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis()))
    )

    val subBlockPeer: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      TestProbe("SubBlockPeer").ref,
      Some(
        PeerInfo(
          PeerSpec(
            synchronizerSettings.scorexSettings.network.agentName,
            Version.SubblocksVersion,
            synchronizerSettings.scorexSettings.network.nodeName,
            None,
            Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
          ),
          System.currentTimeMillis()
        )
      )
    )

    val hist: ErgoHistory = ErgoHistory.readOrGenerate(historySettings)(null)
    val chain: Seq[ErgoFullBlock] =
      genChain(3, hist, nBits = historySettings.chainSettings.initialNBits)
    applyChain(hist, chain)

    val state: WrappedUtxoState =
      boxesHolderGen.map(WrappedUtxoState(_, createTempDir, parameters, historySettings)).sample.get

    if (sendUtxoState) {
      synchronizer ! ChangedState(state)
    }
    synchronizer ! ChangedHistory(hist)
    synchronizer ! ChangedMempool(ErgoMemPool.empty(historySettings))
    Thread.sleep(300)
    viewHolderProbe.receiveWhile(max = 300.millis, idle = 100.millis) { case m => m }
    ncProbe.receiveWhile(max = 300.millis, idle = 100.millis) { case m => m }
  }

  private def withFixture(f: Fixture)(test: Fixture => Any): Unit =
    try test(f)
    finally Await.result(f.system.terminate(), Duration.Inf)

  property(
    "processOrderingBlockAnnouncement fallback submits header before extension and requests BlockTransactions"
  ) {
    withFixture(new Fixture) { f =>
      val parentHeader = f.hist.bestFullBlockOpt.get.header
      val expectedNBits =
        DifficultySerializer.encodeCompactBits(f.hist.requiredDifficultyAfter(parentHeader))
      val digest        = Digest32 @@ Array.fill(32)(0.toByte)
      val missingPrevId = Array.fill(32)(0xAB.toByte)
      val ibExtension   = InputBlockFields.toExtensionFields(Some(missingPrevId), digest, digest)
      val block = nextBlock(
        f.hist.bestFullBlockOpt,
        f.chain.head.blockTransactions.txs,
        defaultExtension ++ ibExtension
      )
      val sol = block.header.powSolution
      val header = block.header.copy(
        version = Header.Interpreter60Version,
        parentId = parentHeader.id,
        height = parentHeader.height + 1,
        nBits = expectedNBits,
        powSolution = new AutolykosSolution(sol.pk, sol.w, Array.fill(8)(0: Byte), sol.d)
      )
      val oba = OrderingBlockAnnouncement(
        OrderingBlockAnnouncement.CurrentVersion,
        header,
        Seq.empty,
        Seq.empty,
        block.extension.fields
      )
      oba.valid(f.realPowScheme, Some(expectedNBits)) shouldBe true

      f.synchronizer ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(OrderingBlockAnnouncementMessageSpec.toBytes(oba)),
        Some(f.peer)
      )

      val vhMessages = f.viewHolderProbe.receiveWhile(max = 1.second, idle = 300.millis) {
        case m => m
      }
      val remoteModifiers = vhMessages.collect { case ModifiersFromRemote(mods) => mods }.flatten
      remoteModifiers.length shouldBe 2
      remoteModifiers.head shouldBe oba.header
      remoteModifiers(1) match {
        case ext: Extension => ext.headerId shouldBe oba.header.id
        case other          => fail(s"Expected Extension second, got $other")
      }
      f.deliveryTracker.status(oba.header.id, Header.modifierTypeId, Seq.empty) shouldBe
        ModifiersStatus.Received
      f.deliveryTracker.status(
        Extension(oba.header.id, oba.extensionFields).id,
        Extension.modifierTypeId,
        Seq.empty
      ) shouldBe ModifiersStatus.Received

      val netMessages = f.ncProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }
      val txReq = netMessages.collect {
        case SendToNetwork(msg, SendToPeer(target))
            if msg.spec.messageCode == RequestModifierSpec.messageCode =>
          (msg.data.get.asInstanceOf[InvData], target)
      }
      txReq should contain(
        (InvData(BlockTransactions.modifierTypeId, Seq(oba.header.transactionsId)), f.peer)
      )
    }
  }

  property(
    "CheckDelivery re-requests Matrix modifier types via another subblock peer and preserves Requested tracking"
  ) {
    withFixture(new Fixture) { f =>
      f.syncTracker.updateStatus(f.subBlockPeer, Equal, Some(f.hist.fullBlockHeight))

      val typesAndIds = Seq(
        (OrderingBlockAnnouncementTypeId.value, bytesToId(Array.fill(32)(0x11.toByte)), 0),
        (InputBlockTypeId.value, bytesToId(Array.fill(32)(0x22.toByte)), 2),
        (InputBlockTransactionIdsTypeId.value, bytesToId(Array.fill(32)(0x33.toByte)), 0)
      )

      typesAndIds.foreach { case (typeId, modId, initialChecks) =>
        f.synchronizer.underlyingActor.requestBlockSection(typeId, Seq(modId), f.peer, initialChecks)
        f.ncProbe.expectMsgClass(3.seconds, classOf[SendToNetwork])

        f.synchronizer ! CheckDelivery(f.peer, typeId, modId)

        val retry = f.ncProbe.fishForMessage(3.seconds) {
          case SendToNetwork(msg, _) if msg.spec.messageCode == RequestModifierSpec.messageCode =>
            val inv = msg.data.get.asInstanceOf[InvData]
            inv.typeId == typeId && inv.ids == Seq(modId)
          case _ => false
        }.asInstanceOf[SendToNetwork]

        retry.sendingStrategy shouldBe SendToPeer(f.subBlockPeer)
        f.deliveryTracker.status(modId, typeId, Seq.empty) shouldBe ModifiersStatus.Requested
        val reqInfo = f.deliveryTracker.getRequestedInfo(typeId, modId).get
        reqInfo.peer shouldBe f.subBlockPeer
        reqInfo.checks shouldBe (initialChecks + 1)
      }
    }
  }

  property(
    "digest mode ignores incoming input block transaction IDs and transactions messages without forwarding to view holder"
  ) {
    withFixture(new Fixture(sendUtxoState = false)) { f =>
      val ibId = bytesToId(Array.fill(32)(0x44.toByte))
      val inv  = InvData(InputBlockTransactionIdsTypeId.value, Seq(ibId))
      f.synchronizer ! Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(f.peer))
      f.ncProbe.expectMsgClass(3.seconds, classOf[SendToNetwork])
      f.deliveryTracker.status(ibId, InputBlockTransactionIdsTypeId.value, Seq.empty) shouldBe
        ModifiersStatus.Requested

      val idsData = InputBlockTransactionIdsData(ibId, Seq.empty)
      f.synchronizer ! Message(
        InputBlockTransactionIdsMessageSpec,
        Left(InputBlockTransactionIdsMessageSpec.toBytes(idsData)),
        Some(f.peer)
      )
      val txsData = InputBlockTransactionsData(ibId, Seq.empty)
      f.synchronizer ! Message(
        InputBlockTransactionsMessageSpec,
        Left(InputBlockTransactionsMessageSpec.toBytes(txsData)),
        Some(f.peer)
      )

      val vhMessages = f.viewHolderProbe.receiveWhile(max = 500.millis, idle = 200.millis) {
        case m => m
      }
      vhMessages.exists(_.isInstanceOf[ProcessInputBlockTransactions]) shouldBe false
      f.deliveryTracker.status(ibId, InputBlockTransactionIdsTypeId.value, Seq.empty) shouldBe
        ModifiersStatus.Unknown
    }
  }
}
