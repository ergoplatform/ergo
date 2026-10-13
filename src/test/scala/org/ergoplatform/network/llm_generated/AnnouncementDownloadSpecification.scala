package org.ergoplatform.network.llm_generated

import akka.actor.{ActorRef, Props}
import akka.testkit.{TestActorRef, TestProbe}
import java.net.InetSocketAddress
import org.ergoplatform.consensus.Equal
import org.ergoplatform.mining.{CandidateGenerator, InputBlockFields}
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import org.ergoplatform.modifiers.{ErgoFullBlock, NetworkObjectTypeId}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ProcessOrderingBlock
import org.ergoplatform.network.message.inputblocks.{
  OrderingBlockAnnouncement,
  OrderingBlockAnnouncementMessageSpec
}
import org.ergoplatform.network.message.{
  InvData,
  InvSpec,
  Message,
  ModifiersData,
  ModifiersSpec,
  RequestModifierSpec
}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.network.{
  ErgoNodeViewSynchronizer,
  ErgoSyncTracker,
  ModePeerFeature
}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.DownloadRequest
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{
  GetDataFromCurrentView,
  ModifiersFromRemote
}
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.state.{DigestState, StateType, UtxoState}
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, LocallyGeneratedBlockSection}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.defaultMinerPk
import org.ergoplatform.utils.ErgoNodeTestConstants.{defaultPeerSpec, initSettings}
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{
  createUtxoState,
  validFullBlock
}
import org.ergoplatform.wallet.utils.FileUtils
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}
import scorex.core.network.NetworkController.ReceivableMessages.{
  PenalizePeer,
  SendToNetwork
}
import scorex.core.network.{ConnectedPeer, ConnectionId, DeliveryTracker, Outgoing}
import scorex.testkit.utils.AkkaFixture
import scorex.util.ModifierId

/** The synchronizer must be idle when the holder receives the announcement. Sending it
  * through a calling-thread synchronizer would queue DownloadRequest until after the local
  * section appends, hiding the bug. Direct holder delivery handles each event immediately.
  */
class AnnouncementDownloadSpecification extends ErgoCorePropertyTest with FileUtils {
  private case object NetworkDrained

  private class UtxoHolder(settings: ErgoSettings)
    extends ErgoNodeViewHolder[UtxoState](settings)

  private class DigestHolder(settings: ErgoSettings)
    extends ErgoNodeViewHolder[DigestState](settings)

  private class Synchronizer(
    network: ActorRef,
    holder: ActorRef,
    settings: ErgoSettings,
    tracker: ErgoSyncTracker,
    delivery: DeliveryTracker
  )(implicit ec: ExecutionContext)
    extends ErgoNodeViewSynchronizer(
      network,
      holder,
      ErgoSyncInfoMessageSpec,
      settings,
      tracker,
      delivery
    ) {
    // Same sender as the real network messages: a barrier, not a timed negative assertion.
    def drainNetwork(): Unit = network ! NetworkDrained
  }

  private class Fixture(stateType: StateType) extends AkkaFixture {
    implicit val ec: ExecutionContext = system.dispatcher

    val nodeSettings: ErgoSettings = initSettings.copy(
      directory = createTempDir.getAbsolutePath,
      nodeSettings = initSettings.nodeSettings
        .copy(stateType = stateType, verifyTransactions = true, blocksToKeep = -1),
      scorexSettings = initSettings.scorexSettings.copy(
        network = initSettings.scorexSettings.network
          .copy(syncInterval = 1.hour, deliveryTimeout = 1.hour)
      )
    )

    // Fixed reward transactions, real state roots and proofs; no sampled generators.
    private val (initialState, _) = createUtxoState(nodeSettings)
    val genesis: ErgoFullBlock    = rewardBlock(None, initialState)
    private val afterGenesis      = initialState.applyModifier(genesis, None)(_ => ()).get
    val block: ErgoFullBlock      = rewardBlock(Some(genesis), afterGenesis)
    afterGenesis.closeStorage()

    private def rewardBlock(
      parent: Option[ErgoFullBlock],
      state: UtxoState
    ): ErgoFullBlock = {
      val rewards = CandidateGenerator.collectRewards(
        state.emissionBoxOpt,
        parent.map(_.header.height).getOrElse(0),
        Seq.empty,
        defaultMinerPk,
        state.stateContext
      )
      validFullBlock(
        parent,
        state,
        rewards,
        Some(
          parent.map(_.header.timestamp + 1).getOrElse(System.currentTimeMillis() - 1000)
        )
      )
    }

    val holder: ActorRef = stateType match {
      case StateType.Utxo   => TestActorRef(Props(new UtxoHolder(nodeSettings)))
      case StateType.Digest => TestActorRef(Props(new DigestHolder(nodeSettings)))
    }
    (genesis.header +: genesis.blockSections).foreach { section =>
      holder ! LocallyGeneratedBlockSection(section)
    }

    val network  = TestProbe()
    val events   = TestProbe()
    val tracker  = ErgoSyncTracker(nodeSettings.scorexSettings.network)
    val delivery = DeliveryTracker.empty(nodeSettings)

    val peer = ConnectedPeer(
      ConnectionId(
        new InetSocketAddress("127.0.0.1", 9030),
        new InetSocketAddress("127.0.0.1", 9031),
        Outgoing
      ),
      TestProbe().ref,
      Some(
        PeerInfo(
          defaultPeerSpec.copy(features =
            Seq(ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1))
          ),
          System.currentTimeMillis()
        )
      )
    )
    tracker.updateStatus(peer, Equal, Some(genesis.header.height))

    val synchronizer: TestActorRef[Synchronizer] = TestActorRef(
      Props(new Synchronizer(network.ref, holder, nodeSettings, tracker, delivery))
    )
    system.eventStream.subscribe(events.ref, classOf[DownloadRequest])
    snapshot() shouldBe ((true, false, false, genesis.header.id))
    networkMessages()

    val announcement: OrderingBlockAnnouncement = OrderingBlockAnnouncement(
      1,
      block.header,
      block.transactions,
      Seq.empty,
      block.extension.fields
    )

    def snapshot(): (Boolean, Boolean, Boolean, ModifierId) = {
      val probe = TestProbe()
      probe.send(
        holder,
        GetDataFromCurrentView[Any, (Boolean, Boolean, Boolean, ModifierId)] { view =>
          (
            view.history.isHeadersChainSynced,
            view.history.contains(block.header.id),
            view.history.contains(block.blockTransactions.id),
            view.history.bestFullBlockOpt.get.header.id
          )
        }
      )
      probe.expectMsgType[(Boolean, Boolean, Boolean, ModifierId)]
    }

    def networkMessages(): Vector[Any] = {
      synchronizer.underlyingActor.drainNetwork()
      val messages = network
        .receiveWhile(5.seconds, messages = Int.MaxValue) {
          case message if message != NetworkDrained => message
        }
        .toVector
      network.expectMsg(NetworkDrained)
      messages
    }

    def requests(messages: Seq[Any]): Map[NetworkObjectTypeId.Value, Seq[ModifierId]] = {
      messages
        .collect {
          case SendToNetwork(message, _) if message.spec == RequestModifierSpec =>
            message.data.get.asInstanceOf[InvData]
        }
        .groupBy(_.typeId)
        .map { case (typeId, invs) => typeId -> invs.flatMap(_.ids) }
    }

    def peerReplies(
      requests: Map[NetworkObjectTypeId.Value, Seq[ModifierId]]
    ): Vector[Any] = {
      val sections = (block.header +: block.blockSections).map(s => s.id -> s).toMap
      requests.foreach {
        case (typeId, ids) =>
          val bytes = ids.map(id => id -> sections(id).bytes).toMap
          synchronizer ! Message(
            ModifiersSpec,
            Left(ModifiersSpec.toBytes(ModifiersData(typeId, bytes))),
            Some(peer)
          )
      }
      networkMessages()
    }

    def downloadMaps(): Vector[Map[NetworkObjectTypeId.Value, Seq[ModifierId]]] = {
      events.ref ! NetworkDrained
      val maps = events
        .receiveWhile(5.seconds, messages = Int.MaxValue) {
          case request: DownloadRequest => request.modifiersToFetch
        }
        .toVector
      events.expectMsg(NetworkDrained)
      maps
    }
  }

  private def withFixture(
    stateType: StateType = StateType.Utxo
  )(test: Fixture => Unit): Unit = {
    val fixture = new Fixture(stateType)
    try test(fixture)
    finally Await.result(fixture.system.terminate(), 10.seconds)
  }

  property("announcement only: a synced UTXO follower requests none of its own sections") {
    withFixture() { f =>
      f.holder ! ProcessOrderingBlock(f.announcement)
      val requests = f.requests(f.networkMessages())
      f.snapshot() shouldBe ((true, true, true, f.block.header.id))
      requests shouldBe empty
    }
  }

  property(
    "announcement only: replies to issued requests after local apply incur no penalty"
  ) {
    withFixture() { f =>
      f.holder ! ProcessOrderingBlock(f.announcement)
      val requests = f.requests(f.networkMessages())
      f.snapshot()._4 shouldBe f.block.header.id
      val penalties = f.peerReplies(requests).collect { case p: PenalizePeer => p }
      penalties shouldBe empty
    }
  }

  property("digest follower still requests ADProofs, but not transactions or extension") {
    withFixture(StateType.Digest) { f =>
      f.holder ! ProcessOrderingBlock(f.announcement)
      val requests = f.requests(f.networkMessages())
      f.snapshot() shouldBe ((true, true, true, f.genesis.header.id))
      val penalties = f.peerReplies(requests).collect { case p: PenalizePeer => p }
      f.snapshot()._4 shouldBe f.block.header.id
      requests shouldBe Map(ADProofs.modifierTypeId -> Seq(f.block.header.ADProofsId))
      penalties shouldBe empty
    }
  }

  property("root mismatch still explicitly requests the full transaction body") {
    withFixture() { f =>
      val broken = f.announcement.copy(nonBroadcastedTransactions = Seq.empty)
      f.holder ! ProcessOrderingBlock(broken)
      f.requests(f.networkMessages()).get(BlockTransactions.modifierTypeId) shouldBe
      Some(Seq(f.block.header.transactionsId))
      f.downloadMaps() should contain(
        Map(
          BlockTransactions.modifierTypeId ->
          Seq(f.block.header.transactionsId)
        )
      )
      f.snapshot()._3 shouldBe false
    }
  }

  property(
    "missing broadcast transactions still explicitly requests the full transaction body"
  ) {
    withFixture() { f =>
      val missing = f.announcement.copy(
        nonBroadcastedTransactions = Seq.empty,
        broadcastedTransactionIds  = f.block.transactions.map(_.id)
      )
      f.holder ! ProcessOrderingBlock(missing)
      f.requests(f.networkMessages()).get(BlockTransactions.modifierTypeId) shouldBe
      Some(Seq(f.block.header.transactionsId))
      f.downloadMaps() should contain(
        Map(
          BlockTransactions.modifierTypeId ->
          Seq(f.block.header.transactionsId)
        )
      )
      f.snapshot()._3 shouldBe false
    }
  }

  property(
    "missing previous input block still requests the full body through the synchronizer"
  ) {
    withFixture() { f =>
      val fields = InputBlockFields.toExtensionFields(
        Some(Array.fill(32)(1.toByte)),
        f.block.header.transactionsRoot,
        f.block.header.transactionsRoot
      )
      val extension =
        Extension(f.block.header.id, f.block.extension.fields ++ fields.fields)
      val header = f.block.header.copy(extensionRoot = extension.digest)
      val missing =
        f.announcement.copy(header = header, extensionFields = extension.fields)
      f.synchronizer ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(OrderingBlockAnnouncementMessageSpec.toBytes(missing)),
        Some(f.peer)
      )
      f.requests(f.networkMessages()).get(BlockTransactions.modifierTypeId) shouldBe
      Some(Seq(header.transactionsId))
    }
  }

  property("ordinary remote header still requests its transactions and extension") {
    withFixture() { f =>
      f.holder ! ModifiersFromRemote(Seq(f.block.header))
      f.requests(f.networkMessages()) shouldBe Map(
        BlockTransactions.modifierTypeId -> Seq(f.block.header.transactionsId),
        Extension.modifierTypeId         -> Seq(f.block.header.extensionId)
      )
    }
  }

  property(
    "Inv first still requests all sections and penalizes late replies (scope: #2654)"
  ) {
    withFixture() { f =>
      f.block.header.sectionIds.foreach {
        case (typeId, id) =>
          f.synchronizer ! Message(
            InvSpec,
            Left(InvSpec.toBytes(InvData(typeId, Seq(id)))),
            Some(f.peer)
          )
      }
      val requests = f.requests(f.networkMessages())
      requests shouldBe f.block.header.sectionIds.map {
        case (typeId, id) => typeId -> Seq(id)
      }
      f.holder ! ProcessOrderingBlock(f.announcement)
      f.requests(f.networkMessages()) shouldBe empty
      f.snapshot()._4 shouldBe f.block.header.id
      f.peerReplies(requests).collect { case p: PenalizePeer => p } should have size 3
    }
  }
}
