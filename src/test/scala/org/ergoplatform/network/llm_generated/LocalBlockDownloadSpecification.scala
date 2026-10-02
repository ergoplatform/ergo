package org.ergoplatform.network

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.consensus.Equal
import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.network.message.{InvData, InvSpec, Message, RequestModifierSpec}
import org.ergoplatform.network.message.inputblocks.{
  OrderingBlockAnnouncement,
  OrderingBlockAnnouncementMessageSpec
}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.{
  ErgoNodeViewHolder,
  LocallyGeneratedBlockSection,
  LocallyGeneratedOrderingBlock
}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.{EmptyDigest32, defaultExtension}
import org.ergoplatform.utils.ErgoNodeTestConstants.{defaultPeerSpec, initSettings}
import org.ergoplatform.utils.generators.ChainGenerator.genChain
import org.ergoplatform.utils.generators.ConnectedPeerGenerators.connectionIdGen
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{
  createUtxoState,
  validFullBlock
}
import org.ergoplatform.wallet.utils.FileUtils
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.core.network.ModifiersStatus.Requested
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.util.{bytesToId, idToBytes}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.DurationInt

class LocalBlockDownloadSpecification extends ErgoCorePropertyTest with FileUtils {

  class ViewHolder(settings: ErgoSettings) extends ErgoNodeViewHolder[UtxoState](settings) {
    def currentHistory: ErgoHistory = history()
  }

  private class Fixture {
    implicit val system: ActorSystem = ActorSystem()
    implicit val ec: ExecutionContext = system.dispatcher
    val directory: java.io.File = createTempDir
    val settings: ErgoSettings = initSettings.copy(directory = directory.getAbsolutePath)
    val network: TestProbe = TestProbe()
    val peerHandler: TestProbe = TestProbe()
    val holder: TestActorRef[ViewHolder] = TestActorRef(Props(new ViewHolder(settings)))
    val history: ErgoHistory = holder.underlyingActor.currentHistory
    val tracker: DeliveryTracker = DeliveryTracker.empty(settings)
    val syncTracker: ErgoSyncTracker = ErgoSyncTracker(settings.scorexSettings.network)
    val synchronizer: TestActorRef[ErgoNodeViewSynchronizer] = TestActorRef(
      Props(
        new ErgoNodeViewSynchronizer(
          network.ref,
          holder,
          ErgoSyncInfoMessageSpec,
          settings,
          syncTracker,
          tracker
        )
      )
    )
    val peer: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      peerHandler.ref,
      Some(PeerInfo(
        defaultPeerSpec.copy(features = Seq(
          ModePeerFeature(StateType.Utxo, verifyingTransactions = true, None, -1)
        )),
        System.currentTimeMillis()
      ))
    )

    // TestActorRef uses the calling-thread dispatcher: DownloadRequest is handled
    // before the holder resumes applying the next section, even though history is shared.
    // GetNodeViewChanges in the synchronizer's preStart initializes its real readers.
    val (state, boxes) = createUtxoState(settings)
    var generatorState: WrappedUtxoState = WrappedUtxoState(state, boxes, settings)
    val genesis: ErgoFullBlock = validFullBlock(None, generatorState)

    def requests(): Seq[InvData] = {
      network.receiveWhile(max = 1.second, idle = 50.millis) {
        case send: SendToNetwork
          if send.message.spec.messageCode == RequestModifierSpec.messageCode =>
          Some(send.message.data.get.asInstanceOf[InvData])
        case _ => None
      }.flatten
    }

    def connectPeer(): Unit = {
      syncTracker.updateStatus(peer, Equal, Some(history.fullBlockHeight))
    }

    def assertApplied(block: ErgoFullBlock): Unit = {
      history.bestFullBlockOpt.map(_.id) shouldBe Some(block.id)
      block.mandatoryBlockSections.foreach { section =>
        history.contains(section.id) shouldBe true
      }
    }

    def advance(block: ErgoFullBlock): Unit = {
      generatorState = generatorState.applyModifier(block)(_ => ()).get
    }

    def applyGenesis(): Unit = {
      holder ! LocallyGeneratedBlockSection(genesis.header)
      genesis.blockSections.foreach { section =>
        holder ! LocallyGeneratedBlockSection(section)
      }
      assertApplied(genesis)
      advance(genesis)
      requests()
      connectPeer()
    }

    def missingInputAnnouncement(): OrderingBlockAnnouncement = {
      val missingInputId = bytesToId(Array.fill(32)(0x42.toByte))
      val fields = InputBlockFields.toExtensionFields(
        Some(idToBytes(missingInputId)), EmptyDigest32, EmptyDigest32
      )
      val block = genChain(1, history, extension = defaultExtension ++ fields).head
      val announcement = OrderingBlockAnnouncement(
        OrderingBlockAnnouncement.CurrentVersion,
        block.header,
        Seq.empty,
        Seq.empty,
        block.extension.fields
      )
      announcement.valid(
        settings.chainSettings.powScheme,
        Some(settings.chainSettings.initialNBits)
      ) shouldBe true
      history.getInputBlockTransactions(missingInputId) shouldBe None
      announcement
    }

    def announce(announcement: OrderingBlockAnnouncement): Unit = {
      synchronizer ! Message(
        OrderingBlockAnnouncementMessageSpec,
        Left(OrderingBlockAnnouncementMessageSpec.toBytes(announcement)),
        Some(peer)
      )
      history.getOrderingBlockAnnouncement(announcement.header.id) shouldBe defined
    }

    def close(): Unit = {
      Await.result(system.terminate(), 30.seconds)
      generatorState.closeStorage()
      deleteRecursive(directory)
    }
  }

  private def withFixture(test: Fixture => Unit): Unit = {
    val fixture = new Fixture
    try test(fixture) finally fixture.close()
  }

  property("sealing local ordering blocks does not request their own sections") {
    withFixture { f =>
      import f._
      applyGenesis()

      val blocks = (1 to 3).foldLeft(Seq(genesis)) { (applied, _) =>
        val block = validFullBlock(Some(applied.last), generatorState)
        holder ! LocallyGeneratedOrderingBlock(block, block.transactions)
        assertApplied(block)
        advance(block)
        applied :+ block
      }.tail

      val ownSections = blocks.flatMap(_.header.sectionIds).toSet
      val ownRequests = requests().flatMap(inv => inv.ids.map(inv.typeId -> _))
        .filter(ownSections.contains)
      ownRequests shouldBe empty
    }
  }

  property("local header then sections from POST /blocks does not request its own sections") {
    withFixture { f =>
      import f._
      applyGenesis()

      val block = validFullBlock(Some(genesis), generatorState)
      holder ! LocallyGeneratedBlockSection(block.header)
      history.contains(block.header.id) shouldBe true
      history.contains(block.blockTransactions.id) shouldBe false
      // Observe requests while only the header is present, before sending any sections.
      val headerRequests = requests()
      block.blockSections.foreach { section =>
        holder ! LocallyGeneratedBlockSection(section)
      }
      assertApplied(block)

      val ownSections = block.header.sectionIds.toSet
      val ownRequests = (headerRequests ++ requests())
        .flatMap(inv => inv.ids.map(inv.typeId -> _)).filter(ownSections.contains)
      ownRequests shouldBe empty
    }
  }

  property("transactions Inv then ordering announcement requests the body once") {
    withFixture { f =>
      import f._
      connectPeer()
      requests()
      val announcement = missingInputAnnouncement()
      val header = announcement.header

      val inv = InvData(BlockTransactions.modifierTypeId, Seq(header.transactionsId))
      synchronizer ! Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(peer))
      tracker.status(
        header.transactionsId, BlockTransactions.modifierTypeId, Seq(history)
      ) shouldBe Requested
      val firstRequests = requests()
      firstRequests shouldBe Seq(inv)

      announce(announcement)
      val bodyRequests = (firstRequests ++ requests()).filter(_ == inv)
      bodyRequests should have size 1
    }
  }

  property("a remote header still requests its missing sections") {
    withFixture { f =>
      import f._
      applyGenesis()
      val block = validFullBlock(Some(genesis), generatorState)
      holder ! ModifiersFromRemote(Seq(block.header))
      val requested = requests().flatMap(inv => inv.ids.map(inv.typeId -> _)).toSet
      requested should contain(BlockTransactions.modifierTypeId -> block.header.transactionsId)
      requested should contain(block.extension.modifierTypeId -> block.header.extensionId)
    }
  }

  property("an ordering announcement still requests an unknown body") {
    withFixture { f =>
      import f._
      connectPeer()
      requests()
      val announcement = missingInputAnnouncement()
      announce(announcement)
      requests() shouldBe Seq(InvData(
        BlockTransactions.modifierTypeId, Seq(announcement.header.transactionsId)
      ))
      tracker.status(
        announcement.header.transactionsId, BlockTransactions.modifierTypeId, Seq(history)
      ) shouldBe Requested
    }
  }
}
