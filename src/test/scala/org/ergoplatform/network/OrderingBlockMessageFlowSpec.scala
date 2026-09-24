package org.ergoplatform.network

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, LocallyGeneratedOrderingBlock}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalacheck.Gen
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.{ConnectedPeer, DeliveryTracker, SendToPeers}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.consensus.{Equal, Fork, Younger}
import scorex.testkit.utils.AkkaFixture

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor}

/**
  * Tests for message flow of input/ordering blocks synchronization.
  * 
  * Tests verify:
  * - Ordering block announcement propagation to appropriate peers
  * - Input block message flow and processing
  * - FullBlockApplied event chain and downstream effects
  */
class OrderingBlockMessageFlowSpec extends AnyPropSpec
  with Matchers
  with FileUtils
  with Eventually {

  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.HistoryTestHelpers._

  val wrappedUtxoStateGen: Gen[org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState] =
    boxesHolderGen.map(org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState(_, createTempDir, parameters, settings))

  private def withFixture(testCode: SynchronizerFixture => Any): Unit = {
    val fixture = new SynchronizerFixture
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

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 500.millis)

  def nodeViewSynchronizer(implicit system: ActorSystem):
  (ActorRef, ActorRef, ConnectedPeer, TestProbe, TestProbe, TestProbe, DeliveryTracker, ErgoSyncTracker) = {
    val settings = ErgoSettingsReader.read()
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

    val peerInfo = PeerInfo(defaultPeerSpec, System.currentTimeMillis())
    val p: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      pchProbe.ref,
      Some(peerInfo)
    )

    (synchronizerMockRef, nodeViewHolderMockRef, p, pchProbe, ncProbe, eventListener, deliveryTracker, syncTracker)
  }

  class SynchronizerFixture extends AkkaFixture {
    val (synchronizer, nodeViewHolder, peer, pchProbe, ncProbe, eventListener, deliveryTracker, syncTracker) = nodeViewSynchronizer
  }

  // ============================================================================
  // Ordering Block Announcement Propagation Tests
  // ============================================================================

  property("ordering block announcement forwarded only to Equal status peers") {
    withFixture { fixture =>
      import fixture._

      // Setup: node at height 10
      val localHistory = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1)
      val fullChain = genChain(10, localHistory)
      fullChain.foreach { block =>
        localHistory.append(block.header).get
        block.blockSections.foreach(section => localHistory.append(section).get)
      }

      synchronizer ! ChangedHistory(localHistory)
      synchronizer ! ChangedMempool(ErgoMemPool.empty(settings))

      // Register two peers: one Equal, one Younger
      val peerEqual = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis()))
      )
      val peerYounger = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis()))
      )

      syncTracker.updateStatus(peerEqual, Equal, Some(10))
      syncTracker.updateStatus(peerYounger, Younger, Some(5))

      // Create and send ordering block
      val wrappedState = wrappedUtxoStateGen.sample.get
      val currentBlock = validFullBlock(fullChain.lastOption, wrappedState)
      synchronizer ! LocallyGeneratedOrderingBlock(currentBlock, Seq.empty)

      // Verify ordering block announcement sent only to Equal peer
      eventually(timeout(5.seconds)) {
        val msg = ncProbe.expectMsgClass(3.seconds, classOf[SendToNetwork])
        msg.message.spec.messageCode shouldBe OrderingBlockAnnouncementMessageSpec.messageCode
        msg.sendingStrategy match {
          case SendToPeers(peers) =>
            peers should contain(peerEqual)
            peers should not contain peerYounger
          case _ => fail("Expected SendToPeers strategy")
        }
      }
    }
  }

  property("ordering block announcement forwarded to Fork status peers") {
    withFixture { fixture =>
      import fixture._

      // Setup: node at height 10
      val localHistory = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1)
      val fullChain = genChain(10, localHistory)
      fullChain.foreach { block =>
        localHistory.append(block.header).get
        block.blockSections.foreach(section => localHistory.append(section).get)
      }

      synchronizer ! ChangedHistory(localHistory)
      synchronizer ! ChangedMempool(ErgoMemPool.empty(settings))

      // Register peer on fork
      val peerFork = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis()))
      )

      syncTracker.updateStatus(peerFork, Fork, Some(10))

      // Create and send ordering block
      val wrappedState = wrappedUtxoStateGen.sample.get
      val currentBlock = validFullBlock(fullChain.lastOption, wrappedState)
      synchronizer ! LocallyGeneratedOrderingBlock(currentBlock, Seq.empty)

      // Verify ordering block announcement sent to Fork peer
      eventually(timeout(5.seconds)) {
        val msg = ncProbe.expectMsgClass(3.seconds, classOf[SendToNetwork])
        msg.message.spec.messageCode shouldBe OrderingBlockAnnouncementMessageSpec.messageCode
        msg.sendingStrategy match {
          case SendToPeers(peers) => peers should contain(peerFork)
          case _ => fail("Expected SendToPeers strategy")
        }
      }
    }
  }

  property("no ordering block announcement sent when no eligible peers") {
    withFixture { fixture =>
      import fixture._

      // Setup: node at height 10
      val localHistory = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1)
      val fullChain = genChain(10, localHistory)
      fullChain.foreach { block =>
        localHistory.append(block.header).get
        block.blockSections.foreach(section => localHistory.append(section).get)
      }

      synchronizer ! ChangedHistory(localHistory)
      synchronizer ! ChangedMempool(ErgoMemPool.empty(settings))

      // Register only Younger peer (not eligible for ordering block announcements)
      val peerYounger = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis()))
      )

      syncTracker.updateStatus(peerYounger, Younger, Some(5))

      // Create and send ordering block
      val wrappedState = wrappedUtxoStateGen.sample.get
      val currentBlock = validFullBlock(fullChain.lastOption, wrappedState)
      synchronizer ! LocallyGeneratedOrderingBlock(currentBlock, Seq.empty)

      // Verify either no message or message with empty peer list
      ncProbe.fishForMessage(2.seconds) { msg =>
        msg match {
          case stn: SendToNetwork if stn.message.spec.messageCode == OrderingBlockAnnouncementMessageSpec.messageCode =>
            stn.sendingStrategy match {
              case SendToPeers(peers) => peers shouldBe empty
              case _ => // other strategies are ok
            }
            true
          case _: SendToNetwork => false
          case _ => false
        }
      }
    }
  }

  // ============================================================================
  // Input Block Message Flow Tests
  // ============================================================================

  property("ordering block processed when input blocks already available") {
    withFixture { fixture =>
      import fixture._

      // Setup: node at height 10
      val localHistory = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1)
      val fullChain = genChain(10, localHistory)
      fullChain.foreach { block =>
        localHistory.append(block.header).get
        block.blockSections.foreach(section => localHistory.append(section).get)
      }

      synchronizer ! ChangedHistory(localHistory)
      synchronizer ! ChangedMempool(ErgoMemPool.empty(settings))

      // Subscribe to FullBlockApplied events
      system.eventStream.subscribe(eventListener.ref, classOf[FullBlockApplied])

      // Create ordering block at height 11
      val wrappedState = wrappedUtxoStateGen.sample.get
      val nextBlock = validFullBlock(fullChain.lastOption, wrappedState)

      // Simulate scenario where input blocks are already stored
      // (In real scenario, input blocks would arrive before ordering block announcement)
      
      // Send ordering block (input blocks assumed to be available)
      nodeViewHolder ! LocallyGeneratedOrderingBlock(nextBlock, Seq.empty)

      // Verify FullBlockApplied is published (indicating successful processing)
      val fullBlockAppliedMsg = eventListener.expectMsgClass(15.seconds, classOf[FullBlockApplied])
      fullBlockAppliedMsg.header.id shouldBe nextBlock.header.id
    }
  }


  // ============================================================================
  // FullBlockApplied Event Chain Tests
  // ============================================================================


  property("FullBlockApplied contains correct header information") {
    withFixture { fixture =>
      import fixture._

      // Setup: node at height 10
      val localHistory = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1)
      val fullChain = genChain(10, localHistory)
      fullChain.foreach { block =>
        localHistory.append(block.header).get
        block.blockSections.foreach(section => localHistory.append(section).get)
      }

      synchronizer ! ChangedHistory(localHistory)
      synchronizer ! ChangedMempool(ErgoMemPool.empty(settings))

      // Subscribe to FullBlockApplied
      system.eventStream.subscribe(eventListener.ref, classOf[FullBlockApplied])

      // Create ordering block at height 11
      val wrappedState = wrappedUtxoStateGen.sample.get
      val nextBlock = validFullBlock(fullChain.lastOption, wrappedState)

      // Send ordering block
      nodeViewHolder ! LocallyGeneratedOrderingBlock(nextBlock, Seq.empty)

      // Verify FullBlockApplied header details
      val fullBlockAppliedMsg = eventListener.expectMsgClass(15.seconds, classOf[FullBlockApplied])
      
      fullBlockAppliedMsg.header.id shouldBe nextBlock.header.id
      fullBlockAppliedMsg.header.height shouldBe 11
      fullBlockAppliedMsg.header.parentId shouldBe fullChain.last.header.id
      fullBlockAppliedMsg.header.stateRoot shouldBe nextBlock.header.stateRoot
    }
  }

}
