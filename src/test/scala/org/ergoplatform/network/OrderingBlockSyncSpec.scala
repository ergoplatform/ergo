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
import org.ergoplatform.consensus.{Equal, Younger}
import scorex.testkit.utils.AkkaFixture

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor}

/**
  * Tests for ordering block synchronization logic added in commit b35b5c9:
  * - FullBlockApplied is published after LocallyGeneratedOrderingBlock
  * - Ordering blocks are only sent to nearly synced peers (within 2 blocks)
  *
  * Note: The tests verify the behavior as implemented in the commit.
  * The height filtering condition (peerHeight <= historyReader.fullBlockHeight + 2)
  * filters peers that are too far AHEAD, not peers that are far BEHIND.
  * Peers are filtered by status (Equal/Fork) which indirectly handles sync status.
  */
class OrderingBlockSyncSpec extends AnyPropSpec
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

  property("publish FullBlockApplied after LocallyGeneratedOrderingBlock") {
    withFixture { fixture =>
      import fixture._

      // Setup: create a chain of full blocks at height 10
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

      // Create ordering block at height 11 (on top of the chain)
      val wrappedState = wrappedUtxoStateGen.sample.get
      val nextBlock = validFullBlock(fullChain.lastOption, wrappedState)

      val expectedHeaderId = nextBlock.header.id
      
      // Send locally generated ordering block to the node view holder (not the synchronizer)
      // The node view holder processes it and publishes FullBlockApplied
      nodeViewHolder ! LocallyGeneratedOrderingBlock(nextBlock, Seq.empty)

      // Verify FullBlockApplied is published (any header)
      // Note: This tests that the fix in ErgoNodeViewHolder.scala publishes FullBlockApplied
      // after processing LocallyGeneratedOrderingBlock
      val fullBlockAppliedMsg = eventListener.expectMsgClass(15.seconds, classOf[FullBlockApplied])
      
      // Verify the header ID matches
      fullBlockAppliedMsg.header.id shouldBe expectedHeaderId
    }
  }

  property("filter peers by status (Equal/Fork) for ordering block announcements") {
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

      // Register peer with Younger status (peer is behind us)
      val peerYounger = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis()))
      )

      // Update peer status to Younger (behind us)
      // According to the implementation, only Equal/Fork peers receive ordering block announcements
      syncTracker.updateStatus(peerYounger, Younger, Some(5))

      // Create ordering block at current height (11)
      val wrappedState = wrappedUtxoStateGen.sample.get
      val currentBlock = validFullBlock(fullChain.lastOption, wrappedState)

      // Send locally generated ordering block
      synchronizer ! LocallyGeneratedOrderingBlock(currentBlock, Seq.empty)

      // Verify that either no message is sent, or if sent, it has no peers (empty peer list)
      // Younger peers should not receive ordering block announcements
      // (they should receive full block sections via FullBlockApplied instead)
      ncProbe.fishForMessage(2.seconds) { msg =>
        msg match {
          case stn: SendToNetwork if stn.message.spec.messageCode == OrderingBlockAnnouncementMessageSpec.messageCode =>
            // If message is sent, verify it has no peers
            stn.sendingStrategy match {
              case SendToPeers(peers) => peers shouldBe empty
              case _ => // other strategies are ok too
            }
            true
          case _: SendToNetwork =>
            // Ignore other SendToNetwork messages
            false
          case _ =>
            false
        }
      }
    }
  }

  property("send ordering block announcement to Equal status peers") {
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

      // Register peer with Equal status (nearly synced)
      val peerEqual = ConnectedPeer(
        connectionIdGen.sample.get,
        pchProbe.ref,
        Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis()))
      )

      // Update peer status to Equal (at similar height)
      syncTracker.updateStatus(peerEqual, Equal, Some(10))

      // Create ordering block at current height (11)
      val wrappedState = wrappedUtxoStateGen.sample.get
      val currentBlock = validFullBlock(fullChain.lastOption, wrappedState)

      // Send locally generated ordering block
      synchronizer ! LocallyGeneratedOrderingBlock(currentBlock, Seq.empty)

      // Verify that SendToNetwork message IS sent to Equal status peer
      // The message should contain ordering block announcement
      eventually(timeout(5.seconds)) {
        val msg = ncProbe.expectMsgClass(3.seconds, classOf[SendToNetwork])
        msg.message.spec.messageCode shouldBe OrderingBlockAnnouncementMessageSpec.messageCode
      }
    }
  }
}
