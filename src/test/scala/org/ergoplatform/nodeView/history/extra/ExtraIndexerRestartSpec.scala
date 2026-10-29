package org.ergoplatform.nodeView.history.extra

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.ErgoNodeViewHolder.CurrentView
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.state.ErgoStateReader
import org.ergoplatform.settings.{CacheSettings, NetworkType}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.generators.ValidBlocksGenerators.createDigestState
import org.ergoplatform.wallet.utils.TestFileUtils

import scala.concurrent.duration._

/** A supervised restart must recover the one-time history initialization. */
class ExtraIndexerRestartSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants.{settings => baseSettings}

  private case object IsInitialized
  private case object FailOnce

  private class RestartProbeIndexer(cacheSettings: CacheSettings,
                                    addressEncoder: ErgoAddressEncoder,
                                    nodeViewHolderRef: ActorRef)
    extends ExtraIndexer(cacheSettings, addressEncoder, Some(nodeViewHolderRef), true) {
    override def receive: Receive = super.receive.orElse {
      case IsInitialized => sender() ! (_history ne null)
      case FailOnce => throw new IllegalStateException("injected actor failure")
    }
  }

  property("startup and supervised restart pull a fresh applied-state snapshot") {
    new TestKit(ActorSystem()) {
      val dir = createTempDir
      val settings = baseSettings.copy(
        directory = dir.getAbsolutePath,
        networkType = NetworkType.TestNet,
        nodeSettings = baseSettings.nodeSettings.copy(extraIndex = false, headerChainDiff = 5000))
      val history = ErgoHistory.readOrGenerate(settings)(null)
      val chainState = ChainGenerator.generate(5, dir, history, None)
      val viewHolderProbe = TestProbe()
      val indexer = system.actorOf(Props(new RestartProbeIndexer(
        settings.cacheSettings, settings.chainSettings.addressEncoder, viewHolderProbe.ref)))
      val probe = TestProbe()

      // Answers the view request of the indexer the way the node view holder does, with a state
      // which has applied the best chain of `history` up to `height` only.
      def replyWithStateAt(height: Int): Header = {
        val request = viewHolderProbe.expectMsgType[GetDataFromCurrentView[_, _]]
          .asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        val applied = history.bestHeaderAtHeight(height).get
        val state = createDigestState(idToVersion(applied.id), applied.stateRoot)
        try viewHolderProbe.send(indexer, request.f(CurrentView[ErgoStateReader](history, state, null, null)))
        finally state.close()
        applied
      }

      // The index reaches the applied block and stops there, below the best full block.
      def awaitIndexedExactly(applied: Header): Unit = {
        applied.height should be < history.fullBlockHeight
        awaitAssert(IndexerState.fromHistory(history).indexedHeaderId shouldBe Some(applied.id), 10.seconds, 50.millis)
        Thread.sleep(1000) // nothing above it follows
        IndexerState.fromHistory(history).indexedHeaderId shouldBe Some(applied.id)
        IndexerState.fromHistory(history).indexedHeight shouldBe applied.height
      }

      try {
        val appliedAtStart = replyWithStateAt(2)
        probe.send(indexer, IsInitialized)
        probe.expectMsg(true)
        awaitIndexedExactly(appliedAtStart)

        probe.send(indexer, FailOnce)
        val appliedAtRestart = replyWithStateAt(4)
        probe.awaitAssert({
          probe.send(indexer, IsInitialized)
          probe.expectMsg(1.second, true)
        }, 5.seconds, 100.millis)
        awaitIndexedExactly(appliedAtRestart)
      } finally {
        TestKit.shutdownActorSystem(system)
        history.closeStorage()
        chainState.closeStorage()
      }
    }
  }
}
