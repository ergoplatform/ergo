package org.ergoplatform.network

import akka.actor.{ActorIdentity, ActorSystem, Identify, Props}
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.network.message._
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{SnapshotTestAccess, SnapshotsInfo, StateType, UtxoState}
import org.ergoplatform.serialization.ManifestSerializer
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestContext, NodeViewTestOps}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.wallet.utils.TestFileUtils
import org.scalatest.concurrent.Eventually
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.{ConnectedPeer, DeliveryTracker, SendToPeer, SendToPeers}
import scorex.crypto.hash.Digest32
import scorex.util.bytesToId

import java.io.File
import java.net.InetSocketAddress
import scala.concurrent.duration._

class SnapshotBootstrapJoinSpec extends ErgoCorePropertyTest
  with NodeViewTestOps with TestFileUtils with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants.defaultPeerSpec
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._

  property("early snapshot-only offers recover through the real holder") {
    val snapshotHeight = 19
    val defaults = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
      popowBootstrap = false, utxoBootstrap = true).toSettings
    val bootstrapSettings = defaults.copy(chainSettings = defaults.chainSettings.copy(
      voting = defaults.chainSettings.voting.copy(votingLength = snapshotHeight + 1),
      epochLength = snapshotHeight + 1))

    new NodeViewFixture(bootstrapSettings, parameters).apply { fixture =>
      implicit val ctx: NodeViewTestContext = fixture
      implicit val system: ActorSystem = fixture.actorSystem
      val sourceDir = createTempDir
      val sourceSettings = fixture.settings.copy(directory = sourceDir.getAbsolutePath)
      val boxes = boxesHolderGenOfSize(9216).sample.get
      val sourceStateDir = new File(sourceDir, "state")
      sourceStateDir.mkdirs() shouldBe true
      val source = UtxoState.fromBoxHolder(boxes, None, sourceStateDir,
        sourceSettings, parameters)

      try {
        val manifest = SnapshotTestAccess.dumpManifest(source, snapshotHeight,
          ManifestSerializer.MainnetManifestDepth)
        manifest.subtreesIds should not be empty
        val expectedChunks = manifest.subtreesIds.map(bytesToId).toSet
        val manifestBytes = source.getManifestBytes(manifest.id).get

        val history = getHistory
        val generated = genHeaderChain(snapshotHeight, history,
          diffBitsOpt = None, useRealTs = false).headers
        val template = generated.last
        val anchor = powScheme.prove(Some(generated(snapshotHeight - 2)),
          template.version, template.nBits, source.rootDigest, template.ADProofsRoot,
          template.transactionsRoot, template.timestamp, template.extensionRoot,
          template.votes, defaultMinerSecretNumber).get
        val network = TestProbe("SnapshotNetwork")
        val firstHandler = TestProbe("FirstSnapshotHandler")
        val secondHandler = TestProbe("SecondSnapshotHandler")
        val snapshotOnlyMode = ModePeerFeature(StateType.Utxo,
          verifyingTransactions = true, nipopowBootstrapped = None,
          blocksToKeep = ModePeerFeature.UTXOSetBootstrapped)
        val peerInfo = PeerInfo(defaultPeerSpec.copy(features = Seq(snapshotOnlyMode)),
          System.currentTimeMillis())
        val firstPeer = ConnectedPeer(connectionIdGen.sample.get, firstHandler.ref, Some(peerInfo))
        val secondPeer = firstPeer.copy(
          connectionId = firstPeer.connectionId.copy(
            remoteAddress = new InetSocketAddress("127.0.0.2", 28444)),
          handlerRef = secondHandler.ref)
        BlockSectionsDownloadFilter.condition(firstPeer) shouldBe false
        UtxoSetNetworkingFilter.condition(firstPeer) shouldBe true
        val syncTracker = ErgoSyncTracker(fixture.settings.scorexSettings.network)
        val deliveryTracker = DeliveryTracker.empty(fixture.settings)
        val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
          network.ref, fixture.nodeViewHolderRef, ErgoSyncInfoMessageSpec,
          fixture.settings, syncTracker, deliveryTracker)(fixture.executionContext)))

        try {
          val infoBytes = SnapshotsInfoSpec.toBytes(
            new SnapshotsInfo(Map(snapshotHeight -> manifest.id)))
          val barrier = TestProbe("EarlySnapshotOfferBarrier")
          history.bestHeaderIdAtHeight(snapshotHeight) shouldBe None
          barrier.send(synchronizer, ChangedHistory(history))
          barrier.send(synchronizer, ChangedMempool(ErgoMemPool.empty(fixture.settings)))
          barrier.send(synchronizer, ChangedState(getCurrentState.getReader))
          barrier.send(synchronizer, HandshakedPeer(firstPeer))
          barrier.send(synchronizer, HandshakedPeer(secondPeer))
          barrier.send(synchronizer, Message(SnapshotsInfoSpec, Left(infoBytes), Some(firstPeer)))
          barrier.send(synchronizer, Message(SnapshotsInfoSpec, Left(infoBytes), Some(secondPeer)))
          barrier.send(synchronizer, Identify("early-offers-processed"))
          barrier.expectMsgType[ActorIdentity].correlationId shouldBe "early-offers-processed"
          network.receiveWhile(200.millis) { case message => message }

          applyHeaderChain(history, HeaderChain(generated.dropRight(1) :+ anchor))
          val selectedHistory = getHistory
          selectedHistory.bestHeaderIdAtHeight(snapshotHeight) shouldBe Some(anchor.id)
          selectedHistory.bestFullBlockOpt shouldBe None
          selectedHistory.isUtxoSnapshotApplied shouldBe false
          selectedHistory.setHeadersChainSynced()
          barrier.send(synchronizer, ChangedHistory(selectedHistory))
          barrier.send(synchronizer, ErgoNodeViewSynchronizer.CheckModifiersToDownload)
          network.fishForMessage(10.seconds) {
            case sent: SendToNetwork if
                sent.message.spec.messageCode == GetSnapshotsInfoSpec.messageCode =>
              sent.sendingStrategy match {
                case targets: SendToPeers =>
                  targets.chosenPeers.map(_.handlerRef).toSet ==
                    Set(firstPeer.handlerRef, secondPeer.handlerRef)
                case _ => false
              }
            case _ => false
          }

          barrier.send(synchronizer, Message(SnapshotsInfoSpec, Left(infoBytes), Some(firstPeer)))
          barrier.send(synchronizer, Message(SnapshotsInfoSpec, Left(infoBytes), Some(secondPeer)))
          val manifestRequest = network.fishForMessage(10.seconds) {
            case request: SendToNetwork =>
              request.message.spec.messageCode == GetManifestSpec.messageCode
            case _ => false
          }.asInstanceOf[SendToNetwork]
          val manifestPeer = manifestRequest.sendingStrategy.asInstanceOf[SendToPeer].chosenPeer
          manifestRequest.message.data.get.asInstanceOf[Array[Byte]].toSeq shouldBe manifest.id.toSeq
          synchronizer ! Message(ManifestSpec, Left(ManifestSpec.toBytes(manifestBytes)), Some(manifestPeer))

          var remaining = expectedChunks
          while (remaining.nonEmpty) {
            val request = network.fishForMessage(20.seconds) {
              case sent: SendToNetwork =>
                sent.message.spec.messageCode == GetUtxoSnapshotChunkSpec.messageCode &&
                  remaining.contains(bytesToId(sent.message.data.get.asInstanceOf[Array[Byte]]))
              case _ => false
            }.asInstanceOf[SendToNetwork]
            val chunkId = request.message.data.get.asInstanceOf[Array[Byte]]
            val peer = request.sendingStrategy.asInstanceOf[SendToPeer].chosenPeer
            val chunkBytes = source.getUtxoSnapshotChunkBytes(Digest32 @@ chunkId).get
            synchronizer ! Message(UtxoSnapshotChunkSpec,
              Left(UtxoSnapshotChunkSpec.toBytes(chunkBytes)), Some(peer))
            remaining -= bytesToId(chunkId)
          }

          eventually(timeout(30.seconds), interval(200.millis)) {
            getCurrentState.rootDigest.toSeq shouldBe source.rootDigest.toSeq
            getHistory.isUtxoSnapshotApplied shouldBe true
            getHistory.minimalFullBlockHeight shouldBe snapshotHeight + 1
          }
          val restored = getCurrentState.asInstanceOf[UtxoState]
          boxes.sortedBoxes.foreach(box => restored.boxById(box.id) shouldBe Some(box))
        } finally {
          val shutdown = TestProbe("SnapshotSynchronizerShutdown")
          shutdown.watch(synchronizer)
          system.stop(synchronizer)
          shutdown.expectTerminated(synchronizer, 10.seconds)
        }
      } finally {
        source.closeStorage()
      }
    }
  }
}
