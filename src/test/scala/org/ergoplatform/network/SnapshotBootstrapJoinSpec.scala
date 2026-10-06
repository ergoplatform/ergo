package org.ergoplatform.network

import akka.actor.{ActorSystem, Props}
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
import scorex.core.network.{ConnectedPeer, DeliveryTracker, SendToPeer}
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

  property("snapshot manifest and chunks import through the real holder") {
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
        applyHeaderChain(history, HeaderChain(generated.dropRight(1) :+ anchor))
        getHistory.bestHeaderIdAtHeight(snapshotHeight) shouldBe Some(anchor.id)
        getHistory.isUtxoSnapshotApplied shouldBe false

        val network = TestProbe("SnapshotNetwork")
        val firstHandler = TestProbe("FirstSnapshotHandler")
        val secondHandler = TestProbe("SecondSnapshotHandler")
        val peerInfo = PeerInfo(defaultPeerSpec, System.currentTimeMillis())
        val firstPeer = ConnectedPeer(connectionIdGen.sample.get, firstHandler.ref, Some(peerInfo))
        val secondPeer = firstPeer.copy(
          connectionId = firstPeer.connectionId.copy(
            remoteAddress = new InetSocketAddress("127.0.0.2", 28444)),
          handlerRef = secondHandler.ref)
        val syncTracker = ErgoSyncTracker(fixture.settings.scorexSettings.network)
        val deliveryTracker = DeliveryTracker.empty(fixture.settings)
        val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
          network.ref, fixture.nodeViewHolderRef, ErgoSyncInfoMessageSpec,
          fixture.settings, syncTracker, deliveryTracker)(fixture.executionContext)))

        synchronizer ! ChangedHistory(history)
        synchronizer ! ChangedMempool(ErgoMemPool.empty(fixture.settings))
        synchronizer ! ChangedState(getCurrentState.getReader)

        val infoBytes = SnapshotsInfoSpec.toBytes(
          new SnapshotsInfo(Map(snapshotHeight -> manifest.id)))
        synchronizer ! Message(SnapshotsInfoSpec, Left(infoBytes), Some(firstPeer))
        synchronizer ! Message(SnapshotsInfoSpec, Left(infoBytes), Some(secondPeer))
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
        source.closeStorage()
      }
    }
  }
}
