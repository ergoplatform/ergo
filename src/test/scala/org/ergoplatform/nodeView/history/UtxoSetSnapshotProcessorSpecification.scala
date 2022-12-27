package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.UtxoSetSnapshotProcessor
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.HistoryTestHelpers
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSerializer
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId

import scala.util.Random

class UtxoSetSnapshotProcessorSpecification extends HistoryTestHelpers {

  private val s = settings

  val utxoSetSnapshotProcessor = new UtxoSetSnapshotProcessor {
    override protected val settings: ErgoSettings = s
    override protected val historyStorage: HistoryStorage = HistoryStorage(settings)
    override private[history] var minimalFullBlockHeightVar = ErgoHistory.GenesisHeight
  }

  property("registerManifestToDownload + getUtxoSetSnapshotDownloadPlan + getChunkIdsToDownload") {
    val bh     = boxesHolderGenOfSize(65536).sample.get
    val us     = createUtxoState(bh, parameters)
    val (manifest, subtrees) = us.slicedTree()
    println("Subtrees: " + subtrees.size)
    val snapshotHeight = 1
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(Random.nextInt(100).toByte))
    utxoSetSnapshotProcessor.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    val dp = utxoSetSnapshotProcessor.getUtxoSetSnapshotDownloadPlan().get
    dp.snapshotHeight shouldBe snapshotHeight
    val subtreeIds = subtrees.map(s => ModifierId @@ Algos.encode(s.id))
    val expected = dp.expectedChunkIds.map(id => ModifierId @@ Algos.encode(id))
    expected shouldBe subtreeIds
    val toDownload = utxoSetSnapshotProcessor.getChunkIdsToDownload(expected.size).map(id => ModifierId @@ Algos.encode(id))
    toDownload shouldBe expected

    val serializer = new BatchAVLProverSerializer[Digest32, HF]()(Algos.hash)
    subtrees.foreach { subtree =>
      utxoSetSnapshotProcessor.registerDownloadedChunk(subtree.id, serializer.subtreeToBytes(subtree))
    }
    utxoSetSnapshotProcessor.downloadedChunksIterator().toSeq.map(s => ModifierId @@ Algos.encode(s.id)) shouldBe subtreeIds
    val restoredState = utxoSetSnapshotProcessor.createPersistentProver(us.store, blockId).get
    bh.sortedBoxes.foreach { box =>
      restoredState.unauthenticatedLookup(box.id).isDefined shouldBe true
    }
    restoredState.checkTree(postProof = false)
  }

}
