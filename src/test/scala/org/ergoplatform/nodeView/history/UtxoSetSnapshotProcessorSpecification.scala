package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.UtxoSetSnapshotProcessor
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.HistoryTestHelpers
import scorex.util.ModifierId

class UtxoSetSnapshotProcessorSpecification extends HistoryTestHelpers {

  private val s = settings

  val utxoSetSnapshotProcessor = new UtxoSetSnapshotProcessor {
    override protected val settings: ErgoSettings = s
    override protected val historyStorage: HistoryStorage = HistoryStorage(settings)
    override private[history] var minimalFullBlockHeightVar = ErgoHistory.GenesisHeight
  }

  property("registerManifestToDownload + getUtxoSetSnapshotDownloadPlan + getChunkIdsToDownload") {
    val bh     = boxesHolderGen.sample.get
    val us     = createUtxoState(bh, parameters)
    val (manifest, subtrees) = us.slicedTree()
    println(s"Subtrees: ${subtrees.size}")
    val snapshotHeight = 1
    utxoSetSnapshotProcessor.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    val dp = utxoSetSnapshotProcessor.getUtxoSetSnapshotDownloadPlan().get
    dp.snapshotHeight shouldBe snapshotHeight
    val subtreeIds = subtrees.map(s => ModifierId @@ Algos.encode(s.id))
    val expected = dp.expectedChunkIds.map(id => ModifierId @@ Algos.encode(id))
    expected shouldBe subtreeIds
    val toDownload = utxoSetSnapshotProcessor.getChunkIdsToDownload(expected.size).map(id => ModifierId @@ Algos.encode(id))
    toDownload shouldBe expected
  }

}
