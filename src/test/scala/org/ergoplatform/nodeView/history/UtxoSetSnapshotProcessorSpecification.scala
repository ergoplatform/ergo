package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.UtxoSetSnapshotProcessor
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.HistoryTestHelpers

class UtxoSetSnapshotProcessorSpecification extends HistoryTestHelpers {

  private val s = settings

  val utxoSetSnapshotProcessor = new UtxoSetSnapshotProcessor {
    override protected val settings: ErgoSettings = s
    override protected val historyStorage: HistoryStorage = HistoryStorage(settings)
    override private[history] var minimalFullBlockHeightVar = ErgoHistory.GenesisHeight
  }

  property("registerManifestToDownload + getUtxoSetSnapshotDownloadPlan + getChunkIdsToDownload") {
    ???
  }

}
