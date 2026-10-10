package org.ergoplatform.nodeView.history

import scorex.db.ByteArrayWrapper
import scorex.util.ModifierId

/** Test-only loss of one persisted section, including the history cache entry. */
object HistorySectionFault {
  def removeTransactionSection(history: ErgoHistory, sectionId: ModifierId): Unit =
    history.synchronized {
      history.historyStorage.remove(Array.empty[ByteArrayWrapper], Array(sectionId)).get
    }
}
