package org.ergoplatform.nodeView.history.components

import org.ergoplatform.nodeView.history.storage.HistoryStorage

trait Persistence {

  private[history] val storage: HistoryStorage

}
