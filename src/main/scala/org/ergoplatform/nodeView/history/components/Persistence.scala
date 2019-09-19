package org.ergoplatform.nodeView.history.components

import org.ergoplatform.nodeView.history.storage.HistoryStorage

/**
  * A component proving an access to persistent storage.
  */
trait Persistence {

  private[history] val storage: HistoryStorage

}
