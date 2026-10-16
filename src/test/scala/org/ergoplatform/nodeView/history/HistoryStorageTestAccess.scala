package org.ergoplatform.nodeView.history

import scorex.util.ModifierId

import scala.util.Try

private[nodeView] object HistoryStorageTestAccess {

  def removeModifier(history: ErgoHistory, id: ModifierId): Try[Unit] =
    history.historyStorage.remove(Array.empty, Array(id))
}
