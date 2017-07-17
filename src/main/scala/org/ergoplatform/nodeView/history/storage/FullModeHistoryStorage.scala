package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.LSMStore

class FullModeHistoryStorage(protected val storage: LSMStore) extends HistoryStorage

