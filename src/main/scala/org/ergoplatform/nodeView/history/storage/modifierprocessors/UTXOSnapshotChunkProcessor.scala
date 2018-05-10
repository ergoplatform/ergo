package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

/**
  * Contains all functions required by History to process UTXOSnapshotChunk
  */
trait UTXOSnapshotChunkProcessor {

  protected val historyStorage: HistoryStorage

  def process(m: UTXOSnapshotChunk): ProgressInfo[ErgoPersistentModifier] = {
    //TODO
    val toInsert = ???
    historyStorage.insert(ByteArrayWrapper(m.id), Seq.empty, toInsert)
    ProgressInfo(None, Seq.empty, Seq(m), Seq.empty)
  }

  def validate(m: UTXOSnapshotChunk): Try[Unit] = if (historyStorage.contains(m.id)) {
    Failure(new Error(s"UTXOSnapshotChunk with id ${Base58.encode(m.id)} is already in history"))
  } else {
    Success()
  }

}
