package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.state.{UtxoSnapshotChunk, UtxoSnapshotManifest}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.util.ScorexLogging

import scala.util.{Failure, Success, Try}

trait UtxoSnapshotManifestProcessor extends ScorexLogging with ScorexEncoding {

  protected val historyStorage: HistoryStorage

  def process(m: UtxoSnapshotManifest): ProgressInfo[ErgoPersistentModifier] = {
    val chunksToRequest = m.chunkRoots.map(UtxoSnapshotChunk.rootDigestToId).map(UtxoSnapshotChunk.modifierTypeId -> _)
    historyStorage.insertObjects(Seq(m))
    ProgressInfo(None, Seq.empty, Seq.empty, chunksToRequest)
  }

  def validate(m: UtxoSnapshotManifest): Try[Unit] = if (historyStorage.contains(m.id)) {
    Failure(new Exception(s"UtxoSnapshotManifest with id ${m.encodedId} is already in history"))
  } else {
    historyStorage.modifierById(m.blockId) match {
      case Some(_: Header) => Success(())
      case _ => Failure(new Exception("Header manifest relates to is not found in history"))
    }
  }

}
