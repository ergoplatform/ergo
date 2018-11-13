package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.state.{UtxoSnapshot, UtxoSnapshotChunk, UtxoSnapshotManifest}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.util.{ModifierId, ScorexLogging}

import scala.util.{Failure, Success, Try}

trait UtxoSnapshotChunkProcessor extends ScorexLogging with ScorexEncoding {

  protected val historyStorage: HistoryStorage

  private val emptyProgressInfo = ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)

  def process(m: UtxoSnapshotChunk): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.modifierById(m.manifestId) match {
      case Some(manifest: UtxoSnapshotManifest) =>
        historyStorage.insert(Algos.idToBAW(m.id), Seq.empty, Seq(m))
        val otherChunks = manifest.chunkRoots
          .map(r => historyStorage.modifierById(UtxoSnapshot.rootDigestToId(r)))
          .collect { case Some(chunk: UtxoSnapshotChunk) => chunk }
        val lastHeaders = takeLastHeaders(manifest.blockId, Constants.LastHeadersInContext)
        if (otherChunks.lengthCompare(manifest.size - 1) == 0 &&
          lastHeaders.lengthCompare(Constants.LastHeadersInContext) != 0) {
          // Time to apply snapshot
          val snapshot = UtxoSnapshot(manifest, otherChunks :+ m, lastHeaders)
          ProgressInfo(None, Seq.empty, Seq(snapshot), Seq.empty)
        } else {
          emptyProgressInfo
        }
      case _ =>
        emptyProgressInfo
    }
  }

  def validate(m: UtxoSnapshotChunk): Try[Unit] = if (historyStorage.contains(m.id)) {
    Failure(new Exception(s"UtxoSnapshotChunk with id ${m.encodedId} is already in history"))
  } else {
    Success(Unit)
  }

  private def takeLastHeaders(lastHeaderId: ModifierId, qty: Int): Seq[Header] = {
    (0 to qty).foldLeft(Seq.empty[Header]) { case (acc, _) =>
      historyStorage.modifierById(acc.headOption.map(_.parentId).getOrElse(lastHeaderId)) match {
        case Some(h: Header) =>
          acc :+ h
        case _ =>
          acc
      }
    }
  }

}
