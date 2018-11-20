package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.state.{UtxoSnapshot, UtxoSnapshotChunk, UtxoSnapshotManifest}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.util.{ModifierId, ScorexLogging}

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/** Contains all functions for locally generated state snapshot processing.
  * */
trait UtxoSnapshotProcessor extends ScorexLogging with ScorexEncoding {

  protected val config: NodeConfigurationSettings

  protected val historyStorage: HistoryStorage

  def headerIdsAtHeight(height: Int): Seq[ModifierId]

  def typedModifierById[T <: ErgoPersistentModifier : ClassTag](id: ModifierId): Option[T]

  def process(m: UtxoSnapshot): ProgressInfo[ErgoPersistentModifier] = {
    m.lastHeaders.headOption.foreach(h => pruneOldSnapshots(h.height))
    log.info(s"Appending new state snapshot ${m.id}")
    historyStorage.insertObjects(m.chunks :+ m.manifest)
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  def pruneOldSnapshots(newHeight: Int): Unit = {
    assert(newHeight % config.snapshotCreationInterval == 0, "Trying to prune snapshots at wrong height.")
    val heightToRemove = newHeight - config.snapshotCreationInterval * config.keepLastSnapshots
    headerIdsAtHeight(heightToRemove).headOption
      .flatMap { id =>
        typedModifierById[Header](id).flatMap { h =>
          val manifestId = UtxoSnapshotManifest.blockIdToManifestId(h.id)
          typedModifierById[UtxoSnapshotManifest](manifestId).map { manifest =>
            val chunks = manifest.chunkRoots.map(UtxoSnapshot.rootDigestToId)
            manifestId -> chunks
          }
        }
      }
      .fold(()) { case (manifest, chunks) =>
        log.info(s"Removing old snapshot $manifest")
        historyStorage.remove(chunks :+ manifest)
      }
  }

  def validate(m: UtxoSnapshot): Try[Unit] = if (historyStorage.contains(m.id)) {
    Failure(new Exception(s"UtxoSnapshot with id ${m.encodedId} is already in history"))
  } else {
    Success(Unit)
  }

}
