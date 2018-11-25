package org.ergoplatform.nodeView.history.modifierprocessors

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.state.{UtxoSnapshot, UtxoSnapshotChunk, UtxoSnapshotManifest}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.util.{Failure, Try}

trait UtxoSnapshotChunkProcessor extends ScorexLogging with ScorexEncoding {

  protected val historyStorage: HistoryStorage

  protected val PendingManifestIdKey: ByteArrayWrapper

  private val emptyProgressInfo = ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)

  protected val LastSnapshotAppliedHeightKey: ByteArrayWrapper =
    ByteArrayWrapper(Array.fill(Constants.HashLength)(UtxoSnapshot.modifierTypeId))

  protected def toDownload(header: Header): Seq[(ModifierTypeId, ModifierId)]

  protected def lastSnapshotAppliedHeight: Option[Int] = historyStorage.getIndex(LastSnapshotAppliedHeightKey)
    .map(w => Ints.fromByteArray(w.data))

  protected def pendingManifestOpt: Option[UtxoSnapshotManifest] = {
    historyStorage.getIndex(PendingManifestIdKey).flatMap {
      case r if r.data.nonEmpty =>
        historyStorage.modifierById(bytesToId(r.data))
          .fold[Option[UtxoSnapshotManifest]](None) {
            case m: UtxoSnapshotManifest => Some(m)
            case _ => throw new Error("Wrong history index")
          }
      case _ =>
        None
    }
  }

  def process(m: UtxoSnapshotChunk): ProgressInfo[ErgoPersistentModifier] = {
    pendingManifestOpt match {
      case Some(manifest: UtxoSnapshotManifest) =>
        val otherChunks = manifest.chunkRoots
          .map(r => historyStorage.modifierById(UtxoSnapshot.rootDigestToId(r)))
          .collect { case Some(chunk: UtxoSnapshotChunk) => chunk }
        lazy val lastHeaders = takeLastHeaders(manifest.blockId, Constants.LastHeadersInContext)
        if (otherChunks.lengthCompare(manifest.chunkRoots.size - 1) == 0 && lastHeaders.nonEmpty) {
          // Time to apply snapshot
          val snapshot = UtxoSnapshot(manifest, otherChunks :+ m, lastHeaders)
          val snapshotHeight = lastHeaders.head.height
          val indexesToInsert = Seq(
            LastSnapshotAppliedHeightKey -> ByteArrayWrapper(Ints.toByteArray(snapshotHeight)),
            PendingManifestIdKey -> ByteArrayWrapper(Array.empty)
          )
          historyStorage.insert(Algos.idToBAW(m.id), indexesToInsert, Seq.empty)
          ProgressInfo(None, Seq.empty, Seq(snapshot), toDownload(lastHeaders.head))
        } else {
          historyStorage.insertObjects(Seq(m))
          emptyProgressInfo
        }
      case _ =>
        emptyProgressInfo
    }
  }

  def validate(m: UtxoSnapshotChunk): Try[Unit] = {
    pendingManifestOpt match {
      case Some(manifest: UtxoSnapshotManifest) =>
        m.validate(manifest)
      case _ =>
        Failure(new Exception(s"Manifest for chunk ${m.id} is undefined"))
    }
  }

  private def takeLastHeaders(lastHeaderId: ModifierId, qty: Int): Seq[Header] = {
    (0 until qty).foldLeft(Seq.empty[Header]) { case (acc, _) =>
      historyStorage.modifierById(acc.headOption.map(_.parentId).getOrElse(lastHeaderId)) match {
        case Some(h: Header) =>
          acc :+ h
        case _ =>
          acc
      }
    }
  }

}
