package org.ergoplatform.nodeView.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.{ManifestId, SubtreeId}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{ErgoAlgos, ErgoSettings}
import org.ergoplatform.wallet.Constants
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSerializer, BatchAVLProverSubtree}
import scorex.crypto.hash.Digest32
import scorex.db.{LDBFactory, LDBKVStore}
import scorex.util.ScorexLogging
import scorex.util.encode.Base16

import scala.util.{Failure, Success, Try}

class SnapshotsDb(store: LDBKVStore) extends ScorexLogging {

  private val serializer = new BatchAVLProverSerializer[Digest32, HF]()(ErgoAlgos.hash)

  private val snapshotInfoKey: Array[Byte] = Array.fill(32)(0: Byte)

  private def snapshotsInfoToBytes(snapshotsInfo: SnapshotsInfo): Array[Byte] = {
    Bytes.concat(
      snapshotsInfo.availableManifests.map { case (h, manifestId) =>
        Bytes.concat(Ints.toByteArray(h), manifestId)
      }.toSeq: _*
    )
  }

  private def snapshotsInfoFromBytes(bytes: Array[Byte]): SnapshotsInfo = {
    val manifests = bytes.grouped(36).map { rowBytes =>
      val height = Ints.fromByteArray(rowBytes.take(4))
      val manifestId = Digest32 @@ rowBytes.drop(4)
      height -> manifestId
    }.toMap
    SnapshotsInfo(manifests)
  }

  def writeSnapshotsInfo(snapshotsInfo: SnapshotsInfo): Try[Unit] = {
    store.insert(Seq(snapshotInfoKey -> snapshotsInfoToBytes(snapshotsInfo)))
  }

  def readSnapshotsInfo: SnapshotsInfo = {
    store.get(snapshotInfoKey).map(snapshotsInfoFromBytes).getOrElse(SnapshotsInfo.makeEmpty)
  }

  def notEmpty(): Boolean = {
    store.get(snapshotInfoKey).isDefined
  }

  def pruneSnapshots(before: Height): Unit = {
    log.info("Starting snapshots pruning")
    val (toPrune, toLeave) = readSnapshotsInfo
      .availableManifests
      .partition(_._1 < before)

    toPrune.foreach { case (h, manifestId) =>
      log.info(s"Pruning snapshot at height $h")
      val keysToRemove = store.get(manifestId) match {
        case Some(manifestBytes) =>
          serializer.manifestFromBytes(manifestBytes, Constants.ModifierIdLength) match {
            case Success(m) =>
              m.subtreesIds += manifestId
            case Failure(e) =>
              log.error(s"Can't parse manifest ${Base16.encode(manifestId)} :", e)
              Seq.empty
          }
        case None =>
          log.error(s"Manifest ${Base16.encode(manifestId)} not found:")
          Seq.empty
      }
      store.remove(keysToRemove)
    }

    val updInfo = SnapshotsInfo(toLeave)
    writeSnapshotsInfo(updInfo)

    log.info("Snapshots pruning finished")
  }

  def writeSnapshot(height: Height,
                    manifest: UtxoState.Manifest,
                    subtrees: Seq[UtxoState.Subtree]): Unit = {
    val manifestBytes = serializer.manifestToBytes(manifest)
    val manifestId = manifest.id
    //todo: RAM consumption doubles here, avoid it
    val subTreesToWrite = subtrees.map(s => s.id -> serializer.subtreeToBytes(s))
    store.insert(Seq(manifestId -> manifestBytes) ++ subTreesToWrite)
    val si = readSnapshotsInfo.withNewManifest(height, manifestId)
    writeSnapshotsInfo(si)
  }

  def readManifest(id: ManifestId): Option[BatchAVLProverManifest[Digest32]] = {
    readManifestBytes(id).flatMap(bs => serializer.manifestFromBytes(bs, Constants.ModifierIdLength).toOption)
  }

  def readManifestBytes(id: ManifestId): Option[Array[Byte]] = {
    store.get(id)
  }

  def readSubtreeBytes(id: SubtreeId): Option[Array[Byte]] = {
    store.get(id)
  }

  def readSubtree(id: SubtreeId): Option[BatchAVLProverSubtree[Digest32]] = {
    readSubtreeBytes(id).flatMap(bs => serializer.subtreeFromBytes(bs, Constants.ModifierIdLength).toOption)
  }

}

object SnapshotsDb {

  def create(ergoSettings: ErgoSettings): SnapshotsDb = {
    val dir = s"${ergoSettings.directory}/snapshots"
    create(dir)
  }

  private[state] def create(dir: String): SnapshotsDb = {
    val store = LDBFactory.createKvDb(dir)
    new SnapshotsDb(store)
  }

}
