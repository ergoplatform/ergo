package org.ergoplatform.nodeView.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.ManifestId
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{ErgoAlgos, ErgoSettings}
import org.ergoplatform.wallet.Constants
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSerializer, BatchAVLProverSubtree}
import scorex.crypto.hash.Digest32
import scorex.db.{LDBFactory, LDBKVStore}

import scala.util.Try

case class SnapshotsInfo(availableManifests: Map[Height, ManifestId]) {
  def withNewManifest(height: Height, manifestId: ManifestId): SnapshotsInfo = {
    SnapshotsInfo(availableManifests.updated(height, manifestId))
  }
}

object SnapshotsInfo {
  val empty = SnapshotsInfo(Map.empty)
}

class SnapshotsDb(store: LDBKVStore) {

  private val serializer = new BatchAVLProverSerializer[Digest32, HF]()(ErgoAlgos.hash)

  private val snapshotInfoKey: Array[Byte] = Array.fill(32)(0: Byte)

  private def snapshotsInfoToBytes(snapshotsInfo: SnapshotsInfo): Array[Byte] = {
    Bytes.concat(
      snapshotsInfo.availableManifests.map{case (h, manifestId) =>
        Bytes.concat(Ints.toByteArray(h), manifestId)
      }.toSeq :_*
    )
  }

  private def snapshotsInfoFromBytes(bytes: Array[Byte]): SnapshotsInfo = {
    val manifests = bytes.grouped(36).map {rowBytes =>
      val height = Ints.fromByteArray(rowBytes.take(4))
      val manifestId = Digest32 @@ rowBytes.drop(4)
      height -> manifestId
    }.toMap
    SnapshotsInfo(manifests)
  }

  def writeSnapshotsInfo(snapshotsInfo: SnapshotsInfo): Try[Unit] = {
    store.insert(Seq(snapshotInfoKey -> snapshotsInfoToBytes(snapshotsInfo)))
  }

  def readSnapshotsInfo: Option[SnapshotsInfo] = {
    store.get(snapshotInfoKey).map(snapshotsInfoFromBytes)
  }

  def writeSnapshot(height: Height,
                    manifest: UtxoState.Manifest,
                    subtrees: Seq[UtxoState.Subtree]): Unit = {
    val manifestBytes = serializer.manifestToBytes(manifest)
    val manifestId = manifest.id
    //todo: RAM consumption doubles here, avoid it
    val subTreesToWrite = subtrees.map(s => s.id -> serializer.subtreeToBytes(s))
    store.insert(Seq(manifestId -> manifestBytes) ++ subTreesToWrite)
    val si = readSnapshotsInfo.getOrElse(SnapshotsInfo.empty).withNewManifest(height, manifestId)
    writeSnapshotsInfo(si)
  }

  def readManifestBytes(id: ManifestId): Option[BatchAVLProverManifest[Digest32]] = {
    store.get(id).flatMap(bs => serializer.manifestFromBytes(bs, Constants.ModifierIdLength).toOption)
  }

  def readSubtreeBytes(id: ManifestId): Option[BatchAVLProverSubtree[Digest32]] = {
    store.get(id).flatMap(bs => serializer.subtreeFromBytes(bs, Constants.ModifierIdLength).toOption)
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
