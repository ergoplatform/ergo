package org.ergoplatform.nodeView.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.ManifestId
import org.ergoplatform.settings.ErgoSettings
import scorex.crypto.hash.Digest32
import scorex.db.{LDBFactory, LDBKVStore}

import scala.util.Try

case class SnapshotsInfo(availableManifests: Map[Height, ManifestId])

class SnapshotsDb(store: LDBKVStore) {

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
    ???
  }

}

object SnapshotsDb {

  def create(ergoSettings: ErgoSettings): Unit = {
    val dir = s"${ergoSettings.directory}/snapshots"
    create(dir)
  }

  private[state] def create(dir: String): SnapshotsDb = {
    val store = LDBFactory.createKvDb(dir)
    new SnapshotsDb(store)
  }

}
