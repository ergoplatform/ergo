package org.ergoplatform.nodeView.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.ManifestId
import org.ergoplatform.settings.ErgoSettings
import scorex.db.{LDBFactory, LDBKVStore}

case class SnapshotsInfo(availableManifests: Map[Height, ManifestId])

class SnapshotsDb(store: LDBKVStore) {

  private def snapshotsInfoToBytes(snapshotsInfo: SnapshotsInfo): Array[Byte] = {
    Bytes.concat(
      snapshotsInfo.availableManifests.map{case (h, manifestId) =>
        Bytes.concat(Ints.toByteArray(h), manifestId)
      }.toSeq :_*
    )
  }

  def writeSnapshot(height: Height, manifest: UtxoState.Manifest, subtrees: Seq[UtxoState.Subtree]): Unit = {
    snapshotsInfoToBytes(null)
  }

}

object SnapshotsDb {
  def create(ergoSettings: ErgoSettings): Unit ={
    val store = LDBFactory.createKvDb(s"${ergoSettings.directory}/snapshots")
    new SnapshotsDb(store)
  }
}
