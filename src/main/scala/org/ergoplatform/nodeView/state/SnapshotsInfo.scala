package org.ergoplatform.nodeView.state

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.ManifestId
import scorex.db.LDBKVStore

case class SnapshotsInfo(availableManifests: Map[Height, ManifestId])

class SnapshotsDb(store: LDBKVStore) {

  private def snapshotsInfoToBytes(snapshotsInfo: SnapshotsInfo): Array[Byte] = {
    Bytes.concat(
      snapshotsInfo.availableManifests.map{case (h, manifestId) =>
        Bytes.concat(Ints.toByteArray(h), manifestId)
      }
    )
  }

  def writeSnapshot(height: Height, manifest: UtxoState.Manifest, subtrees: Seq[UtxoState.Subtree]): Unit = {
    store.insert()
  }

}

object SnapshotsDb {


  def create(dir: File): Unit ={
    val store = new LDBKVStore(dir)
    SnapshotsDb(store)
  }
}
