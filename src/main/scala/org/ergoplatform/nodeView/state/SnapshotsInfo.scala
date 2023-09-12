package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.ManifestId
import org.ergoplatform.settings.Constants
import scorex.core.serialization.ErgoSerializer
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}

/**
  * Container for UTXO set snapshots the node holds
  * @param availableManifests - available UTXO set snapshot manifests and corresponding heights
  */
class SnapshotsInfo(val availableManifests: Map[Height, ManifestId]) {

  /**
    * @return new container instance with new snapshot added
    */
  def withNewManifest(height: Height, manifestId: ManifestId): SnapshotsInfo = {
    new SnapshotsInfo(availableManifests.updated(height, manifestId))
  }

  /**
    * @return whether snapshots available
    */
  def nonEmpty: Boolean = availableManifests.nonEmpty
}

object SnapshotsInfo {

  /**
    * @return empty container with no snapshots
    */
  val empty: SnapshotsInfo = new SnapshotsInfo(Map.empty)

}

object SnapshotsInfoSerializer extends ErgoSerializer[SnapshotsInfo] {

  override def serialize(snapshotsInfo: SnapshotsInfo, w: Writer): Unit = {
    w.putUInt(snapshotsInfo.availableManifests.size)
    snapshotsInfo.availableManifests.foreach { case (height, manifestId) =>
      w.putUInt(height)
      w.putBytes(manifestId)
    }
  }

  override def parse(r: Reader): SnapshotsInfo = {
    val manifestsCount = r.getUInt().toInt // we read from trusted source, no need for extra checks
    val manifests = (1 to manifestsCount).map { _ =>
      val h = r.getUInt().toInt
      val manifestId = Digest32 @@ r.getBytes(Constants.HashLength)
      h -> manifestId
    }.toMap
    new SnapshotsInfo(manifests)
  }

}



