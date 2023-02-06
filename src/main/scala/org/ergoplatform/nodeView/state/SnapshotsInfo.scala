package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.ManifestId

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




