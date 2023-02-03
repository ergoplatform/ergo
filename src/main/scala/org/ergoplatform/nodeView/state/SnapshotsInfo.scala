package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.ManifestId

/**
  * Container for UTXO set snapshots the node holds
  * @param availableManifests - available UTXO set snapshot manifests and corresponding heights
  */
case class SnapshotsInfo(availableManifests: Map[Height, ManifestId]) {

  /**
    * @return new container instance with new snapshot added
    */
  def withNewManifest(height: Height, manifestId: ManifestId): SnapshotsInfo = {
    SnapshotsInfo(availableManifests.updated(height, manifestId))
  }

  /**
    * @return whether snapshots available
    */
  def nonEmpty: Boolean = availableManifests.nonEmpty
}

object SnapshotsInfo {

  /**
    * @return create container with no snapshots there
    */
  def makeEmpty(): SnapshotsInfo = SnapshotsInfo(Map.empty)

}




