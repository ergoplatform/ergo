package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.{ManifestId, SubtreeId}
import org.ergoplatform.settings.Algos.HF
import scorex.crypto.authds.avltree.batch.{PersistentBatchAVLProver, VersionedLDBAVLStorage}
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import org.ergoplatform.settings.Constants.{MakeSnapshotEvery, timeToTakeSnapshot}

trait UtxoSetSnapshotPersistence extends ScorexLogging {

  def constants: StateConstants
  protected def persistentProver: PersistentBatchAVLProver[Digest32, HF]

  private[nodeView] val snapshotsDb = SnapshotsDb.create(constants.settings) //todo: move to some other place ?

  private[nodeView] def dumpSnapshot(height: Height): Array[Byte] = {
    snapshotsDb.writeSnapshot(persistentProver.storage.asInstanceOf[VersionedLDBAVLStorage], height)
  }

  protected def saveSnapshotIfNeeded(height: Height, estimatedTip: Option[Height]): Unit = {
    if (constants.settings.nodeSettings.areSnapshotsStored &&
        timeToTakeSnapshot(height) &&
        estimatedTip.nonEmpty &&
        estimatedTip.get - height <= MakeSnapshotEvery) {

      val ms0 = System.currentTimeMillis()
      snapshotsDb.pruneSnapshots(height - MakeSnapshotEvery * 2)
      dumpSnapshot(height)
      val ms = System.currentTimeMillis()
      log.info("Time to dump utxo set snapshot: " + (ms - ms0))
    }
  }

  def snapshotsAvailable(): SnapshotsInfo = {
    snapshotsDb.readSnapshotsInfo
  }

  def getSnapshotInfo(): SnapshotsInfo = {
    snapshotsDb.readSnapshotsInfo
  }

  def getManifestBytes(id: ManifestId): Option[Array[Byte]] = {
    snapshotsDb.readManifestBytes(id)
  }

  def getUtxoSnapshotChunkBytes(id: SubtreeId): Option[Array[Byte]] = {
    snapshotsDb.readSubtreeBytes(id)
  }

}
