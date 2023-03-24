package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.{ManifestId, SubtreeId}
import org.ergoplatform.settings.Algos.HF
import scorex.crypto.authds.avltree.batch.{PersistentBatchAVLProver, VersionedLDBAVLStorage}
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import org.ergoplatform.settings.Constants.{MakeSnapshotEvery, timeToTakeSnapshot}

import scala.concurrent.Future

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

      import scala.concurrent.ExecutionContext.Implicits.global
      val ms0 = System.currentTimeMillis()
      // todo: check that future will work with the same tree
      Future {
        log.info("Started work within future")
        val ft0 = System.currentTimeMillis()
        dumpSnapshot(height)
        snapshotsDb.pruneSnapshots(height - MakeSnapshotEvery * 2) //todo: async
        val ft = System.currentTimeMillis()
        log.info("Work within future: " + (ft - ft0) + " ms.")
      }
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
