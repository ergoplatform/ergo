package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.{ManifestId, SubtreeId}
import org.ergoplatform.settings.Algos.HF
import scorex.crypto.authds.avltree.batch.{PersistentBatchAVLProver, VersionedRocksDBAVLStorage}
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.serialization.ManifestSerializer

import scala.concurrent.Future
import scala.util.Try

/**
  * Functions needed for storing UTXO set snapshots and working with them
  */
trait UtxoSetSnapshotPersistence extends ScorexLogging {

  protected def ergoSettings: ErgoSettings
  protected def persistentProver: PersistentBatchAVLProver[Digest32, HF]

  private[nodeView] val snapshotsDb = SnapshotsDb.create(ergoSettings)

  // Dump current UTXO set snapshot to persistent snapshots database
  // private[nodeView] as used in tests also
  private[nodeView] def dumpSnapshot(height: Height,
                                     expectedRootHash: Array[Byte],
                                     manifestDepth: Byte = ManifestSerializer.MainnetManifestDepth): Try[Array[Byte]] = {
    val storage = persistentProver.storage.asInstanceOf[VersionedRocksDBAVLStorage]
    snapshotsDb.writeSnapshot(storage, height, expectedRootHash, manifestDepth)
  }

  /**
    * Check if it is time to store UTXO set snapshot, if so, store it asynchronously,
    * to avoid locking the thread currently working with UTXO set, and then prune snapshots which become
    * obsolete (also in a separate thread).
    *
    * Thus this method is doing everything which is needed to store snapshot when needed. Client logic then
    * may simply call it on every block applied to the state.
    *
    * @param height - height of a block just processed (so we work with UTXO set after block with this height applied)
    * @param estimatedTip - estimated height of best blockchain in the network
    */
  protected def saveSnapshotIfNeeded(height: Height, estimatedTip: Option[Height]): Unit = {
    val makeSnapshotEvery = ergoSettings.chainSettings.makeSnapshotEvery
    def timeToTakeSnapshot(height: Int): Boolean = {
      height % makeSnapshotEvery == makeSnapshotEvery - 1
    }
    log.info(s"checking snapshot for $height, simple check: " + timeToTakeSnapshot(height))
    if (ergoSettings.nodeSettings.areSnapshotsStored &&
        timeToTakeSnapshot(height) &&
        estimatedTip.nonEmpty &&
        estimatedTip.get - height <= makeSnapshotEvery) {

      val ms0 = System.currentTimeMillis()

      // drop tree height byte from digest to get current root hash of AVL+ tree
      // we then pass the hash and check in another thread whether it is the same after taking db snapshot
      // there is small probability of failure, but that is ok, and better than any locking likely
      val expectedRootHash = persistentProver.digest.dropRight(1)
      Future {
        log.info("Started work within future")
        val ft0 = System.currentTimeMillis()
        dumpSnapshot(height, expectedRootHash)
        snapshotsDb.pruneSnapshots(ergoSettings.nodeSettings.utxoSettings.storingUtxoSnapshots)
        val ft = System.currentTimeMillis()
        log.info("Work within future finished in: " + (ft - ft0) + " ms.")
      }(scala.concurrent.ExecutionContext.Implicits.global)
      val ms = System.currentTimeMillis()
      log.info("Main thread time to dump utxo set snapshot: " + (ms - ms0) + " ms.")
    }
  }

  /**
    * @return list of stored UTXO set snapshots
    */
  def getSnapshotInfo(): SnapshotsInfo = {
    snapshotsDb.readSnapshotsInfo
  }

  /**
    * Read snapshot manifest bytes from database without decoding. Used to serve clients over the wire.
    * @param id - manifest id
    */
  def getManifestBytes(id: ManifestId): Option[Array[Byte]] = {
    snapshotsDb.readManifestBytes(id)
  }

  /**
    * Read snapshot chunk (subtree) bytes from database without decoding. Used to serve clients over the wire.
    * @param id - subtree id
    */
  def getUtxoSnapshotChunkBytes(id: SubtreeId): Option[Array[Byte]] = {
    snapshotsDb.readSubtreeBytes(id)
  }

}
