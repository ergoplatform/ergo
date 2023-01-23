package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.{ManifestId, SubtreeId}
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.settings.Algos.HF
import scorex.crypto.authds.avltree.batch.PersistentBatchAVLProver
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSerializer, BatchAVLProverSubtree}
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import org.ergoplatform.settings.Constants.MakeSnapshotEvery

trait UtxoSetSnapshotPersistence extends ScorexLogging {

  def constants: StateConstants
  protected def persistentProver: PersistentBatchAVLProver[Digest32, HF]

  private val snapshotsDb = SnapshotsDb.create(constants.settings) //todo: move to some other place ?

  //todo: scaladoc
  //todo: return Iterator?
  def slicedTree(): (BatchAVLProverManifest[Digest32], Seq[BatchAVLProverSubtree[Digest32]]) = {
    persistentProver.synchronized {
      val serializer = new BatchAVLProverSerializer[Digest32, HF]()(Algos.hash)
      serializer.slice(persistentProver.avlProver, subtreeDepth = 12) //todo: name constant
    }
  }

  protected def saveSnapshotIfNeeded(height: Height, estimatedTip: Option[Height]): Unit = {
    if (Constants.timeToTakeSnapshot(height) &&
        estimatedTip.nonEmpty &&
        estimatedTip.get - height <= MakeSnapshotEvery) {

      val (manifest, subtrees) = slicedTree()

      val ms0 = System.currentTimeMillis()
      snapshotsDb.pruneSnapshots(height - MakeSnapshotEvery * 2)
      snapshotsDb.writeSnapshot(height, manifest, subtrees)
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

  def getManifest(id: ManifestId): Option[Array[Byte]] = {
    snapshotsDb.readManifestBytes(id)
  }

  def getUtxoSnapshotChunkBytes(id: SubtreeId): Option[Array[Byte]] = {
    snapshotsDb.readSubtreeBytes(id)
  }

}
