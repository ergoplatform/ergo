package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import scorex.crypto.authds.avltree.batch.PersistentBatchAVLProver
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSerializer, BatchAVLProverSubtree}
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging

trait UtxoSetSnapshotPersistence extends ScorexLogging {

  def constants: StateConstants
  def persistentProver: PersistentBatchAVLProver[Digest32, HF]

  private val snapshotsDb = SnapshotsDb.create(constants.settings) //todo: move to some other place ?

  //todo: scaladoc
  def slicedTree(): (BatchAVLProverManifest[Digest32], Seq[BatchAVLProverSubtree[Digest32]]) = {
    persistentProver.synchronized {
      val serializer = new BatchAVLProverSerializer[Digest32, HF]()(Algos.hash)
      serializer.slice(persistentProver.avlProver, subtreeDepth = 12)
    }
  }

  protected def saveSnapshotIfNeeded(height: Height, estimatedTip: Option[Height]): Unit = {

    val SnapshotEvery = 5 // test value, switch to 51840 after testing

    if (estimatedTip.nonEmpty &&
      (height % SnapshotEvery == 0) &&
      estimatedTip.get - height <= SnapshotEvery) {

      val (manifest, subtrees) = slicedTree()

      val ms0 = System.currentTimeMillis()
      snapshotsDb.pruneSnapshots(height - SnapshotEvery * 2)
      snapshotsDb.writeSnapshot(height, manifest, subtrees)
      val ms = System.currentTimeMillis()
      log.info("Time to dump utxo set snapshot: " + (ms - ms0))
    }
  }

  def snapshotsAvailable(): SnapshotsInfo = {
    snapshotsDb.readSnapshotsInfo
  }

}
