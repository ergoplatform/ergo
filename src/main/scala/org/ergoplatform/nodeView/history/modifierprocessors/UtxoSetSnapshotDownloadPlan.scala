package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.SubtreeId
import scorex.core.network.ConnectedPeer
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverManifest
import scorex.crypto.hash.Digest32


/**
  * Entity which stores information about state of UTXO set snapshots downloading
  *
  * @param createdTime - time when snapshot was created
  * @param latestUpdateTime - latest time when anything was updated for the state of UTXO set snapshot
  * @param snapshotHeight - height of a block UTXO set snapshot is corresponding to (UTXO set if after the block applied)
  * @param utxoSetRootHash - root hash of AVL+ tree which is authenticating UTXO set snapshot
  * @param utxoSetTreeHeight - tree height of AVL+ tree which is authenticating UTXO set snapshot
  * @param expectedChunkIds - ids of UTXO set snapshot chunks to be downloaded
  * @param downloadedChunkIds - shapshot chunks already downloaded, in form of boolean map over
  *                             `expectedChunkIds` (true = downloaded)
  * @param downloadingChunks - number of UTXO set shapshot chunks the node is currently downloading
  * @param peersToDownload - peers UTXO set snapshot chunks can be downloaded from
  */
case class UtxoSetSnapshotDownloadPlan(createdTime: Long,
                                       latestUpdateTime: Long,
                                       snapshotHeight: Height,
                                       utxoSetRootHash: Digest32,
                                       utxoSetTreeHeight: Byte,
                                       expectedChunkIds: IndexedSeq[SubtreeId],
                                       downloadedChunkIds: IndexedSeq[Boolean],
                                       downloadingChunks: Int,
                                       peersToDownload: Seq[ConnectedPeer]) {

  def id: Digest32 = utxoSetRootHash

  /**
    * @return how many chunks to download
    */
  def totalChunks: Int = expectedChunkIds.size

  /**
    * @return whether UTXO set snapshot fully downloaded
    */
  def fullyDownloaded: Boolean = {
    (expectedChunkIds.size == downloadedChunkIds.size) &&
      downloadingChunks == 0 &&
      downloadedChunkIds.forall(_ == true)
  }

}

object UtxoSetSnapshotDownloadPlan {

  /**
    * Create UTXO set snapshot download plan from manifest, height of a block corresponding to UTXO set
    * manifest represents, and peers to download UTXO set snapshot from
    */
  def fromManifest(manifest: BatchAVLProverManifest[Digest32],
                   blockHeight: Height,
                   peersToDownload: Seq[ConnectedPeer]): UtxoSetSnapshotDownloadPlan = {
    val subtrees = manifest.subtreesIds
    val now = System.currentTimeMillis()

    // it is safe to call .toByte below, as the whole tree has height <= 127, and manifest even less
    UtxoSetSnapshotDownloadPlan(
      createdTime = now,
      latestUpdateTime = now,
      snapshotHeight = blockHeight,
      utxoSetRootHash = manifest.id,
      utxoSetTreeHeight = manifest.rootHeight.toByte,
      expectedChunkIds = subtrees.toIndexedSeq,
      downloadedChunkIds = IndexedSeq.empty,
      downloadingChunks = 0,
      peersToDownload = peersToDownload)
  }

}
