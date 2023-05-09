package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoState}
import org.ergoplatform.nodeView.state.UtxoState.SubtreeId
import org.ergoplatform.settings.{Algos, ErgoAlgos, ErgoSettings}
import scorex.core.VersionTag
import scorex.core.network.ConnectedPeer
import scorex.core.serialization.SubtreeSerializer
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSubtree}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.db.LDBVersionedStore
import scorex.util.{ModifierId, ScorexLogging}
import spire.syntax.all.cfor

import scala.util.{Failure, Random, Try}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, PersistentBatchAVLProver, VersionedLDBAVLStorage}

/**
  * Parts of history processing and storage corresponding to UTXO set snapshot processing and storage
  *
  * Stores UTXO set snapshots manifests and chunks for incomplete snapshots.
  */
trait UtxoSetSnapshotProcessor extends ScorexLogging {

  import org.ergoplatform.settings.ErgoAlgos.HF

  // node config to read history-related settings here and in descendants
  protected val settings: ErgoSettings

  // database to read history-related objects here and in descendants
  protected val historyStorage: HistoryStorage

  // minimal height to applu full blocks from
  // its value depends on node settings,
  // if download with UTXO set snapshot is used, the value is being set to a first block after the snapshot,
  // if blockToKeep > 0, the value is being set to a first block of blockchain suffix after headers downloaded
  private[history] var minimalFullBlockHeightVar: Int

  private val downloadedChunksPrefix = Blake2b256.hash("downloaded chunk").drop(4)

  private var _manifest: Option[BatchAVLProverManifest[Digest32]] = None

  private var _cachedDownloadPlan: Option[UtxoSetSnapshotDownloadPlan] = None

  /**
    * @return if UTXO set snapshot was applied during this session (stored in memory only).
    *         This flag is needed to prevent double application of UTXO set snapshot.
    *         After first full-block block application not needed anymore.
    */
  def isUtxoSnapshotApplied: Boolean = {
    minimalFullBlockHeightVar > ErgoHistory.GenesisHeight
  }

  /**
    * Writes that UTXO set snapshot applied at height `height`. Starts full blocks applications since the next block
    * after.
    */
  def utxoSnapshotApplied(height: Height): Unit = {
    minimalFullBlockHeightVar = height + 1
  }

  private def updateUtxoSetSnashotDownloadPlan(plan: UtxoSetSnapshotDownloadPlan): Unit = {
    _cachedDownloadPlan = Some(plan)
  }

  /**
    * Register manifest as one to be downloaded and create download plan from it
    * @param manifest - manifest corresponding to UTXO set snapshot to be downloaded
    * @param blockHeight - height of a block corresponding to the manifest
    * @param peersToDownload - peers to download chunks related to manifest from
    * @return download plan
    */
  def registerManifestToDownload(manifest: BatchAVLProverManifest[Digest32],
                                 blockHeight: Height,
                                 peersToDownload: Seq[ConnectedPeer]): UtxoSetSnapshotDownloadPlan = {
    val plan = UtxoSetSnapshotDownloadPlan.fromManifest(manifest, blockHeight, peersToDownload)
    _manifest = Some(manifest)
    updateUtxoSetSnashotDownloadPlan(plan)
    plan
  }

  /**
    * @return UTXO set snapshot download plan, if available
    */
  def utxoSetSnapshotDownloadPlan(): Option[UtxoSetSnapshotDownloadPlan] = {
    _cachedDownloadPlan match {
      case s@Some(_) => s
      case None => None
    }
  }

  /**
    * @return random peer from which UTXO snapshot chunks can be requested
    */
  def randomPeerToDownloadChunks(): Option[ConnectedPeer] = {
    val peers = _cachedDownloadPlan.map(_.peersToDownload).getOrElse(Seq.empty)
    if (peers.nonEmpty) {
      Some(peers(Random.nextInt(peers.size)))
    } else {
      None
    }
  }

  /**
    * @return up to `howMany` ids of UTXO set snapshot chunks to download
    */
  def getChunkIdsToDownload(howMany: Int): Seq[SubtreeId] = {
    utxoSetSnapshotDownloadPlan() match {
      case Some(plan) =>
        val expected = plan.expectedChunkIds
        val downloadIndex = plan.downloadedChunkIds.size
        val toDownload = if (expected.size > downloadIndex) {
          expected.slice(downloadIndex, downloadIndex + howMany)
        } else {
          IndexedSeq.empty
        }
        log.info(s"Downloaded or waiting ${plan.downloadedChunkIds.size} chunks out of ${expected.size}, downloading ${toDownload.size} more")
        val newDownloaded = plan.downloadedChunkIds ++ toDownload.map(_ => false)
        val newDownloading = plan.downloadingChunks + toDownload.size
        val updPlan = plan.copy(
          latestUpdateTime = System.currentTimeMillis(),
          downloadedChunkIds = newDownloaded,
          downloadingChunks = newDownloading
        )
        _cachedDownloadPlan = Some(updPlan)
        toDownload

      case None =>
        log.warn(s"No download plan is found when requested to propose $howMany chunks to download")
        Seq.empty
    }
  }

  /**
    * Write serialized UTXO set snapshot chunk to the database
    */
  def registerDownloadedChunk(chunkId: Array[Byte], chunkSerialized: Array[Byte]): Unit = {
    utxoSetSnapshotDownloadPlan() match {
      case Some(plan) =>
        cfor(0)(_ < plan.downloadedChunkIds.size, _ + 1) { idx =>
          if (!plan.downloadedChunkIds(idx) && plan.expectedChunkIds(idx).sameElements(chunkId)) {
            val idxBytes = Ints.toByteArray(idx)
            historyStorage.insert(downloadedChunksPrefix ++ idxBytes, chunkSerialized)
            val updDownloaded = plan.downloadedChunkIds.updated(idx, true)
            val updDownloading = plan.downloadingChunks - 1
            val updPlan = plan.copy(latestUpdateTime = System.currentTimeMillis(), downloadedChunkIds = updDownloaded, downloadingChunks = updDownloading)
            updateUtxoSetSnashotDownloadPlan(updPlan)
            return
          }
        }
      case None =>
        log.warn(s"Chunk ${Algos.encode(chunkId)} downloaded but no download plan found")
    }
  }

  /**
    * @return iterator for chunks downloded. Reads them from database one-by-one when requested.
    */
  def downloadedChunksIterator(): Iterator[BatchAVLProverSubtree[Digest32]] = {
    utxoSetSnapshotDownloadPlan() match {
      case Some(plan) =>
        Iterator.range(0, plan.totalChunks).flatMap{idx =>
          val idxBytes = Ints.toByteArray(idx)
          historyStorage
            .get(downloadedChunksPrefix ++ idxBytes)
            .flatMap(bs => SubtreeSerializer.parseBytesTry(bs).toOption)
        }
      case None =>
        log.error("No download plan found in downloadedChunksIterator")
      Iterator.empty
    }
  }

  /**
    * Create disk-persistent authenticated AVL+ tree prover
    * @param stateStore - disk database where AVL+ tree will be after restoration
    * @param blockId - id of a block corresponding to the tree (tree is on top of a state after the block)
    * @return prover with initialized tree database
    */
  def createPersistentProver(stateStore: LDBVersionedStore,
                             blockId: ModifierId): Try[PersistentBatchAVLProver[Digest32, HF]] = {
    _manifest match {
      case Some(manifest) =>
        log.info("Starting UTXO set snapshot transfer into state database")
        val esc = ErgoStateReader.storageStateContext(stateStore, settings)
        val metadata = UtxoState.metadata(VersionTag @@@ blockId, VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight), None, esc)
        VersionedLDBAVLStorage.recreate(manifest, downloadedChunksIterator(), additionalData = metadata.toIterator, stateStore).flatMap {
          ldbStorage =>
            log.info("Finished UTXO set snapshot transfer into state database")
            ldbStorage.restorePrunedProver().map {
              prunedAvlProver =>
                new PersistentBatchAVLProver[Digest32, HF] {
                  override var avlProver: BatchAVLProver[Digest32, ErgoAlgos.HF] = prunedAvlProver
                  override val storage: VersionedLDBAVLStorage = ldbStorage
                }
            }
        }
      case None =>
        val msg = "No manifest available in createPersistentProver"
        log.error(msg)
        Failure(new Exception(msg))
    }
  }
}