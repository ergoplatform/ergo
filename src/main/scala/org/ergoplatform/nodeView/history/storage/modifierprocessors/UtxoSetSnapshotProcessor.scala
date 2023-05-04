package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoState}
import org.ergoplatform.nodeView.state.UtxoState.SubtreeId
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.VersionTag
import scorex.core.network.ConnectedPeer
import scorex.core.serialization.SubtreeSerializer
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSubtree}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.db.LDBVersionedStore
import scorex.util.{ModifierId, ScorexLogging}
import spire.syntax.all.cfor

import scala.util.{Random, Try}
import scorex.crypto.authds.avltree.batch.{PersistentBatchAVLProver, VersionedLDBAVLStorage}

/**
  * Parts of history processing and storage corresponding to UTXO set snapshot processing and storage
  *
  * Stores:
  * - writes chunks
  * - writes data for incomplete snapshots
  */
trait UtxoSetSnapshotProcessor extends ScorexLogging {

  import org.ergoplatform.settings.ErgoAlgos.HF

  protected val settings: ErgoSettings
  protected val historyStorage: HistoryStorage

  private[history] var minimalFullBlockHeightVar: Int

  private var _utxoSnapshotApplied = false

  def isUtxoSnapshotApplied = {
    _utxoSnapshotApplied
  }

  def utxoSnapshotApplied(height: Height): Unit = {
    _utxoSnapshotApplied = true
    minimalFullBlockHeightVar = height + 1
  }

  private val downloadedChunksPrefix = Blake2b256.hash("downloaded chunk").drop(4)

  private var _manifest: Option[BatchAVLProverManifest[Digest32]] = None

  private var _cachedDownloadPlan: Option[UtxoSetSnapshotDownloadPlan] = None

  def registerManifestToDownload(manifest: BatchAVLProverManifest[Digest32],
                                 blockHeight: Height,
                                 peersToDownload: Seq[ConnectedPeer]): UtxoSetSnapshotDownloadPlan = {
    val plan = UtxoSetSnapshotDownloadPlan.fromManifest(manifest, blockHeight, peersToDownload)
    _manifest = Some(manifest)
    updateUtxoSetSnashotDownloadPlan(plan)
    plan
  }

  def utxoSetSnapshotDownloadPlan(): Option[UtxoSetSnapshotDownloadPlan] = {
    _cachedDownloadPlan match {
      case s@Some(_) => s
      case None => None
    }
  }

  def randomPeerToDownloadChunks(): Option[ConnectedPeer] = {
    val peers = _cachedDownloadPlan.map(_.peersToDownload).getOrElse(Seq.empty)
    if (peers.nonEmpty) {
      Some(peers(Random.nextInt(peers.size)))
    } else {
      None
    }
  }

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
        val updPlan = plan.copy(latestUpdateTime = System.currentTimeMillis(), downloadedChunkIds = newDownloaded, downloadingChunks = newDownloading)
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
        log.error("todo: msg") // todo:
      Iterator.empty
    }
  }

  private def updateUtxoSetSnashotDownloadPlan(plan: UtxoSetSnapshotDownloadPlan): Unit = {
    _cachedDownloadPlan = Some(plan)
  }

  def createPersistentProver(stateStore: LDBVersionedStore,
                             blockId: ModifierId): Try[PersistentBatchAVLProver[Digest32, HF]] = {
    val manifest = _manifest.get //todo: .get
    log.info("Starting UTXO set snapshot transfer into state database")
    val esc = ErgoStateReader.storageStateContext(stateStore, settings)
    val metadata = UtxoState.metadata(VersionTag @@@ blockId, VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight), None, esc)
    VersionedLDBAVLStorage.recreate(manifest, downloadedChunksIterator(), additionalData = metadata.toIterator, stateStore).flatMap {
      ldbStorage =>
        log.info("Finished UTXO set snapshot transfer into state database")
        ldbStorage.restorePrunedProver().map { prunedAvlProver =>
          new PersistentBatchAVLProver[Digest32, HF] {
            override var avlProver = prunedAvlProver
            override val storage = ldbStorage
          }
        }
    }
  }
}

case class UtxoSetSnapshotDownloadPlan(startingTime: Long,
                                       latestUpdateTime: Long,
                                       snapshotHeight: Height,
                                       utxoSetRootHash: Digest32,
                                       utxoSetTreeHeight: Byte,
                                       expectedChunkIds: IndexedSeq[SubtreeId],
                                       downloadedChunkIds: IndexedSeq[Boolean],
                                       downloadingChunks: Int,
                                       peersToDownload: Seq[ConnectedPeer]) {

  def id: Digest32 = utxoSetRootHash

  def totalChunks: Int = expectedChunkIds.size

  def fullyDownloaded: Boolean = {
    (expectedChunkIds.size == downloadedChunkIds.size) &&
      downloadingChunks == 0 &&
      downloadedChunkIds.forall(_ == true)
  }

}

object UtxoSetSnapshotDownloadPlan {

  def fromManifest(manifest: BatchAVLProverManifest[Digest32],
                   blockHeight: Height,
                   peersToDownload: Seq[ConnectedPeer]): UtxoSetSnapshotDownloadPlan = {
    val subtrees = manifest.subtreesIds
    val now = System.currentTimeMillis()

    // it is safe to call .toByte below, as the whole tree has height <= 127, and manifest even less
    UtxoSetSnapshotDownloadPlan(now, now, blockHeight, manifest.id, manifest.rootHeight.toByte, subtrees.toIndexedSeq,
                                IndexedSeq.empty, 0, peersToDownload)
  }

}
