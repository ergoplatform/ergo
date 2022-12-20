package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.UtxoState.SubtreeId
import org.ergoplatform.settings.{Algos, Constants}
import scorex.crypto.authds.avltree.batch.BatchAVLProver
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverManifest
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.{ByteArrayBuilder, ScorexLogging}
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}
import spire.syntax.all.cfor

import java.nio.ByteBuffer
import scala.collection.mutable

/**
  * Stores:
  * - writes chunks
  * - writes data for incomplete snapshots
  */
trait UtxoSetSnapshotProcessor extends ScorexLogging {

  protected val historyStorage: HistoryStorage

  private[history] var minimalFullBlockHeightVar: Int

  var _utxoSnapshotApplied = false

  def utxoSnapshotApplied(height: Height): Unit = {
    _utxoSnapshotApplied = true
    minimalFullBlockHeightVar = height + 1 //todo: or height + 1?
  }

  private val expectedChunksPrefix = Blake2b256.hash("expected chunk").drop(4)
  private val downloadedChunksPrefix = Blake2b256.hash("downloaded chunk").drop(4)

  private val downloadPlanKey = Blake2b256.hash("download plan")

  private var _manifest: Option[BatchAVLProverManifest[Digest32]] = None

  private var _cachedDownloadPlan: Option[UtxoSetSnapshotDownloadPlan] = None

  def pruneSnapshot(downloadPlan: UtxoSetSnapshotDownloadPlan) = ??? //todo: implement

  def registerManifestToDownload(manifest: BatchAVLProverManifest[Digest32],
                                 blockHeight: Height): UtxoSetSnapshotDownloadPlan = {
    val plan = UtxoSetSnapshotDownloadPlan.fromManifest(manifest, blockHeight)
    _manifest = Some(manifest)
    println(_manifest.get.id)
    updateUtxoSetSnashotDownloadPlan(plan)
  }

  def getUtxoSetSnapshotDownloadPlan(): Option[UtxoSetSnapshotDownloadPlan] = {
    _cachedDownloadPlan match {
      case s@Some(_) => s
      case None =>
        historyStorage.get(downloadPlanKey).flatMap { planId =>
          val planOpt = readDownloadPlanFromDb(Digest32 @@ planId)
          if (planOpt.isEmpty) log.warn(s"No download plan with id ${Algos.encode(planId)} found")
          if (planOpt.nonEmpty) _cachedDownloadPlan = planOpt
          planOpt
        }
    }
  }

  def getChunkIdsToDownload(howMany: Int): Seq[SubtreeId] = {
    getUtxoSetSnapshotDownloadPlan() match {
      case Some(plan) =>
        val expected = plan.expectedChunkIds
        val downloadIndex = plan.downloadedChunkIds.size
        val toDownload = if (expected.size > downloadIndex) {
          expected.slice(downloadIndex, downloadIndex + howMany)
        } else {
          IndexedSeq.empty
        }
        val newDownloaded = plan.downloadedChunkIds ++ toDownload.map(_ => false)
        val newDownloading = plan.downloadingChunks + toDownload.size
        plan.copy(latestUpdateTime = System.currentTimeMillis(), downloadedChunkIds = newDownloaded, downloadingChunks = newDownloading)
        _cachedDownloadPlan = Some(plan) // we update only in-memory cache, so in case of node restart, chunks won't be missed
        toDownload
      case None =>
        log.warn(s"No download plan is found when requested to propose $howMany chunks to download")
        Seq.empty
    }
  }

  def registerDownloadedChunk(chunkId: Array[Byte], chunkSerialized: Array[Byte]): Unit = {
    getUtxoSetSnapshotDownloadPlan() match {
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

  private def updateUtxoSetSnashotDownloadPlan(plan: UtxoSetSnapshotDownloadPlan) = {
    _cachedDownloadPlan = Some(plan)
    historyStorage.insert(downloadPlanKey, plan.id)
    writeDownloadPlanToTheDb(plan) // todo: not always write to db
    plan
  }

  private def writeDownloadPlanToTheDb(plan: UtxoSetSnapshotDownloadPlan) = {
    val w = new VLQByteBufferWriter(new ByteArrayBuilder())
    w.putULong(plan.startingTime)
    w.putULong(plan.latestUpdateTime)
    w.putUInt(plan.snapshotHeight)
    w.putBytes(plan.utxoSetRootHash)
    w.put(plan.utxoSetTreeHeight)
    w.putUInt(plan.expectedChunkIds.size)
    w.putUInt(plan.downloadedChunkIds.size)
    val metaDataBytes = w.result().toBytes

    historyStorage.insert(plan.id, metaDataBytes)

    var idx = 0
    plan.expectedChunkIds.foreach { chunkId =>
      val idxBytes = Ints.toByteArray(idx)
      historyStorage.insert(expectedChunksPrefix ++ idxBytes, chunkId)
      idx = idx + 1
    }

  }

  private def readDownloadPlanFromDb(id: Digest32): Option[UtxoSetSnapshotDownloadPlan] = {
    historyStorage.get(id).map { bytes =>
      val r = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
      val startingTime = r.getULong()
      val latestChunkFetchTime = r.getULong()
      val snapshotHeight = r.getUInt().toInt
      val utxoSetRootHash = r.getBytes(Constants.HashLength)
      val utxoSetTreeHeight = r.getByte()
      val expectedChunksSize = r.getUInt().toInt
      val downloadedChunksSize = r.getUInt().toInt

      val expectedChunks = new mutable.ArrayBuffer[SubtreeId](initialSize = expectedChunksSize)
      (0 until expectedChunksSize).foreach { idx =>
        val idxBytes = Ints.toByteArray(idx)
        historyStorage.get(expectedChunksPrefix ++ idxBytes) match {
          case Some(chunkBytes) => expectedChunks += (Digest32 @@ chunkBytes)
          case None => log.warn(s"Expected chunk #${id} not found in the database")
        }
      }
      val downloadedChunks = new mutable.ArrayBuffer[Boolean](initialSize = downloadedChunksSize)
      (0 until downloadedChunksSize).foreach { idx =>
        val idxBytes = Ints.toByteArray(idx)
        downloadedChunks += historyStorage.contains(downloadedChunksPrefix ++ idxBytes)
      }

      UtxoSetSnapshotDownloadPlan(
        startingTime,
        latestChunkFetchTime,
        snapshotHeight,
        Digest32 @@ utxoSetRootHash,
        utxoSetTreeHeight,
        expectedChunks,
        downloadedChunks,
        downloadingChunks = 0
      )
    }
  }


  import scala.util.Try
  import scorex.crypto.authds.avltree.batch.{BatchAVLProver, PersistentBatchAVLProver, VersionedAVLStorage, VersionedLDBAVLStorage}
  import org.ergoplatform.settings.Algos.HF

  def createPersistentProver(): Try[PersistentBatchAVLProver[Digest32, HF]] = Try {
    val manifest = _manifest.get
    var avlProver: BatchAVLProver[Digest32, HF] =
    val storage: VersionedAVLStorage[Digest32] = new VersionedLDBAVLStorage(???, ???)
    storage.update()
  }
}

//todo: add peers to download from
case class UtxoSetSnapshotDownloadPlan(startingTime: Long,
                                       latestUpdateTime: Long,
                                       snapshotHeight: Height,
                                       utxoSetRootHash: Digest32,
                                       utxoSetTreeHeight: Byte,
                                       expectedChunkIds: IndexedSeq[SubtreeId],
                                       downloadedChunkIds: IndexedSeq[Boolean],
                                       downloadingChunks: Int) {

  def id: Digest32 = utxoSetRootHash

  def totalChunks: Int = expectedChunkIds.size

  def fullyDownloaded: Boolean = {
    (expectedChunkIds.size == downloadedChunkIds.size) &&
      downloadingChunks == 0 &&
      downloadedChunkIds.forall(_ == true)
  }

}

object UtxoSetSnapshotDownloadPlan {

  def fromManifest(manifest: BatchAVLProverManifest[Digest32], blockHeight: Height): UtxoSetSnapshotDownloadPlan = {
    val subtrees = manifest.subtreesIds
    val now = System.currentTimeMillis()
    //todo: fix .toByte below by making height byte
    UtxoSetSnapshotDownloadPlan(now, now, blockHeight, manifest.id, manifest.rootHeight.toByte, subtrees.toIndexedSeq, IndexedSeq.empty, 0)
  }

}
