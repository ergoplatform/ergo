package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.UtxoState.SubtreeId
import org.ergoplatform.settings.Constants
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverManifest
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.{ByteArrayBuilder, ScorexLogging}
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}
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
    minimalFullBlockHeightVar = height + 1  //todo: or height + 1?
  }

  private val expectedChunksPrefix = Blake2b256.hash("expected chunk").drop(4)
  private val downloadedChunksPrefix = Blake2b256.hash("downloaded chunk").drop(4)

  def pruneSnapshot(downloadPlan: UtxoSetSnapshotDownloadPlan) = ???

  def writeDownloadPlanToTheDb(plan: UtxoSetSnapshotDownloadPlan) = {
    val w = new VLQByteBufferWriter(new ByteArrayBuilder())
    w.putULong(plan.startingTime)
    w.putULong(plan.latestChunkFetchTime)
    w.putUInt(plan.snapshotHeight)
    w.putBytes(plan.utxoSetRootHash)
    w.put(plan.utxoSetTreeHeight)
    w.putUInt(plan.totalChunks)
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

    idx = 0
    plan.downloadedChunkIds.foreach { chunkId =>
      val idxBytes = Ints.toByteArray(idx)
      historyStorage.insert(downloadedChunksPrefix ++ idxBytes, chunkId)
      idx = idx + 1
    }
  }

  def readDownloadPlanFromDb(id: Digest32): Option[UtxoSetSnapshotDownloadPlan] = {
    historyStorage.get(id).map {bytes =>
      val r = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
      val startingTime = r.getULong()
      val latestChunkFetchTime = r.getULong()
      val snapshotHeight = r.getUInt().toInt
      val utxoSetRootHash = r.getBytes(Constants.HashLength)
      val utxoSetTreeHeight = r.getByte()
      val totalChunks = r.getUInt().toInt
      val expectedChunksSize = r.getUInt().toInt
      val downloadedChunksSize = r.getUInt().toInt
      val expectedChunks = new mutable.ArrayBuffer[SubtreeId](initialSize = expectedChunksSize)
      (0 until expectedChunksSize).foreach {idx =>
        val idxBytes = Ints.toByteArray(idx)
        historyStorage.get(expectedChunksPrefix ++ idxBytes) match {
          case Some(chunkBytes) => expectedChunks += (Digest32 @@ chunkBytes)
          case None => log.warn(s"Expected chunk #${id} not found in the database")
        }
      }
      val downloadedChunks = new mutable.ArrayBuffer[SubtreeId](initialSize = expectedChunksSize)
      (0 until downloadedChunksSize).foreach {idx =>
        val idxBytes = Ints.toByteArray(idx)
        historyStorage.get(downloadedChunksPrefix ++ idxBytes) match {
          case Some(chunkBytes) => downloadedChunks += (Digest32 @@ chunkBytes)
          case None => log.warn(s"Downloaded chunk #${id} not found in the database")
        }
      }
      UtxoSetSnapshotDownloadPlan(
        startingTime,
        latestChunkFetchTime,
        snapshotHeight,
        Digest32 @@ utxoSetRootHash,
        utxoSetTreeHeight,
        totalChunks,
        expectedChunks,
        downloadedChunks
      )
    }
  }
}

case class UtxoSetSnapshotDownloadPlan(startingTime: Long,
                                       latestChunkFetchTime: Long,
                                       snapshotHeight: Height,
                                       utxoSetRootHash: Digest32,
                                       utxoSetTreeHeight: Byte,
                                       totalChunks: Int,
                                       expectedChunkIds: IndexedSeq[SubtreeId],
                                       downloadedChunkIds: IndexedSeq[SubtreeId]) {
  def id: Digest32 = utxoSetRootHash
}

object UtxoSetSnapshotDownloadPlan {
  def fromManifest(manifest: BatchAVLProverManifest[Digest32], blockHeight: Height): UtxoSetSnapshotDownloadPlan = {
    val subtrees = manifest.subtreesIds
    val now = System.currentTimeMillis()
    //todo: fix .toByte below by making height byte
    UtxoSetSnapshotDownloadPlan(now, now, blockHeight, manifest.id, manifest.rootHeight.toByte, subtrees.size, subtrees.toIndexedSeq, IndexedSeq.empty)
  }
}
