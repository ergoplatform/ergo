package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoState}
import org.ergoplatform.nodeView.state.UtxoState.SubtreeId
import org.ergoplatform.settings.{Algos, ErgoAlgos, ErgoSettings}
import org.ergoplatform.core.VersionTag
import scorex.core.network.ConnectedPeer
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.serialization.{ManifestSerializer, SubtreeSerializer}
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSubtree}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.db.{ByteArrayWrapper, LDBVersionedStore}
import scorex.util.{ModifierId, ScorexLogging}
import scala.util.{Failure, Random, Success, Try}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, PersistentBatchAVLProver, VersionedLDBAVLStorage}

/**
  * Parts of history processing and storage corresponding to UTXO set snapshot processing and storage
  *
  * Stores UTXO set snapshots manifests and chunks for incomplete snapshots.
  */
trait UtxoSetSnapshotProcessor
  extends MinimalFullBlockHeightFunctions
    with UtxoSnapshotScanSourceReader
    with ScorexLogging {

  import org.ergoplatform.settings.ErgoAlgos.HF

  // node config to read history-related settings here and in descendants
  protected val settings: ErgoSettings

  // database to read history-related objects here and in descendants
  protected val historyStorage: HistoryStorage

  private var _manifest: Option[BatchAVLProverManifest[Digest32]] = None

  private var _manifestBytes: Option[Array[Byte]] = None

  private var _cachedDownloadPlan: Option[UtxoSetSnapshotDownloadPlan] = None

  protected def minimalFullBlockHeightKey: ByteArrayWrapper

  protected def snapshotHeaderStateAtHeight(height: Height): Option[(ModifierId, ADDigest)]

  private def required[A](value: Option[A], error: => Throwable): Try[A] = value match {
    case Some(result) => Success(result)
    case None => Failure(error)
  }

  /**
    * @return if UTXO set snapshot was applied during this session (stored in memory only).
    *         This flag is needed to prevent double application of UTXO set snapshot.
    *         After first full-block block application not needed anymore.
    */
  def isUtxoSnapshotApplied: Boolean = {
    readMinimalFullBlockHeight() > GenesisHeight
  }

  /**
    * Writes that UTXO set snapshot applied at height `height`. Starts full blocks applications since the next block
    * after.
    */
  def onUtxoSnapshotApplied(height: Height, blockId: ModifierId): Try[Unit] = {
    val utxoPhaseTime = {
      _cachedDownloadPlan.map(_.latestUpdateTime).getOrElse(0L) - _cachedDownloadPlan.map(_.createdTime).getOrElse(0L)
    }
    val result = for {
      manifest <- required(_manifest, new IllegalStateException("No UTXO snapshot manifest to finalize"))
      manifestBytes <- required(_manifestBytes,
        new IllegalStateException("No exact UTXO snapshot manifest bytes to finalize"))
      plan <- required(_cachedDownloadPlan,
        new IllegalStateException("No UTXO snapshot download plan to finalize"))
      _ <- Try(require(plan.snapshotHeight == height,
        s"Snapshot height ${plan.snapshotHeight} does not match applied height $height"))
      _ <- Try(require(plan.fullyDownloaded, "UTXO snapshot is not fully downloaded"))
      _ <- Try(require(plan.expectedChunkIds.size == manifest.subtreesIds.size &&
        plan.expectedChunkIds.zip(manifest.subtreesIds).forall { case (a, b) => a.sameElements(b) },
        "UTXO snapshot download plan does not match manifest chunk identifiers"))
      serializedManifest <- ManifestSerializer.defaultSerializer.parseBytesTry(manifestBytes)
      _ <- Try(require(
        serializedManifest.id.sameElements(manifest.id) &&
          serializedManifest.rootHeight == manifest.rootHeight &&
          serializedManifest.subtreesIds.size == manifest.subtreesIds.size &&
          serializedManifest.subtreesIds.zip(manifest.subtreesIds).forall {
            case (a, b) => a.sameElements(b)
          },
        "Exact UTXO snapshot manifest bytes do not match the download plan"
      ))
      _ <- Try {
        plan.expectedChunkIds.indices.foreach { ordinal =>
          val bytes = historyStorage.get(chunkIdFromIndex(ordinal)).getOrElse(
            throw new IllegalStateException(s"Missing downloaded UTXO snapshot chunk $ordinal"))
          val subtree = SubtreeSerializer.parseBytesTry(bytes).get
          require(subtree.verify(plan.expectedChunkIds(ordinal)),
            s"Downloaded UTXO snapshot chunk $ordinal does not match its manifest identifier")
        }
      }
      header <- required(snapshotHeaderStateAtHeight(height),
        new IllegalStateException(s"No header found for UTXO snapshot height $height"))
      _ <- Try(require(header._1 == blockId,
        s"Snapshot block $blockId does not match header ${header._1} at height $height"))
      _ <- Try(require(header._2.sameElements(VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)),
        s"Snapshot manifest root does not match header state root at height $height"))
      source <- UtxoSnapshotScanSource.create(
        height,
        blockId,
        ManifestSerializer.MainnetManifestDepth,
        manifestBytes
      )
      _ <- historyStorage.insert(
        indexesToInsert = Array(
          UtxoSetSnapshotProcessor.SnapshotScanSourceKey -> UtxoSnapshotScanSourceSerializer.toBytes(source),
          minimalFullBlockHeightKey -> Ints.toByteArray(height + 1)
        ),
        objectsToInsert = BlockSection.emptyArray
      )
    } yield {
      _manifest = None
      _manifestBytes = None
      _cachedDownloadPlan = None
      log.info(s"UTXO set downloading and application time: ${utxoPhaseTime / 1000.0} s.")
    }
    result
  }

  private def updateUtxoSetSnashotDownloadPlan(plan: UtxoSetSnapshotDownloadPlan): Unit = {
    _cachedDownloadPlan = Some(plan)
  }

  /**
    * Register manifest as one to be downloaded and create download plan from it
    * @param manifest - manifest corresponding to UTXO set snapshot to be downloaded
    * @param manifestBytes - exact serialized manifest bytes accepted from the network
    * @param blockHeight - height of a block corresponding to the manifest
    * @param peersToDownload - peers to download chunks related to manifest from
    * @return download plan
    */
  def registerManifestToDownload(manifest: BatchAVLProverManifest[Digest32],
                                 manifestBytes: Array[Byte],
                                 blockHeight: Height,
                                 peersToDownload: Seq[ConnectedPeer]): UtxoSetSnapshotDownloadPlan = {
    val plan = UtxoSetSnapshotDownloadPlan.fromManifest(manifest, blockHeight, peersToDownload)
    _manifest = Some(manifest)
    _manifestBytes = Some(manifestBytes.clone())
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
  def registerDownloadedChunk(chunkId: Array[Byte], chunkSerialized: Array[Byte]): Try[Unit] = {
    SubtreeSerializer.parseBytesTry(chunkSerialized).flatMap { subtree =>
      if (!subtree.verify(Digest32 @@ chunkId)) {
        Failure(new IllegalArgumentException(
          s"Downloaded UTXO snapshot chunk ${Algos.encode(chunkId)} has a different root"))
      } else utxoSetSnapshotDownloadPlan() match {
      case Some(plan) =>
        plan.downloadedChunkIds.indices
          .find(idx => !plan.downloadedChunkIds(idx) && plan.expectedChunkIds(idx).sameElements(chunkId)) match {
          case Some(idx) => historyStorage.insert(chunkIdFromIndex(idx), chunkSerialized).map { _ =>
            val updDownloaded = plan.downloadedChunkIds.updated(idx, true)
            val updDownloading = plan.downloadingChunks - 1
            val updPlan = plan.copy(latestUpdateTime = System.currentTimeMillis(), downloadedChunkIds = updDownloaded, downloadingChunks = updDownloading)
            updateUtxoSetSnashotDownloadPlan(updPlan)
          }
          case None => Failure(new IllegalArgumentException(
            s"Chunk ${Algos.encode(chunkId)} is not pending in the UTXO snapshot download plan"))
        }
      case None =>
        Failure(new IllegalStateException(
          s"Chunk ${Algos.encode(chunkId)} downloaded but no download plan found"))
      }
    }
  }

  /** Whether a completed plan still authenticates the current best header and block identity. */
  def isUtxoSetSnapshotDownloadCurrent(height: Height, blockId: ModifierId): Boolean =
    _cachedDownloadPlan.exists { plan =>
      plan.fullyDownloaded && plan.snapshotHeight == height &&
        snapshotHeaderStateAtHeight(height).exists { case (currentBlockId, stateRoot) =>
          currentBlockId == blockId && stateRoot.sameElements(
            VersionedLDBAVLStorage.digest(
              plan.utxoSetRootHash,
              plan.utxoSetTreeHeight & 0xff
            )
          )
        }
    }

  /** Remove a stale in-progress snapshot and all ordinal chunk rows it allocated. */
  def invalidateUtxoSetSnapshotDownload(): Try[Unit] =
    _cachedDownloadPlan match {
      case Some(plan) =>
        val ordinalChunkKeys =
          plan.downloadedChunkIds.indices.map(chunkIdFromIndex).toArray
        val cleanup = if (ordinalChunkKeys.isEmpty) Success(())
        else historyStorage.removeRawObjects(ordinalChunkKeys)
        cleanup.map { _ =>
          _manifest = None
          _manifestBytes = None
          _cachedDownloadPlan = None
        }
      case None => Success(())
    }

  private def chunkIdFromIndex(index: Int): Array[Byte] =
    UtxoSetSnapshotProcessor.snapshotScanChunkKey(index)

  private def downloadedChunkIdsIterator(totalChunks: Int): Iterator[Array[Byte]] = {
    Iterator.range(0, totalChunks).map(chunkIdFromIndex)
  }

  /**
    * @return iterator for chunks downloaded. Reads them from database one-by-one when requested.
    */
  def downloadedChunksIterator(): Iterator[BatchAVLProverSubtree[Digest32]] = {
    utxoSetSnapshotDownloadPlan() match {
      case Some(plan) =>
        downloadedChunkIdsIterator(plan.totalChunks).map { chunkId =>
          val bytes = historyStorage.get(chunkId).getOrElse(
            throw new IllegalStateException(s"Missing downloaded UTXO snapshot chunk ${Algos.encode(chunkId)}"))
          SubtreeSerializer.parseBytesTry(bytes).get
        }
      case None =>
        log.error("No download plan found in downloadedChunksIterator")
      Iterator.empty
    }
  }

  /** Read and validate the persisted immutable source for the expected snapshot block. */
  def readUtxoSnapshotScanSource(expectedBlockId: ModifierId): Try[UtxoSnapshotScanSource] = for {
    bytes <- required(historyStorage.getIndex(UtxoSetSnapshotProcessor.SnapshotScanSourceKey),
      new IllegalStateException("No persisted UTXO snapshot scan source"))
    source <- UtxoSnapshotScanSourceSerializer.parseBytesTry(bytes)
    _ <- Try(require(source.snapshotBlockId == expectedBlockId,
      s"Persisted snapshot block ${source.snapshotBlockId} does not match expected $expectedBlockId"))
    header <- required(snapshotHeaderStateAtHeight(source.snapshotHeight),
      new IllegalStateException(s"No header found for persisted snapshot height ${source.snapshotHeight}"))
    _ <- Try(require(header._1 == expectedBlockId,
      s"Persisted snapshot identity does not match the header at height ${source.snapshotHeight}"))
    manifest <- new ManifestSerializer(source.manifestDepth).parseBytesTry(source.manifestBytes)
    _ <- Try(require(header._2.sameElements(VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)),
      s"Persisted snapshot manifest root does not match the header at height ${source.snapshotHeight}"))
  } yield source

  /** Read and verify one persisted scan part by deterministic index. */
  def readUtxoSnapshotScanPart(source: UtxoSnapshotScanSource,
                               index: Int): Try[BatchAVLProverSubtree[Digest32]] =
    source.readPart(index, ordinal =>
      required(historyStorage.get(UtxoSetSnapshotProcessor.snapshotScanChunkKey(ordinal)),
        new IllegalStateException(s"Missing retained UTXO snapshot chunk $ordinal")))

  /** Remove retained chunks, then the descriptor, safely across retries. */
  def removeUtxoSnapshotScanSource(expectedBlockId: ModifierId): Try[Unit] =
    historyStorage.getIndex(UtxoSetSnapshotProcessor.SnapshotScanSourceKey) match {
      case None => Success(())
      case Some(bytes) => for {
        source <- UtxoSnapshotScanSourceSerializer.parseBytesTry(bytes)
        _ <- Try(require(source.snapshotBlockId == expectedBlockId,
          s"Persisted snapshot block ${source.snapshotBlockId} does not match expected $expectedBlockId"))
        _ <- historyStorage.removeRawObjects(
          Array.range(0, source.chunkCount).map(UtxoSetSnapshotProcessor.snapshotScanChunkKey))
        _ <- historyStorage.remove(
          Array(UtxoSetSnapshotProcessor.SnapshotScanSourceKey),
          Array.empty[ModifierId])
      } yield ()
    }

  /**
    * Create disk-persistent authenticated AVL+ tree prover
    * @param stateStore - disk database where AVL+ tree will be after restoration
    * @param historyReader - history readed to get headers to restore state context
    * @param height - height for which prover will be created (prover state will correspond to a
    *                 moment after application of a block at this height)
    * @param blockId - id of a block corresponding to the tree (tree is on top of a state after the block)
    * @return prover with initialized tree database
    */
  def createPersistentProver(stateStore: LDBVersionedStore,
                             historyReader: ErgoHistoryReader,
                             height: Height,
                             blockId: ModifierId): Try[PersistentBatchAVLProver[Digest32, HF]] = {
    _manifest match {
      case Some(manifest) =>
        log.info("Starting UTXO set snapshot transfer into state database")
        ErgoStateReader.reconstructStateContextBeforeEpoch(historyReader, height, settings) match {
          case Success(esc) =>
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
          case Failure(e) =>
            log.warn("Can't reconstruct state context in createPersistentProver ", e)
            Failure(e)
        }
      case None =>
        val msg = "No manifest available in createPersistentProver"
        log.error(msg)
        Failure(new Exception(msg))
    }
  }

}

object UtxoSetSnapshotProcessor {
  private val DownloadedChunksPrefix = Blake2b256.hash("downloaded chunk").drop(4)

  private[history] val SnapshotScanSourceKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("utxo snapshot scan source"))

  /** Exact object-store key for a retained ordinal snapshot chunk. */
  def snapshotScanChunkKey(index: Int): Array[Byte] =
    DownloadedChunksPrefix ++ Ints.toByteArray(index)
}
