package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.history.storage.modifierprocessors.UtxoSetSnapshotProcessor
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.nodeView.state.UtxoState.SubtreeId
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.core.VersionTag
import org.ergoplatform.serialization.{ManifestSerializer, SubtreeSerializer}
import scorex.db.LDBVersionedStore
import scorex.util.ModifierId

import scala.util.Random

class UtxoSetSnapshotProcessorSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  val s = settings

  val epochLength = 20

  val utxoSetSnapshotProcessor = new UtxoSetSnapshotProcessor {
    var minimalFullBlockHeightVar = GenesisHeight
    override protected val settings: ErgoSettings = s.copy(chainSettings =
      s.chainSettings.copy(voting = s.chainSettings.voting.copy(votingLength = epochLength)))
    override protected val historyStorage: HistoryStorage = HistoryStorage(settings)
    override def readMinimalFullBlockHeight() = minimalFullBlockHeightVar
    override def writeMinimalFullBlockHeight(height: Int): Unit = {
      minimalFullBlockHeightVar = height
    }
  }

  var history = generateHistory(
    verifyTransactions = true,
    StateType.Utxo,
    PoPoWBootstrap = false,
    blocksToKeep = -1,
    epochLength = epochLength,
    useLastEpochs = 2,
    initialDiffOpt = None)

  val chain = genHeaderChain(epochLength + 1, history, diffBitsOpt = None, useRealTs = false)
  history = applyHeaderChain(history, chain)

  property("registerManifestToDownload + getUtxoSetSnapshotDownloadPlan + getChunkIdsToDownload") {
    val bh     = boxesHolderGenOfSize(32 * 1024).sample.get
    val us     = createUtxoState(bh, parameters)

    val snapshotHeight = epochLength - 1
    val serializer = ManifestSerializer.defaultSerializer

    us.dumpSnapshot(snapshotHeight, us.rootDigest.dropRight(1))
    val manifestId = us.snapshotsDb.readSnapshotsInfo.availableManifests.apply(snapshotHeight)
    val manifestBytes = us.snapshotsDb.readManifestBytes(manifestId).get
    val manifest = serializer.parseBytes(manifestBytes)
    val subtreeIds = manifest.subtreesIds
    val subtreeIdsEncoded = subtreeIds.map(id => ModifierId @@ Algos.encode(id))

    subtreeIds.foreach {sid =>
      val subtreeBytes = us.snapshotsDb.readSubtreeBytes(sid).get
      val subtree = SubtreeSerializer.parseBytes(subtreeBytes)
      subtree.verify(sid) shouldBe true
    }

    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(Random.nextInt(100).toByte))
    utxoSetSnapshotProcessor.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    val dp = utxoSetSnapshotProcessor.utxoSetSnapshotDownloadPlan().get
    dp.snapshotHeight shouldBe snapshotHeight
    val expected = dp.expectedChunkIds.map(id => ModifierId @@ Algos.encode(id))
    expected shouldBe subtreeIdsEncoded
    val toDownload = utxoSetSnapshotProcessor.getChunkIdsToDownload(expected.size).map(id => ModifierId @@ Algos.encode(id))
    toDownload shouldBe expected

    subtreeIds.foreach { subtreeId =>
      val subtreeBytes = us.snapshotsDb.readSubtreeBytes(subtreeId).get
      utxoSetSnapshotProcessor.registerDownloadedChunk(subtreeId, subtreeBytes)
    }
    val s = utxoSetSnapshotProcessor.downloadedChunksIterator().map(s => ModifierId @@ Algos.encode(s.id)).toSeq
    s shouldBe subtreeIdsEncoded

    val dir = createTempDir
    val store = new LDBVersionedStore(dir, initialKeepVersions = 100)
    val restoredProver = utxoSetSnapshotProcessor.createPersistentProver(store, history, snapshotHeight, blockId).get
    bh.sortedBoxes.foreach { box =>
      restoredProver.unauthenticatedLookup(box.id).isDefined shouldBe true
    }
    restoredProver.checkTree(postProof = false)
    val restoredState = new UtxoState(restoredProver, version = VersionTag @@@ blockId, store, settings)
    restoredState.stateContext.currentHeight shouldBe (epochLength - 1)
    bh.sortedBoxes.foreach { box =>
      restoredState.boxById(box.id).isDefined shouldBe true
    }
  }

  /**
    * Registers a manifest of a fresh snapshot in a fresh processor (with its own chunk storage), stores chunk bytes
    * chosen by `storedBytes` (None = chunk is not stored) under the expected chunk ids, and restores the state.
    */
  private def restoreWith(storedBytes: (UtxoState, IndexedSeq[SubtreeId], Int) => Option[Array[Byte]]) = {
    val bh = boxesHolderGenOfSize(32 * 1024).sample.get
    val us = createUtxoState(bh, parameters)
    val snapshotHeight = epochLength - 1

    us.dumpSnapshot(snapshotHeight, us.rootDigest.dropRight(1))
    val manifestId = us.snapshotsDb.readSnapshotsInfo.availableManifests.apply(snapshotHeight)
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(us.snapshotsDb.readManifestBytes(manifestId).get)
    val subtreeIds = manifest.subtreesIds.toIndexedSeq
    subtreeIds.size should be > 2

    val processor = new UtxoSetSnapshotProcessor {
      var minimalFullBlockHeightVar = GenesisHeight
      override protected val settings: ErgoSettings = s.copy(directory = createTempDir.getAbsolutePath, chainSettings =
        s.chainSettings.copy(voting = s.chainSettings.voting.copy(votingLength = epochLength)))
      override protected val historyStorage: HistoryStorage = HistoryStorage(settings)
      override def readMinimalFullBlockHeight() = minimalFullBlockHeightVar
      override def writeMinimalFullBlockHeight(height: Int): Unit = {
        minimalFullBlockHeightVar = height
      }
    }
    processor.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    processor.getChunkIdsToDownload(subtreeIds.size).size shouldBe subtreeIds.size
    subtreeIds.indices.foreach { idx =>
      storedBytes(us, subtreeIds, idx).foreach(bytes => processor.registerDownloadedChunk(subtreeIds(idx), bytes))
    }

    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(Random.nextInt(100).toByte))
    val store = new LDBVersionedStore(createTempDir, initialKeepVersions = 100)
    (bh, store, processor.createPersistentProver(store, history, snapshotHeight, blockId))
  }

  private def chunkBytes(us: UtxoState, id: SubtreeId): Array[Byte] = us.snapshotsDb.readSubtreeBytes(id).get

  private def shouldFailIncomplete(restored: scala.util.Try[_], store: LDBVersionedStore, reason: String) = {
    restored.isFailure shouldBe true
    val error = restored.failed.get
    error shouldBe a[UtxoSetSnapshotProcessor.StateWriteFailure]
    error.getCause shouldBe a[IllegalStateException]
    error.getCause.getMessage should include(reason)
    store.lastVersionID shouldBe None
  }

  property("createPersistentProver - complete snapshot is restored") {
    val (bh, _, restored) = restoreWith((us, ids, idx) => Some(chunkBytes(us, ids(idx))))
    val restoredProver = restored.get
    bh.sortedBoxes.foreach { box =>
      restoredProver.unauthenticatedLookup(box.id).isDefined shouldBe true
    }
    restoredProver.checkTree(postProof = false)
  }

  property("createPersistentProver - fails on a chunk missing on read-back") {
    val (_, store, restored) = restoreWith((us, ids, idx) => if (idx == 1) None else Some(chunkBytes(us, ids(idx))))
    shouldFailIncomplete(restored, store, "chunk #1")
    restored.failed.get.getCause.getMessage should include("is missing")
  }

  property("createPersistentProver - fails on an unparseable stored chunk") {
    val (_, store, restored) = restoreWith((us, ids, idx) => Some(if (idx == 1) Array[Byte](1, 2, 3) else chunkBytes(us, ids(idx))))
    shouldFailIncomplete(restored, store, "chunk #1")
    restored.failed.get.getCause.getMessage should include("can not be parsed")
  }

  property("createPersistentProver - fails on a valid chunk stored under another chunk's id") {
    // chunk count is as in the manifest and every stored chunk is parseable; only chunk identities differ
    val (_, store, restored) = restoreWith((us, ids, idx) => Some(chunkBytes(us, ids(if (idx == 1) 2 else idx))))
    shouldFailIncomplete(restored, store, "chunk #1")
    restored.failed.get.getCause.getMessage should include("read back as")
  }

}
