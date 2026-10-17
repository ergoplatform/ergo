package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.modifiers.SnapshotsInfoTypeId
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.storage.modifierprocessors.UtxoSetSnapshotProcessor.SnapshotCheckpointMismatch
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.core.VersionTag
import org.ergoplatform.serialization.{ManifestSerializer, SubtreeSerializer}
import scorex.crypto.authds.ADDigest
import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage
import scorex.db.LDBVersionedStore
import scorex.util.ModifierId

class UtxoSetSnapshotProcessorSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  val s = settings

  val epochLength = 20

  private def historyWithCheckpoint(snapshotHeight: Int, stateRoot: ADDigest): ErgoHistory = {
    val historySettings = s.copy(
      directory = createTempDir.getAbsolutePath,
      chainSettings = s.chainSettings.copy(
        epochLength = epochLength,
        useLastEpochs = 2,
        voting = s.chainSettings.voting.copy(votingLength = epochLength)),
      nodeSettings = s.nodeSettings.copy(
        stateType = StateType.Utxo,
        verifyTransactions = true,
        blocksToKeep = -1,
        utxoSettings = s.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val initialHistory = ErgoHistory.readOrGenerate(historySettings)(null)
    val prefix = genHeaderChain(snapshotHeight - 1, initialHistory, diffBitsOpt = None, useRealTs = false)
    val prefixHistory = applyHeaderChain(initialHistory, prefix)
    val checkpoint = powScheme.prove(
      prefixHistory.bestHeaderOpt,
      Header.InitialVersion,
      s.chainSettings.initialNBits,
      stateRoot,
      EmptyDigest32,
      EmptyDigest32,
      System.currentTimeMillis(),
      EmptyDigest32,
      Array.fill(3)(0: Byte),
      defaultMinerSecretNumber).get
    val selectedHistory = prefixHistory.append(checkpoint).get._1
    selectedHistory.bestHeaderAtHeight(snapshotHeight).map(_.id) shouldBe Some(checkpoint.id)
    selectedHistory.setHeadersChainSynced()
    selectedHistory
  }

  private def selectCompetingCheckpoint(history: ErgoHistory,
                                        snapshotHeight: Int,
                                        stateRoot: ADDigest): Header = {
    val original = history.bestHeaderAtHeight(snapshotHeight).get
    val parent = history.bestHeaderAtHeight(snapshotHeight - 1).get
    val competing = powScheme.prove(
      Some(parent),
      Header.InitialVersion,
      s.chainSettings.initialNBits,
      stateRoot,
      EmptyDigest32,
      EmptyDigest32,
      Math.max(System.currentTimeMillis(), original.timestamp + 1),
      EmptyDigest32,
      Array.fill(3)(0: Byte),
      defaultMinerSecretNumber).get
    competing.id should not be original.id
    history.append(competing).get
    val tip = nextHeader(
      prev = Some(competing),
      control = history.difficultyCalculator,
      tsOpt = Some(competing.timestamp + 1),
      diffBitsOpt = None,
      useRealTs = false)
    history.append(tip).get
    history.bestHeaderOpt.map(_.id) shouldBe Some(tip.id)
    history.bestHeaderAtHeight(snapshotHeight).map(_.id) shouldBe Some(competing.id)
    competing
  }

  property("snapshot session IDs are distinct across history instances") {
    val bh = boxesHolderGenOfSize(32 * 1024).sample.get
    val us = createUtxoState(bh, parameters)
    val snapshotHeight = epochLength - 1
    us.dumpSnapshot(snapshotHeight, us.rootDigest.dropRight(1)).get
    val manifestId = us.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeight)
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(us.snapshotsDb.readManifestBytes(manifestId).get)
    val first = historyWithCheckpoint(snapshotHeight, us.rootDigest)
    val second = historyWithCheckpoint(snapshotHeight, us.rootDigest)

    val firstSession = first.registerManifestToDownload(manifest, snapshotHeight, Seq.empty).sessionId
    val secondSession = second.registerManifestToDownload(manifest, snapshotHeight, Seq.empty).sessionId
    firstSession should not be secondSession
  }

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

    val selectedHistory = historyWithCheckpoint(snapshotHeight, us.rootDigest)
    val selectedHeader = selectedHistory.bestHeaderAtHeight(snapshotHeight).get
    java.util.Arrays.equals(selectedHeader.stateRoot,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)) shouldBe true

    selectedHistory.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    val dp = selectedHistory.utxoSetSnapshotDownloadPlan().get
    dp.snapshotHeight shouldBe snapshotHeight
    val expected = dp.expectedChunkIds.map(id => ModifierId @@ Algos.encode(id))
    expected shouldBe subtreeIdsEncoded
    val toDownload = selectedHistory.getChunkIdsToDownload(expected.size).map(id => ModifierId @@ Algos.encode(id))
    toDownload shouldBe expected

    subtreeIds.foreach { subtreeId =>
      val subtreeBytes = us.snapshotsDb.readSubtreeBytes(subtreeId).get
      selectedHistory.registerDownloadedChunk(subtreeId, subtreeBytes)
    }
    val downloaded = selectedHistory.downloadedChunksIterator().map(s => ModifierId @@ Algos.encode(s.id)).toSeq
    downloaded shouldBe subtreeIdsEncoded

    val dir = createTempDir
    val store = new LDBVersionedStore(dir, initialKeepVersions = 100)
    val restoredProver = selectedHistory.createPersistentProver(store, selectedHistory,
      snapshotHeight, selectedHeader.id, dp.sessionId).get
    bh.sortedBoxes.foreach { box =>
      restoredProver.unauthenticatedLookup(box.id).isDefined shouldBe true
    }
    restoredProver.checkTree(postProof = false)
    val restoredState = new UtxoState(restoredProver, version = VersionTag @@@ selectedHeader.id, store, settings)
    restoredState.stateContext.currentHeight shouldBe (epochLength - 1)
    bh.sortedBoxes.foreach { box =>
      restoredState.boxById(box.id).isDefined shouldBe true
    }
  }

  property("snapshot restoration rejects a selected header with the right root but wrong tree height") {
    val bh = boxesHolderGenOfSize(32 * 1024).sample.get
    val us = createUtxoState(bh, parameters)
    val snapshotHeight = epochLength - 1
    us.dumpSnapshot(snapshotHeight, us.rootDigest.dropRight(1)).get
    val manifestId = us.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeight)
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(us.snapshotsDb.readManifestBytes(manifestId).get)
    val wrongHeightDigest = VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight + 1)
    val selectedHistory = historyWithCheckpoint(snapshotHeight, wrongHeightDigest)
    val selectedHeader = selectedHistory.bestHeaderAtHeight(snapshotHeight).get
    java.util.Arrays.equals(selectedHeader.stateRoot.dropRight(1), manifest.id) shouldBe true
    selectedHeader.stateRoot.last should not be us.rootDigest.last

    val plan = selectedHistory.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    selectedHistory.getChunkIdsToDownload(manifest.subtreesIds.size)
    manifest.subtreesIds.foreach { subtreeId =>
      selectedHistory.registerDownloadedChunk(subtreeId, us.snapshotsDb.readSubtreeBytes(subtreeId).get)
    }
    selectedHistory.utxoSetSnapshotDownloadPlan().get.fullyDownloaded shouldBe true

    val store = new LDBVersionedStore(createTempDir, initialKeepVersions = 100)
    store.lastVersionID shouldBe None
    selectedHistory.createPersistentProver(store, selectedHistory,
      snapshotHeight, selectedHeader.id, plan.sessionId).failed.get shouldBe a[SnapshotCheckpointMismatch]
    store.lastVersionID shouldBe None
  }

  property("snapshot restoration rejects an incomplete download plan") {
    val bh = boxesHolderGenOfSize(32 * 1024).sample.get
    val us = createUtxoState(bh, parameters)
    val snapshotHeight = epochLength - 1
    us.dumpSnapshot(snapshotHeight, us.rootDigest.dropRight(1)).get
    val manifestId = us.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeight)
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(us.snapshotsDb.readManifestBytes(manifestId).get)
    val selectedHistory = historyWithCheckpoint(snapshotHeight, us.rootDigest)
    val selectedHeader = selectedHistory.bestHeaderAtHeight(snapshotHeight).get
    val plan = selectedHistory.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    plan.expectedChunkIds.nonEmpty shouldBe true
    selectedHistory.utxoSetSnapshotDownloadPlan().get.fullyDownloaded shouldBe false

    val store = new LDBVersionedStore(createTempDir, initialKeepVersions = 100)
    store.lastVersionID shouldBe None
    selectedHistory.createPersistentProver(store, selectedHistory,
      snapshotHeight, selectedHeader.id, plan.sessionId).failed.get shouldBe a[SnapshotCheckpointMismatch]
    store.lastVersionID shouldBe None
  }

  property("snapshot restoration rejects another download session") {
    val bh = boxesHolderGenOfSize(32 * 1024).sample.get
    val us = createUtxoState(bh, parameters)
    val snapshotHeight = epochLength - 1
    us.dumpSnapshot(snapshotHeight, us.rootDigest.dropRight(1)).get
    val manifestId = us.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeight)
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(us.snapshotsDb.readManifestBytes(manifestId).get)
    val selectedHistory = historyWithCheckpoint(snapshotHeight, us.rootDigest)
    val selectedHeader = selectedHistory.bestHeaderAtHeight(snapshotHeight).get
    val plan = selectedHistory.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    selectedHistory.getChunkIdsToDownload(manifest.subtreesIds.size)
    manifest.subtreesIds.foreach { subtreeId =>
      selectedHistory.registerDownloadedChunk(subtreeId, us.snapshotsDb.readSubtreeBytes(subtreeId).get)
    }
    selectedHistory.utxoSetSnapshotDownloadPlan().get.fullyDownloaded shouldBe true
    val wrongSession = plan.sessionId + 1L
    wrongSession should not be plan.sessionId

    val store = new LDBVersionedStore(createTempDir, initialKeepVersions = 100)
    store.lastVersionID shouldBe None
    selectedHistory.createPersistentProver(store, selectedHistory,
      snapshotHeight, selectedHeader.id, wrongSession).failed.get shouldBe a[SnapshotCheckpointMismatch]
    store.lastVersionID shouldBe None
  }

  property("snapshot restoration rejects a manifest for a different selected header") {
    val bh = boxesHolderGenOfSize(32 * 1024).sample.get
    val us = createUtxoState(bh, parameters)
    val snapshotHeight = epochLength - 1
    us.dumpSnapshot(snapshotHeight, us.rootDigest.dropRight(1)).get

    val manifestId = us.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeight)
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(us.snapshotsDb.readManifestBytes(manifestId).get)
    val selectedHistory = historyWithCheckpoint(snapshotHeight, EmptyStateRoot)
    val selectedHeader = selectedHistory.bestHeaderAtHeight(snapshotHeight).get
    java.util.Arrays.equals(selectedHeader.stateRoot, us.rootDigest) shouldBe false

    val plan = selectedHistory.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    selectedHistory.getChunkIdsToDownload(manifest.subtreesIds.size)
    manifest.subtreesIds.foreach { subtreeId =>
      selectedHistory.registerDownloadedChunk(subtreeId, us.snapshotsDb.readSubtreeBytes(subtreeId).get)
    }

    val store = new LDBVersionedStore(createTempDir, initialKeepVersions = 100)
    store.lastVersionID shouldBe None
    val result = selectedHistory.createPersistentProver(store, selectedHistory,
      snapshotHeight, selectedHeader.id, plan.sessionId)
    result.failed.get shouldBe a[SnapshotCheckpointMismatch]
    store.lastVersionID shouldBe None
  }

  property("snapshot restoration rejects a completed plan registered at another height") {
    val bh = boxesHolderGenOfSize(32 * 1024).sample.get
    val us = createUtxoState(bh, parameters)
    val snapshotHeight = epochLength - 1
    us.dumpSnapshot(snapshotHeight, us.rootDigest.dropRight(1)).get
    val manifestId = us.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeight)
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(us.snapshotsDb.readManifestBytes(manifestId).get)
    val selectedHistory = historyWithCheckpoint(snapshotHeight, us.rootDigest)
    val selectedHeader = selectedHistory.bestHeaderAtHeight(snapshotHeight).get

    val plan = selectedHistory.registerManifestToDownload(manifest, snapshotHeight - 1, Seq.empty)
    selectedHistory.getChunkIdsToDownload(manifest.subtreesIds.size)
    manifest.subtreesIds.foreach { subtreeId =>
      selectedHistory.registerDownloadedChunk(subtreeId, us.snapshotsDb.readSubtreeBytes(subtreeId).get)
    }
    selectedHistory.utxoSetSnapshotDownloadPlan().get.fullyDownloaded shouldBe true

    val store = new LDBVersionedStore(createTempDir, initialKeepVersions = 100)
    store.lastVersionID shouldBe None
    selectedHistory.createPersistentProver(store, selectedHistory,
      snapshotHeight, selectedHeader.id, plan.sessionId).failed.get shouldBe a[SnapshotCheckpointMismatch]
    store.lastVersionID shouldBe None
  }

  property("snapshot restoration rejects a stale block ID after same-root selection change") {
    val bh = boxesHolderGenOfSize(32 * 1024).sample.get
    val us = createUtxoState(bh, parameters)
    val snapshotHeight = epochLength - 1
    us.dumpSnapshot(snapshotHeight, us.rootDigest.dropRight(1)).get
    val manifestId = us.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeight)
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(us.snapshotsDb.readManifestBytes(manifestId).get)
    val selectedHistory = historyWithCheckpoint(snapshotHeight, us.rootDigest)
    val original = selectedHistory.bestHeaderAtHeight(snapshotHeight).get
    val plan = selectedHistory.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    selectedHistory.getChunkIdsToDownload(manifest.subtreesIds.size)
    manifest.subtreesIds.foreach { subtreeId =>
      selectedHistory.registerDownloadedChunk(subtreeId, us.snapshotsDb.readSubtreeBytes(subtreeId).get)
    }

    val competing = selectCompetingCheckpoint(selectedHistory, snapshotHeight, us.rootDigest)
    java.util.Arrays.equals(competing.stateRoot, original.stateRoot) shouldBe true
    val store = new LDBVersionedStore(createTempDir, initialKeepVersions = 100)
    store.lastVersionID shouldBe None
    selectedHistory.createPersistentProver(store, selectedHistory,
      snapshotHeight, original.id, plan.sessionId).failed.get shouldBe a[SnapshotCheckpointMismatch]
    store.lastVersionID shouldBe None
  }

  property("a displaced snapshot is discarded without advancing the full-block floor") {
    val bh = boxesHolderGenOfSize(32 * 1024).sample.get
    val us = createUtxoState(bh, parameters)
    val snapshotHeight = epochLength - 1
    us.dumpSnapshot(snapshotHeight, us.rootDigest.dropRight(1)).get
    val manifestId = us.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeight)
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(us.snapshotsDb.readManifestBytes(manifestId).get)
    val selectedHistory = historyWithCheckpoint(snapshotHeight, us.rootDigest)
    selectedHistory.isHeadersChainSynced shouldBe true
    val plan = selectedHistory.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    selectedHistory.getChunkIdsToDownload(manifest.subtreesIds.size)
    manifest.subtreesIds.foreach { subtreeId =>
      selectedHistory.registerDownloadedChunk(subtreeId, us.snapshotsDb.readSubtreeBytes(subtreeId).get)
    }
    selectedHistory.utxoSetSnapshotDownloadPlan().get.fullyDownloaded shouldBe true
    selectedHistory.downloadedChunksIterator().hasNext shouldBe true

    val competing = selectCompetingCheckpoint(selectedHistory, snapshotHeight, EmptyStateRoot)
    java.util.Arrays.equals(competing.stateRoot, us.rootDigest) shouldBe false
    val store = new LDBVersionedStore(createTempDir, initialKeepVersions = 100)
    store.lastVersionID shouldBe None
    selectedHistory.createPersistentProver(store, selectedHistory,
      snapshotHeight, competing.id, plan.sessionId).failed.get shouldBe a[SnapshotCheckpointMismatch]
    store.lastVersionID shouldBe None

    selectedHistory.discardSnapshotDownload(plan.sessionId)
    selectedHistory.utxoSetSnapshotDownloadPlan() shouldBe None
    selectedHistory.minimalFullBlockHeight shouldBe GenesisHeight
    selectedHistory.isUtxoSnapshotApplied shouldBe false
    selectedHistory.nextModifiersToDownload(10, (_, _) => true).keySet should contain(SnapshotsInfoTypeId.value)
    selectedHistory.registerManifestToDownload(manifest, snapshotHeight, Seq.empty)
    selectedHistory.downloadedChunksIterator().hasNext shouldBe false
  }

}
