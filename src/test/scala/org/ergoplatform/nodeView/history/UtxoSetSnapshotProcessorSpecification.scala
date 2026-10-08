package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.history.storage.modifierprocessors.{UtxoSetSnapshotDownloadPlan, UtxoSetSnapshotProcessor}
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.core.VersionTag
import org.ergoplatform.serialization.{ManifestSerializer, SubtreeSerializer}
import scorex.db.LDBVersionedStore
import scorex.crypto.hash.Digest32
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

  class TestSnapshotProcessor extends UtxoSetSnapshotProcessor {
    var minimalFullBlockHeightVar = GenesisHeight
    override protected val settings: ErgoSettings = s.copy(chainSettings =
      s.chainSettings.copy(voting = s.chainSettings.voting.copy(votingLength = epochLength)))
    override protected val historyStorage: HistoryStorage = HistoryStorage(settings)
    override def readMinimalFullBlockHeight() = minimalFullBlockHeightVar
    override def writeMinimalFullBlockHeight(height: Int): Unit = {
      minimalFullBlockHeightVar = height
    }
    def setPlanForTest(plan: UtxoSetSnapshotDownloadPlan): Unit = updateUtxoSetSnashotDownloadPlan(plan)
  }

  val utxoSetSnapshotProcessor = new TestSnapshotProcessor

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

  property("released snapshot chunk slots are reused before new positions") {
    val state = createUtxoState(boxesHolderGenOfSize(32 * 1024).sample.get, parameters)
    val height = epochLength - 1
    state.dumpSnapshot(height, state.rootDigest.dropRight(1), ManifestSerializer.MainnetManifestDepth).get
    val id = state.getSnapshotInfo().availableManifests(height)
    val manifest = new ManifestSerializer(ManifestSerializer.MainnetManifestDepth)
      .parseBytes(state.getManifestBytes(id).get)
    manifest.subtreesIds.size should be > 2

    utxoSetSnapshotProcessor.registerManifestToDownload(manifest, height, Seq.empty)
    val first = utxoSetSnapshotProcessor.getChunkIdsToDownload(2)
    utxoSetSnapshotProcessor.releaseChunkDownload(first.head)
    val next = utxoSetSnapshotProcessor.getChunkIdsToDownload(2)
    next.head.sameElements(first.head) shouldBe true
    next(1).sameElements(manifest.subtreesIds(2)) shouldBe true
    utxoSetSnapshotProcessor.utxoSetSnapshotDownloadPlan().get.downloadingChunks shouldBe 3
  }

  property("repeated chunk ids release and complete every reserved position") {
    val repeated = Digest32 @@ Array.fill[Byte](32)(1)
    val other = Digest32 @@ Array.fill[Byte](32)(2)
    val plan = UtxoSetSnapshotDownloadPlan(
      createdTime = 0L,
      latestUpdateTime = 0L,
      snapshotHeight = epochLength - 1,
      utxoSetRootHash = Digest32 @@ Array.fill[Byte](32)(3),
      utxoSetTreeHeight = 1.toByte,
      expectedChunkIds = IndexedSeq(repeated, repeated, other),
      downloadedChunkIds = IndexedSeq.empty,
      downloadingChunks = 0,
      peersToDownload = Seq.empty)
    utxoSetSnapshotProcessor.setPlanForTest(plan)

    utxoSetSnapshotProcessor.getChunkIdsToDownload(3).size shouldBe 3
    utxoSetSnapshotProcessor.releaseChunkDownload(repeated)
    val released = utxoSetSnapshotProcessor.utxoSetSnapshotDownloadPlan().get
    released.reservedChunkIndices shouldBe Set(2)
    released.releasedChunkIndices shouldBe Set(0, 1)
    released.downloadingChunks shouldBe 1

    val retried = utxoSetSnapshotProcessor.getChunkIdsToDownload(2)
    retried.size shouldBe 2
    retried.foreach(_.sameElements(repeated) shouldBe true)
    utxoSetSnapshotProcessor.registerDownloadedChunk(repeated, Array[Byte](1))
    val afterRepeated = utxoSetSnapshotProcessor.utxoSetSnapshotDownloadPlan().get
    afterRepeated.downloadedChunkIds shouldBe IndexedSeq(true, true, false)
    afterRepeated.downloadingChunks shouldBe 1

    utxoSetSnapshotProcessor.registerDownloadedChunk(other, Array[Byte](2))
    utxoSetSnapshotProcessor.utxoSetSnapshotDownloadPlan().get.fullyDownloaded shouldBe true
  }

}
