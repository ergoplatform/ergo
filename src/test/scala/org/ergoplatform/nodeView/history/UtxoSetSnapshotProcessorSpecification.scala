package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.history.storage.modifierprocessors.UtxoSetSnapshotProcessor
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.core.VersionTag
import org.ergoplatform.serialization.{ManifestSerializer, SubtreeSerializer}
import scorex.db.{ByteArrayWrapper, LDBKVStore, LDBVersionedStore}
import scorex.util.ModifierId

import scala.collection.mutable
import scala.util.Random
import scala.util.{Failure, Success, Try}

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

  property("snapshot floor failure preserves the plan and chunks for retry") {
    val holder = boxesHolderGenOfSize(32 * 1024).sample.get
    val state = createUtxoState(holder, parameters)
    val height = epochLength - 1
    state.dumpSnapshot(height, state.rootDigest.dropRight(1)).get
    val manifestId = state.snapshotsDb.readSnapshotsInfo.availableManifests(height)
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(state.snapshotsDb.readManifestBytes(manifestId).get)
    val index = new SnapshotStore
    val objects = new SnapshotStore
    val extra = new SnapshotStore
    val configured = s.copy(
      nodeSettings = s.nodeSettings.copy(extraIndex = false),
      chainSettings = s.chainSettings.copy(voting = s.chainSettings.voting.copy(votingLength = epochLength)))
    def openHistory(): ErgoHistory =
      ErgoHistory.readOrGenerate(configured, new HistoryStorage(index, objects, extra, s.cacheSettings))(null)

    val snapshotHistory = openHistory()
    try {
      snapshotHistory.registerManifestToDownload(manifest, height, Seq.empty)
      val requested = snapshotHistory.getChunkIdsToDownload(manifest.subtreesIds.size)
      requested.size should be > 0
      requested.foreach(id => snapshotHistory.registerDownloadedChunk(id, state.snapshotsDb.readSubtreeBytes(id).get))
      val before = snapshotHistory.downloadedChunksIterator().map(c => Algos.encode(c.id)).toVector
      before.size shouldBe requested.size
      val plan = snapshotHistory.utxoSetSnapshotDownloadPlan()
      val failure = new IllegalStateException("injected floor write failure")
      index.nextFailure = Some(failure)

      val thrown = intercept[IllegalStateException](snapshotHistory.onUtxoSnapshotApplied(height))
      thrown shouldBe failure
      snapshotHistory.readMinimalFullBlockHeight() shouldBe GenesisHeight
      snapshotHistory.utxoSetSnapshotDownloadPlan() shouldBe plan
      snapshotHistory.downloadedChunksIterator().map(c => Algos.encode(c.id)).toVector shouldBe before
      val retryStore = new LDBVersionedStore(createTempDir, initialKeepVersions = 100)
      try {
        val blockId = ModifierId @@ Algos.encode(Array.fill(32)(42.toByte))
        snapshotHistory.createPersistentProver(retryStore, history, height, blockId).isSuccess shouldBe true
      } finally retryStore.close()

      snapshotHistory.onUtxoSnapshotApplied(height)
      snapshotHistory.readMinimalFullBlockHeight() shouldBe height + 1
      snapshotHistory.utxoSetSnapshotDownloadPlan() shouldBe None
      objects.rowCount shouldBe 0
    } finally snapshotHistory.closeStorage()

    val reopened = openHistory()
    try reopened.readMinimalFullBlockHeight() shouldBe height + 1
    finally reopened.closeStorage()
  }

  private class SnapshotStore extends LDBKVStore(null) {
    private val rows = mutable.Map.empty[ByteArrayWrapper, Array[Byte]]
    var nextFailure: Option[Throwable] = None
    def rowCount: Int = rows.size
    override def get(key: Array[Byte]): Option[Array[Byte]] = rows.get(ByteArrayWrapper(key)).map(_.clone())
    override def insert(key: Array[Byte], value: Array[Byte]): Try[Unit] = insert(Array(key), Array(value))
    override def insert(keys: Array[Array[Byte]], values: Array[Array[Byte]]): Try[Unit] = {
      require(keys.length == values.length)
      nextFailure match {
        case Some(error) => nextFailure = None; Failure(error)
        case None =>
          keys.indices.foreach(i => rows.update(ByteArrayWrapper(keys(i).clone()), values(i).clone()))
          Success(())
      }
    }
    override def remove(keys: Array[Array[Byte]]): Try[Unit] = {
      keys.foreach(key => rows.remove(ByteArrayWrapper(key)))
      Success(())
    }
    override def close(): Unit = ()
  }

}
