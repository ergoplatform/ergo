package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.history.storage.modifierprocessors.{
  UtxoSetSnapshotProcessor,
  UtxoSnapshotScanSource,
  UtxoSnapshotScanSourceSerializer
}
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.core.VersionTag
import org.ergoplatform.serialization.{ManifestSerializer, SubtreeSerializer}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.Constants.{DigestType, hashFn}
import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverLeaf, ProverNodes, VersionedLDBAVLStorage}
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, ProxyInternalNode}
import scorex.db.{ByteArrayWrapper, LDBVersionedStore}
import scorex.util.{ByteArrayBuilder, ModifierId}
import scorex.util.serialization.VLQByteBufferWriter

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
    override protected val minimalFullBlockHeightKey: ByteArrayWrapper =
      ByteArrayWrapper(Array.fill(32)(92: Byte))
    override protected def snapshotHeaderStateAtHeight(height: Int): Option[(ModifierId, ADDigest)] = None
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

  private def randomBlockId(): ModifierId =
    ModifierId @@ Algos.encode(Array.fill(32)(Random.nextInt(100).toByte))

  private def freshProcessor(storage: HistoryStorage,
                             snapshotHeight: Int,
                             snapshotBlockId: ModifierId,
                             snapshotDigest: ADDigest): UtxoSetSnapshotProcessor =
    new UtxoSetSnapshotProcessor {
      private var minimalFullBlockHeightVar = GenesisHeight

      override protected val settings: ErgoSettings = s.copy(chainSettings =
        s.chainSettings.copy(voting = s.chainSettings.voting.copy(votingLength = epochLength)))
      override protected val historyStorage: HistoryStorage = storage
      override protected val minimalFullBlockHeightKey: ByteArrayWrapper =
        ByteArrayWrapper(Array.fill(32)(91: Byte))

      override protected def snapshotHeaderStateAtHeight(height: Int): Option[(ModifierId, ADDigest)] =
        if (height == snapshotHeight) Some(snapshotBlockId -> snapshotDigest) else None

      override def readMinimalFullBlockHeight(): Int = minimalFullBlockHeightVar

      override def writeMinimalFullBlockHeight(height: Int): Unit = {
        minimalFullBlockHeightVar = height
      }

      override def onUtxoSnapshotApplied(height: Int, blockId: ModifierId) =
        super.onUtxoSnapshotApplied(height, blockId).map { _ =>
          minimalFullBlockHeightVar = height + 1
        }
    }

  private def normalSnapshotFixture(): (BatchAVLProverManifest[DigestType], Array[Byte], IndexedSeq[Array[Byte]]) = {
    val holder = boxesHolderGenOfSize(32 * 1024).sample.get
    val state = createUtxoState(holder, parameters)
    val snapshotHeight = epochLength - 1
    state.dumpSnapshot(snapshotHeight, state.rootDigest.dropRight(1)).get
    val manifestId = state.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeight)
    val manifestBytes = state.snapshotsDb.readManifestBytes(manifestId).get
    val manifest = ManifestSerializer.defaultSerializer.parseBytes(manifestBytes)
    val chunks = manifest.subtreesIds.map(state.snapshotsDb.readSubtreeBytes(_).get).toIndexedSeq
    (manifest, manifestBytes, chunks)
  }

  private def expectedPartCount(node: ProverNodes[DigestType]): Int = node match {
    case _: ProverLeaf[DigestType] => 1
    case proxy: ProxyInternalNode[DigestType] if proxy.isEmpty => 2
    case internal: InternalProverNode[DigestType] =>
      expectedPartCount(internal.left) + expectedPartCount(internal.right)
  }

  private def finalizeSnapshot(processor: UtxoSetSnapshotProcessor,
                               manifest: BatchAVLProverManifest[DigestType],
                               manifestBytes: Array[Byte],
                               chunks: IndexedSeq[Array[Byte]],
                               snapshotHeight: Int,
                               snapshotBlockId: ModifierId): Unit = {
    processor.registerManifestToDownload(manifest, manifestBytes, snapshotHeight, Seq.empty)
    val requested = processor.getChunkIdsToDownload(manifest.subtreesIds.size)
    requested.zip(chunks).foreach { case (chunkId, bytes) =>
      processor.registerDownloadedChunk(chunkId, bytes).get
    }
    processor.onUtxoSnapshotApplied(snapshotHeight, snapshotBlockId).get
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

    val blockId = randomBlockId()
    utxoSetSnapshotProcessor.registerManifestToDownload(manifest, manifestBytes, snapshotHeight, Seq.empty)
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
    store.update(Array.fill(32)(40: Byte), Nil, Seq.empty).get
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

  property("reject a restored snapshot with an unauthenticated internal routing key without mutating the state store") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    val (subtreeIndex, subtree, internal) = chunks.zipWithIndex.iterator.flatMap {
      case (bytes, index) =>
        val parsed = SubtreeSerializer.parseBytes(bytes)
        parsed.subtreeTop match {
          case node: InternalProverNode[DigestType] => Some((index, parsed, node))
          case _ => None
        }
    }.toSeq.head
    val expectedId = manifest.subtreesIds(subtreeIndex)
    subtree.verify(expectedId) shouldBe true
    val authenticatedLabel = subtree.subtreeTop.label.clone()

    val mutatedKey = internal.key.clone()
    mutatedKey(0) = (mutatedKey(0) ^ 1).toByte
    val mutatedSubtree = subtree.copy(internal.getNewKey(ADKey @@ mutatedKey))
    mutatedSubtree.subtreeTop.label.sameElements(authenticatedLabel) shouldBe true
    val mutatedBytes = SubtreeSerializer.toBytes(mutatedSubtree)
    val reparsed = SubtreeSerializer.parseBytes(mutatedBytes)
    reparsed.verify(expectedId) shouldBe true

    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    processor.registerManifestToDownload(manifest, manifestBytes, snapshotHeight, Seq.empty)
    val mutatedChunks = chunks.updated(subtreeIndex, mutatedBytes)
    val requestedChunkIds = processor.getChunkIdsToDownload(manifest.subtreesIds.size)
    requestedChunkIds.zip(mutatedChunks).foreach { case (chunkId, bytes) =>
      processor.registerDownloadedChunk(chunkId, bytes).get
    }

    val stateStore = new LDBVersionedStore(createTempDir, initialKeepVersions = 100)
    val retainedVersion = Array.fill(32)(41: Byte)
    val retainedKey = Array.fill(32)(42: Byte)
    val retainedValue = Array[Byte](43, 44, 45)
    stateStore.update(retainedVersion, Nil, Seq(retainedKey -> retainedValue)).get
    val rowsBefore = stateStore.getAll
      .map { case (key, value) => key.toIndexedSeq -> value.toIndexedSeq }
      .toSet
    val rollbackVersionsBefore = stateStore.rollbackVersions()
      .map(_.toIndexedSeq)
      .toIndexedSeq

    processor.createPersistentProver(
      stateStore, history, snapshotHeight, snapshotBlockId).isFailure shouldBe true

    stateStore.lastVersionID.get shouldBe retainedVersion
    stateStore.rollbackVersions().map(_.toIndexedSeq).toIndexedSeq shouldBe rollbackVersionsBefore
    stateStore.getAll
      .map { case (key, value) => key.toIndexedSeq -> value.toIndexedSeq }
      .toSet shouldBe rowsBefore
  }

  property("restore the previous state store when prover restoration fails after snapshot write") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    processor.registerManifestToDownload(manifest, manifestBytes, snapshotHeight, Seq.empty)
    processor.getChunkIdsToDownload(manifest.subtreesIds.size)
      .zip(chunks).foreach { case (chunkId, bytes) =>
        processor.registerDownloadedChunk(chunkId, bytes).get
      }

    val stateStore = new LDBVersionedStore(createTempDir, initialKeepVersions = 100) {
      override def get(key: Array[Byte]): Option[Array[Byte]] =
        if (lastVersionID.exists(_.length == 33)) None else super.get(key)
    }
    val retainedVersion = Array.fill(32)(51: Byte)
    val retainedKey = Array.fill(32)(52: Byte)
    val retainedValue = Array[Byte](53, 54, 55)
    stateStore.update(retainedVersion, Nil, Seq(retainedKey -> retainedValue)).get
    val rowsBefore = stateStore.getAll
      .map { case (key, value) => key.toIndexedSeq -> value.toIndexedSeq }
      .toSet
    val rollbackVersionsBefore = stateStore.rollbackVersions()
      .map(_.toIndexedSeq)
      .toIndexedSeq

    processor.createPersistentProver(
      stateStore, history, snapshotHeight, snapshotBlockId).isFailure shouldBe true

    stateStore.lastVersionID.get shouldBe retainedVersion
    stateStore.rollbackVersions().map(_.toIndexedSeq).toIndexedSeq shouldBe rollbackVersionsBefore
    stateStore.getAll
      .map { case (key, value) => key.toIndexedSeq -> value.toIndexedSeq }
      .toSet shouldBe rowsBefore
  }

  property("one-leaf manifest yields one readable embedded scan part") {
    val leaf = new ProverLeaf[DigestType](
      ADKey @@ Array.fill(32)(1: Byte),
      ADValue @@ Array[Byte](1, 2, 3),
      ADKey @@ Array.fill(32)(2: Byte)
    )(hashFn)
    val manifest = new BatchAVLProverManifest[DigestType](leaf, 1)
    manifest.subtreesIds shouldBe empty
    val manifestBytes = ManifestSerializer.defaultSerializer.toBytes(manifest)
    val source = UtxoSnapshotScanSource
      .create(epochLength - 1, randomBlockId(), ManifestSerializer.MainnetManifestDepth, manifestBytes)
      .get
    val reparsed = UtxoSnapshotScanSourceSerializer
      .parseBytesTry(UtxoSnapshotScanSourceSerializer.toBytes(source))
      .get

    reparsed.partCount shouldBe 1
    val part = reparsed.readPart(0, _ => throw new AssertionError("embedded part read a chunk"))
    part.isSuccess shouldBe true
    part.get.subtreeTop.label.sameElements(leaf.label) shouldBe true
  }

  property("reject trailing bytes inside an exact snapshot manifest payload") {
    val (manifest, manifestBytes, _) = normalSnapshotFixture()
    val paddedManifestBytes = manifestBytes :+ 0.toByte
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()

    UtxoSnapshotScanSource
      .create(snapshotHeight, snapshotBlockId,
        ManifestSerializer.MainnetManifestDepth, paddedManifestBytes)
      .isFailure shouldBe true

    val processor = freshProcessor(
      HistoryStorage(s),
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    an[IllegalArgumentException] should be thrownBy processor.registerManifestToDownload(
      manifest, paddedManifestBytes, snapshotHeight, Seq.empty)
    processor.utxoSetSnapshotDownloadPlan() shouldBe None

    val descriptorWriter = new VLQByteBufferWriter(new ByteArrayBuilder())
    descriptorWriter.put(1: Byte)
    descriptorWriter.putInt(snapshotHeight)
    descriptorWriter.putBytes(Array.fill(32)(1: Byte))
    descriptorWriter.put(ManifestSerializer.MainnetManifestDepth)
    descriptorWriter.putUInt(paddedManifestBytes.length.toLong)
    descriptorWriter.putBytes(paddedManifestBytes)
    UtxoSnapshotScanSourceSerializer
      .parseBytesTry(descriptorWriter.result().toBytes)
      .isFailure shouldBe true
  }

  property("scan source serializer rejects wrong version, overflowing or oversized manifest, and trailing bytes") {
    val wrongVersion = Array(2: Byte)
    UtxoSnapshotScanSourceSerializer.parseBytesTry(wrongVersion).isFailure shouldBe true

    val oversizedWriter = new VLQByteBufferWriter(new ByteArrayBuilder())
    oversizedWriter.put(1: Byte)
    oversizedWriter.putInt(epochLength - 1)
    oversizedWriter.putBytes(Array.fill(32)(1: Byte))
    oversizedWriter.put(ManifestSerializer.MainnetManifestDepth)
    oversizedWriter.putUInt(4000001L)
    UtxoSnapshotScanSourceSerializer
      .parseBytesTry(oversizedWriter.result().toBytes)
      .failed.get.getMessage should include("out of bounds")

    val overflowingWriter = new VLQByteBufferWriter(new ByteArrayBuilder())
    overflowingWriter.put(1: Byte)
    overflowingWriter.putInt(epochLength - 1)
    overflowingWriter.putBytes(Array.fill(32)(1: Byte))
    overflowingWriter.put(ManifestSerializer.MainnetManifestDepth)
    overflowingWriter.putUInt(Int.MaxValue.toLong + 1L)
    UtxoSnapshotScanSourceSerializer
      .parseBytesTry(overflowingWriter.result().toBytes)
      .failed.get.getMessage should include("unsigned 31-bit int range")

    val leaf = new ProverLeaf[DigestType](
      ADKey @@ Array.fill(32)(3: Byte),
      ADValue @@ Array[Byte](4),
      ADKey @@ Array.fill(32)(5: Byte)
    )(hashFn)
    val manifestBytes = ManifestSerializer.defaultSerializer.toBytes(
      new BatchAVLProverManifest[DigestType](leaf, 1))
    val source = UtxoSnapshotScanSource
      .create(epochLength - 1, randomBlockId(), ManifestSerializer.MainnetManifestDepth, manifestBytes)
      .get
    val trailing = UtxoSnapshotScanSourceSerializer.toBytes(source) :+ 0.toByte
    UtxoSnapshotScanSourceSerializer.parseBytesTry(trailing).isFailure shouldBe true
  }

  property("persisted source and chunk bytes survive successful finalization") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )

    finalizeSnapshot(processor, manifest, manifestBytes, chunks, snapshotHeight, snapshotBlockId)

    processor.utxoSetSnapshotDownloadPlan() shouldBe None
    val source = processor.readUtxoSnapshotScanSource(snapshotBlockId).get
    val availableSource = processor.readUtxoSnapshotScanSource().get
    availableSource.snapshotHeight shouldBe source.snapshotHeight
    availableSource.snapshotBlockId shouldBe source.snapshotBlockId
    availableSource.manifestDepth shouldBe source.manifestDepth
    availableSource.partCount shouldBe source.partCount
    availableSource.manifestBytes shouldBe source.manifestBytes
    source.manifestBytes shouldBe manifestBytes
    source.partCount shouldBe expectedPartCount(manifest.root)
    processor.readUtxoSnapshotScanPart(source, 0).isSuccess shouldBe true
    storage.get(UtxoSetSnapshotProcessor.snapshotScanChunkKey(0)).get shouldBe chunks.head
  }

  property("missing retained chunk bytes fail scan part read") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    finalizeSnapshot(processor, manifest, manifestBytes, chunks, snapshotHeight, snapshotBlockId)
    storage.removeRawObjects(Array(UtxoSetSnapshotProcessor.snapshotScanChunkKey(0))).get

    val source = processor.readUtxoSnapshotScanSource(snapshotBlockId).get
    (0 until source.partCount).exists { index =>
      processor.readUtxoSnapshotScanPart(source, index).isFailure
    } shouldBe true
  }

  property("retained chunk root must match its manifest chunk id") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    chunks.size should be > 1
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    finalizeSnapshot(processor, manifest, manifestBytes, chunks, snapshotHeight, snapshotBlockId)
    storage.insert(UtxoSetSnapshotProcessor.snapshotScanChunkKey(0), chunks(1)).get

    val source = processor.readUtxoSnapshotScanSource(snapshotBlockId).get
    (0 until source.partCount).exists { index =>
      processor.readUtxoSnapshotScanPart(source, index).isFailure
    } shouldBe true
  }

  property("scan source cleanup removes descriptor and ordinal chunks idempotently") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    finalizeSnapshot(processor, manifest, manifestBytes, chunks, snapshotHeight, snapshotBlockId)

    processor.removeUtxoSnapshotScanSource(snapshotBlockId).isSuccess shouldBe true
    processor.readUtxoSnapshotScanSource(snapshotBlockId).isFailure shouldBe true
    manifest.subtreesIds.indices.foreach { ordinal =>
      storage.get(UtxoSetSnapshotProcessor.snapshotScanChunkKey(ordinal)) shouldBe None
    }
    processor.removeUtxoSnapshotScanSource(snapshotBlockId).isSuccess shouldBe true
  }

  property("scan source cleanup preserves a different stable source byte-for-byte") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    val snapshotHeight = epochLength - 1
    val stableBlockId = ModifierId @@ Algos.encode(Array.fill(32)(81: Byte))
    val obsoleteBlockId = ModifierId @@ Algos.encode(Array.fill(32)(82: Byte))
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      stableBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    finalizeSnapshot(processor, manifest, manifestBytes, chunks, snapshotHeight, stableBlockId)

    val retainedDescriptor = storage
      .getIndex(UtxoSetSnapshotProcessor.SnapshotScanSourceKey).get.clone()
    val retainedChunks = chunks.indices.map { ordinal =>
      storage.get(UtxoSetSnapshotProcessor.snapshotScanChunkKey(ordinal)).get.clone()
    }

    processor.removeUtxoSnapshotScanSource(obsoleteBlockId).isFailure shouldBe true
    storage.getIndex(UtxoSetSnapshotProcessor.SnapshotScanSourceKey).get shouldBe retainedDescriptor
    retainedChunks.indices.foreach { ordinal =>
      storage.get(UtxoSetSnapshotProcessor.snapshotScanChunkKey(ordinal)).get shouldBe retainedChunks(ordinal)
    }
    processor.readUtxoSnapshotScanSource(stableBlockId).isSuccess shouldBe true
  }

  property("an applied scan source rejects a replacement manifest without modifying retained bytes") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    val (replacementManifest, replacementManifestBytes, _) = normalSnapshotFixture()
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = ModifierId @@ Algos.encode(Array.fill(32)(83: Byte))
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    finalizeSnapshot(processor, manifest, manifestBytes, chunks, snapshotHeight, snapshotBlockId)
    val retainedDescriptor = storage
      .getIndex(UtxoSetSnapshotProcessor.SnapshotScanSourceKey).get.clone()
    val retainedChunks = chunks.indices.map { ordinal =>
      storage.get(UtxoSetSnapshotProcessor.snapshotScanChunkKey(ordinal)).get.clone()
    }

    scala.util.Try(processor.registerManifestToDownload(
      replacementManifest,
      replacementManifestBytes,
      snapshotHeight + epochLength,
      Seq.empty
    )).isFailure shouldBe true

    processor.utxoSetSnapshotDownloadPlan() shouldBe None
    storage.getIndex(UtxoSetSnapshotProcessor.SnapshotScanSourceKey).get shouldBe retainedDescriptor
    retainedChunks.indices.foreach { ordinal =>
      storage.get(UtxoSetSnapshotProcessor.snapshotScanChunkKey(ordinal)).get shouldBe retainedChunks(ordinal)
    }
    processor.readUtxoSnapshotScanSource(snapshotBlockId).isSuccess shouldBe true
  }

  property("a completed download plan cannot be replaced while it is being finalized") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    val (replacementManifest, replacementManifestBytes, _) = normalSnapshotFixture()
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = ModifierId @@ Algos.encode(Array.fill(32)(84: Byte))
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    processor.registerManifestToDownload(
      manifest, manifestBytes, snapshotHeight, Seq.empty)
    processor.getChunkIdsToDownload(manifest.subtreesIds.size)
      .zip(chunks).foreach { case (chunkId, bytes) =>
        processor.registerDownloadedChunk(chunkId, bytes).get
      }
    val planBefore = processor.utxoSetSnapshotDownloadPlan().get
    planBefore.fullyDownloaded shouldBe true

    scala.util.Try(processor.registerManifestToDownload(
      replacementManifest,
      replacementManifestBytes,
      snapshotHeight + epochLength,
      Seq.empty
    )).isFailure shouldBe true

    val planAfter = processor.utxoSetSnapshotDownloadPlan().get
    planAfter.id shouldBe planBefore.id
    planAfter.expectedChunkIds.zip(planBefore.expectedChunkIds).foreach { case (after, before) =>
      after shouldBe before
    }
    processor.onUtxoSnapshotApplied(snapshotHeight, snapshotBlockId).isSuccess shouldBe true
    val source = processor.readUtxoSnapshotScanSource(snapshotBlockId).get
    source.partCount should be > 0
    (0 until source.partCount).foreach { index =>
      processor.readUtxoSnapshotScanPart(source, index).isSuccess shouldBe true
    }
  }

  property("chunk ingestion rejects trailing bytes without persisting or advancing the plan") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    chunks should not be empty
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    processor.registerManifestToDownload(manifest, manifestBytes, snapshotHeight, Seq.empty)
    val requested = processor.getChunkIdsToDownload(1)
    val chunkRowBefore = storage
      .get(UtxoSetSnapshotProcessor.snapshotScanChunkKey(0)).map(_.toIndexedSeq)

    val result = processor.registerDownloadedChunk(requested.head, chunks.head :+ 0.toByte)

    result.isFailure shouldBe true
    result.failed.get.getMessage.toLowerCase should include("trailing")
    processor.utxoSetSnapshotDownloadPlan().get.downloadedChunkIds.head shouldBe false
    storage.get(UtxoSetSnapshotProcessor.snapshotScanChunkKey(0)).map(_.toIndexedSeq) shouldBe chunkRowBefore
  }

  property("snapshot finalization rejects trailing bytes in a downloaded chunk row") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    chunks should not be empty
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    processor.registerManifestToDownload(manifest, manifestBytes, snapshotHeight, Seq.empty)
    processor.getChunkIdsToDownload(manifest.subtreesIds.size)
      .zip(chunks).foreach { case (chunkId, bytes) =>
        processor.registerDownloadedChunk(chunkId, bytes).get
      }
    storage.insert(
      UtxoSetSnapshotProcessor.snapshotScanChunkKey(0),
      chunks.head :+ 0.toByte
    ).get
    val descriptorBefore = storage
      .getIndex(UtxoSetSnapshotProcessor.SnapshotScanSourceKey).map(_.toIndexedSeq)

    val result = processor.onUtxoSnapshotApplied(snapshotHeight, snapshotBlockId)

    result.isFailure shouldBe true
    result.failed.get.getMessage.toLowerCase should include("trailing")
    processor.utxoSetSnapshotDownloadPlan().isDefined shouldBe true
    storage.getIndex(UtxoSetSnapshotProcessor.SnapshotScanSourceKey).map(_.toIndexedSeq) shouldBe descriptorBefore
  }

  property("snapshot scan part read rejects trailing bytes in a retained chunk row") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    chunks should not be empty
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    finalizeSnapshot(processor, manifest, manifestBytes, chunks, snapshotHeight, snapshotBlockId)
    storage.insert(
      UtxoSetSnapshotProcessor.snapshotScanChunkKey(0),
      chunks.head :+ 0.toByte
    ).get
    val source = processor.readUtxoSnapshotScanSource(snapshotBlockId).get

    val failures = (0 until source.partCount).flatMap { index =>
      processor.readUtxoSnapshotScanPart(source, index).failed.toOption
    }

    failures should not be empty
    failures.exists(_.getMessage.toLowerCase.contains("trailing")) shouldBe true
  }

  property("failed chunk persistence does not advance the in-memory download plan") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    processor.registerManifestToDownload(manifest, manifestBytes, snapshotHeight, Seq.empty)
    val requested = processor.getChunkIdsToDownload(1)
    storage.close()

    processor.registerDownloadedChunk(requested.head, chunks.head).isFailure shouldBe true
    processor.utxoSetSnapshotDownloadPlan().get.downloadedChunkIds.head shouldBe false
  }

  property("invalidating a stale snapshot removes retained ordinal chunks and the in-memory plan") {
    val (manifest, manifestBytes, chunks) = normalSnapshotFixture()
    val snapshotHeight = epochLength - 1
    val snapshotBlockId = randomBlockId()
    val storage = HistoryStorage(s)
    val processor = freshProcessor(
      storage,
      snapshotHeight,
      snapshotBlockId,
      VersionedLDBAVLStorage.digest(manifest.id, manifest.rootHeight)
    )
    processor.registerManifestToDownload(manifest, manifestBytes, snapshotHeight, Seq.empty)
    val requested = processor.getChunkIdsToDownload(1)
    processor.registerDownloadedChunk(requested.head, chunks.head).get
    storage.get(UtxoSetSnapshotProcessor.snapshotScanChunkKey(0)).isDefined shouldBe true

    processor.invalidateUtxoSetSnapshotDownload().isSuccess shouldBe true

    processor.utxoSetSnapshotDownloadPlan() shouldBe None
    storage.get(UtxoSetSnapshotProcessor.snapshotScanChunkKey(0)) shouldBe None
  }

}
