package org.ergoplatform.nodeView.wallet

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import org.ergoplatform.nodeView.history.storage.modifierprocessors.{UtxoSnapshotScanSource, UtxoSnapshotScanSourceReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.{UtxoSnapshotChunkIntegrityException, UtxoSnapshotScanStatus}
import org.ergoplatform.serialization.ManifestSerializer
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.crypto.authds.avltree.batch.Constants.{DigestType, hashFn}
import scorex.crypto.authds.avltree.batch.ProverLeaf
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSubtree}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.util.ModifierId

import java.util.UUID
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class UtxoSnapshotWalletScannerSpec extends ErgoCorePropertyTest {

  private val ScanDefinition = UtxoSnapshotScanDefinition(
    UtxoSnapshotScanDefinition.WalletScanSemanticsVersion,
    ByteString(Array.fill(UtxoSnapshotScanDefinition.DigestLength)(0x11.toByte)))

  private def snapshotStatus(snapshotHeight: Int,
                             snapshotBlockId: ModifierId,
                             manifestDepth: Int,
                             nextSubtreeIndex: Int,
                             totalSubtrees: Int,
                             completed: Boolean): UtxoSnapshotScanStatus =
    new UtxoSnapshotScanStatus(
      snapshotHeight, snapshotBlockId, manifestDepth, nextSubtreeIndex,
      totalSubtrees, completed, ScanDefinition)

  property("UTXO snapshot collectBoxes fails on malformed box bytes") {
    val leaf = new ProverLeaf[DigestType](
      ADKey @@ Array.fill(32)(1: Byte),
      ADValue @@ Array[Byte](1, 2, 3),
      ADKey @@ Array.fill(32)(2: Byte)
    )(hashFn)
    val subtree = new BatchAVLProverSubtree[DigestType](leaf)

    UtxoSnapshotWalletScanner.collectBoxes(subtree).isFailure shouldBe true
  }

  property("UTXO snapshot collectBoxes skips only the AVL negative-infinity sentinel") {
    val sentinel = new ProverLeaf[DigestType](
      ADKey @@ Array.fill(32)(0: Byte),
      ADValue @@ Array.emptyByteArray,
      ADKey @@ Array.fill(32)(1: Byte)
    )(hashFn)
    val emptyUserLeaf = new ProverLeaf[DigestType](
      ADKey @@ Array.fill(32)(1: Byte),
      ADValue @@ Array.emptyByteArray,
      ADKey @@ Array.fill(32)(2: Byte)
    )(hashFn)

    UtxoSnapshotWalletScanner
      .collectBoxes(new BatchAVLProverSubtree[DigestType](sentinel)).get shouldBe empty
    UtxoSnapshotWalletScanner
      .collectBoxes(new BatchAVLProverSubtree[DigestType](emptyUserLeaf)).isFailure shouldBe true
  }

  property("UTXO snapshot batches resume from the durable cursor and read immutable source parts") {
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(11: Byte))
    val box = org.ergoplatform.utils.ErgoNodeTestConstants.genesisEmissionBox
    val first = subtree(ErgoBoxSerializer.toBytes(box), 1)
    val second = subtree(ErgoBoxSerializer.toBytes(box), 2)
    val source = scanSource(blockId, 100)
    val reader = new RecordingSourceReader(source, IndexedSeq(first, second))
    val status = snapshotStatus(100, blockId, source.manifestDepth, 1, 2, completed = false)

    val batch = UtxoSnapshotWalletScanner.readSnapshotBatch(reader, source, status).get

    reader.readIndexes.toSeq shouldBe Seq(1)
    batch.subtreeIndex shouldBe 1
    batch.nextSubtreeIndex shouldBe 2
    batch.completed shouldBe true
    batch.boxes shouldBe IndexedSeq(box)
  }

  property("UTXO snapshot batches read exactly one bounded span") {
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(21: Byte))
    val box = org.ergoplatform.utils.ErgoNodeTestConstants.genesisEmissionBox
    val part = subtree(ErgoBoxSerializer.toBytes(box), 3)
    val source = scanSource(blockId, 104)
    val parts = IndexedSeq.fill(40)(part)
    val reader = new RecordingSourceReader(source, parts)
    val status = snapshotStatus(104, blockId, source.manifestDepth, 3, 40, completed = false)

    val batch = UtxoSnapshotWalletScanner.readSnapshotBatch(reader, source, status).get

    reader.readIndexes.toSeq shouldBe (3 until 35)
    batch.subtreeIndex shouldBe 3
    batch.nextSubtreeIndex shouldBe 35
    batch.completed shouldBe false
    batch.boxes.size shouldBe 32
  }

  property("UTXO snapshot restart derives only the last committed batch") {
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(23: Byte))
    val totalSubtrees = UtxoSnapshotWalletScanner.SnapshotScanBatchSize * 3 + 1

    UtxoSnapshotWalletScanner.lastCommittedBatchStatus(
      snapshotStatus(106, blockId, 14, 0, totalSubtrees, completed = false)).get shouldBe None

    val firstCheckpoint = UtxoSnapshotWalletScanner.lastCommittedBatchStatus(
      snapshotStatus(106, blockId, 14,
        UtxoSnapshotWalletScanner.SnapshotScanBatchSize,
        totalSubtrees,
        completed = false)).get.get
    firstCheckpoint.nextSubtreeIndex shouldBe 0

    val secondCheckpoint = UtxoSnapshotWalletScanner.lastCommittedBatchStatus(
      snapshotStatus(106, blockId, 14,
        UtxoSnapshotWalletScanner.SnapshotScanBatchSize * 2,
        totalSubtrees,
        completed = false)).get.get
    secondCheckpoint.nextSubtreeIndex shouldBe UtxoSnapshotWalletScanner.SnapshotScanBatchSize

    UtxoSnapshotWalletScanner.lastCommittedBatchStatus(
      snapshotStatus(106, blockId, 14,
        UtxoSnapshotWalletScanner.SnapshotScanBatchSize + 1,
        totalSubtrees,
        completed = false)).isFailure shouldBe true
  }

  property("UTXO snapshot batch bounds do not overflow Int cursors") {
    UtxoSnapshotWalletScanner.nextBatchCursor(
      Int.MaxValue,
      Int.MaxValue - 1) shouldBe Int.MaxValue
  }

  property("UTXO snapshot scanner validates one prior batch before applying the next batch") {
    implicit val system: ActorSystem = ActorSystem("utxo-snapshot-frontier-validation-spec")
    try {
      val wallet = TestProbe()
      val blockId = ModifierId @@ Algos.encode(Array.fill(32)(24: Byte))
      val box = org.ergoplatform.utils.ErgoNodeTestConstants.genesisEmissionBox
      val part = subtree(ErgoBoxSerializer.toBytes(box), 4)
      val source = scanSource(blockId, 107)
      val totalSubtrees = UtxoSnapshotWalletScanner.SnapshotScanBatchSize + 1
      val reader = new RecordingSourceReader(source, IndexedSeq.fill(totalSubtrees)(part))
      val settings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.copy(
        nodeSettings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.nodeSettings.copy(
          utxoSettings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.nodeSettings.utxoSettings
            .copy(utxoBootstrap = true)))
      val scanner = system.actorOf(UtxoSnapshotWalletScanner.props(wallet.ref, settings, reader))
      val run = UtxoSnapshotScanRun(
        UtxoSnapshotRunToken(UUID.randomUUID()), source.snapshotHeight, blockId)
      val status = snapshotStatus(
        source.snapshotHeight,
        blockId,
        source.manifestDepth.toInt,
        UtxoSnapshotWalletScanner.SnapshotScanBatchSize,
        totalSubtrees,
        completed = false)

      scanner ! StartUtxoSnapshotScan(run, forceRestart = false)
      wallet.expectMsgType[GetOrInitUtxoSnapshotScanStatus](5.seconds)
      wallet.reply(Success(status))

      val checkpoint = wallet.expectMsgType[ApplyUtxoSnapshotScanBatch](5.seconds)
      checkpoint.subtreeIndex shouldBe 0
      checkpoint.nextSubtreeIndex shouldBe UtxoSnapshotWalletScanner.SnapshotScanBatchSize
      checkpoint.completed shouldBe false
      checkpoint.boxes.size shouldBe UtxoSnapshotWalletScanner.SnapshotScanBatchSize
      reader.readIndexes.toSeq shouldBe (0 until UtxoSnapshotWalletScanner.SnapshotScanBatchSize)
      wallet.reply(Success(status))

      val next = wallet.expectMsgType[ApplyUtxoSnapshotScanBatch](5.seconds)
      next.subtreeIndex shouldBe UtxoSnapshotWalletScanner.SnapshotScanBatchSize
      next.nextSubtreeIndex shouldBe totalSubtrees
      next.completed shouldBe true
      next.boxes shouldBe IndexedSeq(box)
      reader.readIndexes.toSeq shouldBe (0 until totalSubtrees)
      wallet.reply(Success(status.copy(nextSubtreeIndex = totalSubtrees, completed = true)))
    } finally {
      system.terminate()
    }
  }

  property("UTXO snapshot scanner does not apply a new batch after frontier integrity fails") {
    implicit val system: ActorSystem = ActorSystem("utxo-snapshot-frontier-integrity-failure-spec")
    try {
      val wallet = TestProbe()
      val blockId = ModifierId @@ Algos.encode(Array.fill(32)(25: Byte))
      val box = org.ergoplatform.utils.ErgoNodeTestConstants.genesisEmissionBox
      val part = subtree(ErgoBoxSerializer.toBytes(box), 5)
      val source = scanSource(blockId, 108)
      val totalSubtrees = UtxoSnapshotWalletScanner.SnapshotScanBatchSize + 1
      val reader = new RecordingSourceReader(source, IndexedSeq.fill(totalSubtrees)(part))
      val settings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.copy(
        nodeSettings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.nodeSettings.copy(
          utxoSettings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.nodeSettings.utxoSettings
            .copy(utxoBootstrap = true)))
      val scanner = system.actorOf(UtxoSnapshotWalletScanner.props(wallet.ref, settings, reader))
      val run = UtxoSnapshotScanRun(
        UtxoSnapshotRunToken(UUID.randomUUID()), source.snapshotHeight, blockId)
      val status = snapshotStatus(
        source.snapshotHeight,
        blockId,
        source.manifestDepth.toInt,
        UtxoSnapshotWalletScanner.SnapshotScanBatchSize,
        totalSubtrees,
        completed = false)

      scanner ! StartUtxoSnapshotScan(run, forceRestart = false)
      wallet.expectMsgType[GetOrInitUtxoSnapshotScanStatus](5.seconds)
      wallet.reply(Success(status))
      wallet.expectMsgType[ApplyUtxoSnapshotScanBatch](5.seconds)
      wallet.reply(Failure(new UtxoSnapshotChunkIntegrityException("injected marker mismatch")))

      wallet.expectNoMessage(1500.millis)
      reader.readIndexes.toSeq shouldBe (0 until UtxoSnapshotWalletScanner.SnapshotScanBatchSize)
    } finally {
      system.terminate()
    }
  }

  property("UTXO snapshot scanner exhausts its retry budget and will not hot-loop on duplicate starts") {
    implicit val system: ActorSystem = ActorSystem("utxo-snapshot-terminal-retry-spec")
    try {
      val wallet = TestProbe()
      val blockId = ModifierId @@ Algos.encode(Array.fill(32)(12: Byte))
      val source = scanSource(blockId, 101)
      val reads = new AtomicInteger(0)
      val reader = new UtxoSnapshotScanSourceReader {
        override def readUtxoSnapshotScanSource(): Try[UtxoSnapshotScanSource] =
          Failure(new IllegalStateException("source unavailable"))
        override def readUtxoSnapshotScanSource(expectedBlockId: ModifierId): Try[UtxoSnapshotScanSource] = {
          reads.incrementAndGet()
          Failure(new IllegalStateException("source unavailable"))
        }
        override def readUtxoSnapshotScanPart(source: UtxoSnapshotScanSource,
                                              index: Int): Try[BatchAVLProverSubtree[DigestType]] =
          Failure(new IllegalStateException("not reached"))
      }
      val settings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.copy(
        nodeSettings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.nodeSettings.copy(
          utxoSettings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.nodeSettings.utxoSettings
            .copy(utxoBootstrap = true)))
      val scanner = system.actorOf(UtxoSnapshotWalletScanner.props(wallet.ref, settings, reader))

      val firstRun = UtxoSnapshotScanRun(
        UtxoSnapshotRunToken(UUID.randomUUID()), source.snapshotHeight, blockId)
      val duplicateRun = firstRun.copy(token = UtxoSnapshotRunToken(UUID.randomUUID()))

      scanner ! StartUtxoSnapshotScan(firstRun, forceRestart = false)
      scanner ! StartUtxoSnapshotScan(duplicateRun, forceRestart = false)
      val terminal = wallet.expectMsgType[UtxoSnapshotScanTerminated](6.seconds)
      terminal.run shouldBe firstRun
      reads.get() shouldBe 4

      scanner ! StartUtxoSnapshotScan(duplicateRun, forceRestart = false)
      wallet.expectNoMessage(1500.millis)
      reads.get() shouldBe 4
    } finally {
      system.terminate()
    }
  }

  property("forced replacement owns callbacks and ignores a stale abort") {
    implicit val system: ActorSystem = ActorSystem("utxo-snapshot-forced-replacement-spec")
    val firstReadStarted = new CountDownLatch(1)
    val releaseFirstRead = new CountDownLatch(1)
    try {
      val wallet = TestProbe()
      val blockId = ModifierId @@ Algos.encode(Array.fill(32)(22: Byte))
      val source = scanSource(blockId, 105)
      val reads = new AtomicInteger(0)
      val reader = new UtxoSnapshotScanSourceReader {
        override def readUtxoSnapshotScanSource(): Try[UtxoSnapshotScanSource] =
          Failure(new IllegalStateException("source unavailable"))
        override def readUtxoSnapshotScanSource(expectedBlockId: ModifierId): Try[UtxoSnapshotScanSource] = {
          if (reads.incrementAndGet() == 1) {
            firstReadStarted.countDown()
            releaseFirstRead.await(5, TimeUnit.SECONDS)
          }
          Failure(new IllegalStateException("source unavailable"))
        }
        override def readUtxoSnapshotScanPart(source: UtxoSnapshotScanSource,
                                              index: Int): Try[BatchAVLProverSubtree[DigestType]] =
          Failure(new IllegalStateException("not reached"))
      }
      val settings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.copy(
        nodeSettings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.nodeSettings.copy(
          utxoSettings = org.ergoplatform.utils.ErgoNodeTestConstants.settings.nodeSettings.utxoSettings
            .copy(utxoBootstrap = true)))
      val scanner = system.actorOf(UtxoSnapshotWalletScanner.props(wallet.ref, settings, reader))
      val firstRun = UtxoSnapshotScanRun(
        UtxoSnapshotRunToken(UUID.randomUUID()), source.snapshotHeight, blockId)
      val replacementRun = firstRun.copy(token = UtxoSnapshotRunToken(UUID.randomUUID()))

      scanner ! StartUtxoSnapshotScan(firstRun, forceRestart = false)
      firstReadStarted.await(5, TimeUnit.SECONDS) shouldBe true
      scanner ! StartUtxoSnapshotScan(replacementRun, forceRestart = true)
      scanner ! AbortUtxoSnapshotScan(firstRun)
      releaseFirstRead.countDown()

      wallet.expectMsgType[UtxoSnapshotScanTerminated](6.seconds).run shouldBe replacementRun
      wallet.expectNoMessage(300.millis)
      reads.get() shouldBe 5
    } finally {
      releaseFirstRead.countDown()
      system.terminate()
    }
  }

  property("UTXO snapshot finalization schedules catch-up once and retries cleanup until success") {
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(13: Byte))
    val status = snapshotStatus(102, blockId, 14, 2, 2, completed = true)
    val initial = UtxoSnapshotFinalizationState.empty

    val blocked = initial.plan(status, catchUpReady = false)
    blocked.scheduleCatchUp shouldBe false
    blocked.tryCleanup shouldBe false

    val first = blocked.state.plan(status, catchUpReady = true)
    first.scheduleCatchUp shouldBe true
    first.tryCleanup shouldBe true

    val afterFailedCleanup = first.state.plan(status, catchUpReady = true)
    afterFailedCleanup.scheduleCatchUp shouldBe false
    afterFailedCleanup.tryCleanup shouldBe true

    val (cleanupClaimed, shouldStartCleanup) = afterFailedCleanup.state.claimSourceCleanup(blockId)
    shouldStartCleanup shouldBe true
    cleanupClaimed.isSourceCleanupStarted(blockId) shouldBe true

    val (duplicateClaim, shouldStartDuplicateCleanup) = cleanupClaimed.claimSourceCleanup(blockId)
    duplicateClaim shouldBe cleanupClaimed
    shouldStartDuplicateCleanup shouldBe false

    val cleanupCompleted = duplicateClaim.cleanupSucceeded(blockId)
    cleanupCompleted.isSourceCleanupStarted(blockId) shouldBe false
    cleanupCompleted.claimSourceCleanup(blockId)._2 shouldBe false

    val afterSuccess = cleanupCompleted
      .plan(status, catchUpReady = true)
    afterSuccess.scheduleCatchUp shouldBe false
    afterSuccess.tryCleanup shouldBe false

    val invalidated = cleanupClaimed.invalidate(blockId)
    invalidated.isSourceCleanupStarted(blockId) shouldBe false
    invalidated.claimSourceCleanup(blockId)._2 shouldBe true

    val afterCatchUpFailure = first.state.catchUpFailed(blockId)
      .plan(status, catchUpReady = true)
    afterCatchUpFailure.scheduleCatchUp shouldBe true

    val afterCatchUpCompletion = first.state.catchUpCompleted(blockId)
      .plan(status, catchUpReady = true)
    afterCatchUpCompletion.scheduleCatchUp shouldBe true
  }

  property("UTXO snapshot applied events start only an eligible empty wallet or matching pending scan") {
    val blockId = ModifierId @@ Algos.encode(Array.fill(32)(14: Byte))
    val otherBlockId = ModifierId @@ Algos.encode(Array.fill(32)(15: Byte))
    val pending = snapshotStatus(103, blockId, 14, 0, 2, completed = false)

    UtxoSnapshotScanStartPolicy.shouldStartApplied(
      103, blockId, registryPristine = true, rescanInProgress = false, None) shouldBe true
    UtxoSnapshotScanStartPolicy.shouldStartApplied(
      103, blockId, registryPristine = false, rescanInProgress = false, None) shouldBe false
    UtxoSnapshotScanStartPolicy.shouldStartApplied(
      103, blockId, registryPristine = true, rescanInProgress = true, None) shouldBe false
    UtxoSnapshotScanStartPolicy.shouldStartApplied(
      103, blockId, registryPristine = false, rescanInProgress = true, Some(pending)) shouldBe false
    UtxoSnapshotScanStartPolicy.shouldStartApplied(
      103, blockId, registryPristine = false, rescanInProgress = false, Some(pending)) shouldBe true
    UtxoSnapshotScanStartPolicy.shouldStartApplied(
      103, otherBlockId, registryPristine = false, rescanInProgress = false, Some(pending)) shouldBe false
    UtxoSnapshotScanStartPolicy.shouldStartApplied(
      103, blockId, registryPristine = false, rescanInProgress = false,
      Some(pending.copy(nextSubtreeIndex = 2, completed = true))) shouldBe false
  }

  property("UTXO snapshot finalization retains only the latest deferred block notification") {
    val notifications = Seq(
      100 -> "first",
      101 -> "tip-on-old-branch",
      100 -> "stale",
      101 -> "tip-on-current-branch"
    )

    val retained = notifications.foldLeft(Option.empty[(Int, String)]) {
      ErgoWalletActor.latestDeferredSnapshotValue
    }

    retained shouldBe Some(101 -> "tip-on-current-branch")
  }

  property("wallet catch-up classifies only heights below the retained horizon as pruned") {
    ErgoWalletActor.isWalletCatchUpBlockDefinitelyPruned(
      fullBlocksPruned = false, minFullBlockAvailable = 200, requestedHeight = 100) shouldBe false
    ErgoWalletActor.isWalletCatchUpBlockDefinitelyPruned(
      fullBlocksPruned = true, minFullBlockAvailable = 100, requestedHeight = 100) shouldBe false
    ErgoWalletActor.isWalletCatchUpBlockDefinitelyPruned(
      fullBlocksPruned = true, minFullBlockAvailable = 101, requestedHeight = 100) shouldBe true
  }

  private def subtree(value: Array[Byte], keyByte: Byte): BatchAVLProverSubtree[DigestType] = {
    val leaf = new ProverLeaf[DigestType](
      ADKey @@ Array.fill(32)(keyByte),
      ADValue @@ value,
      ADKey @@ Array.fill(32)((keyByte + 1).toByte)
    )(hashFn)
    new BatchAVLProverSubtree[DigestType](leaf)
  }

  private def scanSource(blockId: ModifierId, height: Int): UtxoSnapshotScanSource = {
    val leaf = new ProverLeaf[DigestType](
      ADKey @@ Array.fill(32)(31: Byte),
      ADValue @@ Array[Byte](1),
      ADKey @@ Array.fill(32)(32: Byte)
    )(hashFn)
    val manifestDepth = ManifestSerializer.MainnetManifestDepth
    val manifest = new BatchAVLProverManifest[DigestType](leaf, 1)
    val manifestBytes = ManifestSerializer.defaultSerializer.toBytes(manifest)
    UtxoSnapshotScanSource.create(height, blockId, manifestDepth, manifestBytes).get
  }

  private final class RecordingSourceReader(source: UtxoSnapshotScanSource,
                                            parts: IndexedSeq[BatchAVLProverSubtree[DigestType]])
    extends UtxoSnapshotScanSourceReader {
    val readIndexes: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty

    override def readUtxoSnapshotScanSource(): Try[UtxoSnapshotScanSource] = Success(source)
    override def readUtxoSnapshotScanSource(expectedBlockId: ModifierId): Try[UtxoSnapshotScanSource] =
      Success(source)
    override def readUtxoSnapshotScanPart(source: UtxoSnapshotScanSource,
                                          index: Int): Try[BatchAVLProverSubtree[DigestType]] = {
      readIndexes += index
      Try(parts(index))
    }
  }
}
