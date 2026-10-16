package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.{Ints, Shorts}
import org.ergoplatform.core.{VersionTag, idToVersion}
import org.ergoplatform.modifiers.history.header.PreGenesisHeader
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.EmptyHistoryHeight
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedTokenId, encodedTokenId}
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.WalletScanLogic.{ScanResults, SpentInputData}
import org.ergoplatform.wallet.boxes.{TrackedBox, TrackedBoxSerializer}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.util.encode.Base16
import scorex.db.LDBVersionedStore

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.collection.compat.immutable.ArraySeq
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Success, Try}

class WalletRegistrySpec
  extends AnyFlatSpec
    with Matchers
    with DBSpec
    with ScalaCheckPropertyChecks {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 4, sizeRange = 10)

  private val emptyBag = KeyValuePairsBag.empty
  private val walletBoxStatus = Set(PaymentsScanId)

  private val ws = settings.walletSettings

  private def unspentWalletBox(box: TrackedBox): TrackedBox =
    box.copy(spendingHeightOpt = None, spendingTxIdOpt = None, scans = walletBoxStatus)

  private def walletTokenBalances(boxes: Seq[TrackedBox]): Map[EncodedTokenId, Long] =
    boxes
      .flatMap(_.box.additionalTokens.toArray)
      .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amount)) =>
        val encodedId = encodedTokenId(id)
        acc.updated(encodedId, acc.getOrElse(encodedId, 0L) + amount)
      }

  it should "read unspent wallet boxes" in {
    forAll(trackedBoxGen) { box =>
      withVersionedStore(10) { store =>
        val unspentBox = box.copy(spendingHeightOpt = None, spendingTxIdOpt = None, scans = walletBoxStatus)
        WalletRegistry.putBox(emptyBag, unspentBox).transact(store).get

        val registry = new WalletRegistry(store)(settings.walletSettings)
        registry.walletUnspentBoxes() shouldBe Seq(unspentBox)

        //put app box
        val appId = ScanId @@ (PaymentsScanId + 1).toShort
        val unspentAppBox = box.copy(spendingHeightOpt = None, spendingTxIdOpt = None, scans = Set(appId))
        WalletRegistry.putBox(emptyBag, unspentAppBox).transact(store).get
        registry.walletUnspentBoxes() shouldBe Seq(unspentBox)
        registry.allUnspentBoxes() shouldBe Seq(unspentBox, unspentAppBox)
      }
    }
  }

  it should "read spent wallet boxes" in {
    forAll(trackedBoxGen, modifierIdGen) { case (box, txId) =>
      withVersionedStore(10) { store =>
        val spentBox = box.copy(spendingHeightOpt = Some(10000), spendingTxIdOpt = Some(txId), scans = walletBoxStatus)
        WalletRegistry.putBox(emptyBag, spentBox).transact(store).get
        val registry = new WalletRegistry(store)(settings.walletSettings)
        registry.walletSpentBoxes() shouldBe Seq(spentBox)
      }
    }
  }

  it should "read confirmed wallet boxes" in {
    forAll(trackedBoxGen, modifierIdGen) { case (box, txId) =>
      withVersionedStore(10) { store =>
        val unspentBox = box.copy(spendingHeightOpt = None, spendingTxIdOpt = None, scans = walletBoxStatus)
        val spentBox = box.copy(spendingHeightOpt = Some(10000), spendingTxIdOpt = Some(txId), scans = walletBoxStatus)
        WalletRegistry.putBoxes(emptyBag, Seq(unspentBox, spentBox)).transact(store).get
        val registry = new WalletRegistry(store)(settings.walletSettings)
        registry.walletSpentBoxes() shouldBe Seq(spentBox)
        registry.walletUnspentBoxes() shouldBe Seq(unspentBox)
        registry.walletConfirmedBoxes() shouldBe Seq(unspentBox, spentBox)
      }
    }
  }

  it should "read wallet transactions" in {
    forAll(walletTransactionGen) { wtx =>
      withVersionedStore(10) { store =>
        WalletRegistry.putTx(emptyBag, wtx).transact(store).get
        val registry = new WalletRegistry(store)(settings.walletSettings)

        registry.allWalletTxs() shouldBe Seq(wtx)
      }
    }
  }

  it should "update historical boxes when `keepSpentBoxes = true`" in {
    val ws = settings.walletSettings.copy(keepSpentBoxes = true)
    val spendingHeight = 0
    forAll(Gen.nonEmptyListOf(trackedBoxGen), modifierIdGen) { (boxes, txId) =>
      withVersionedStore(10) { store =>
        val unspentBoxes = boxes.map(
          _.copy(spendingHeightOpt = None, spendingTxIdOpt = None, scans = walletBoxStatus))
        val transitedBoxes = unspentBoxes.map(
          _.copy(spendingHeightOpt = Some(spendingHeight), spendingTxIdOpt = Some(txId)))

        WalletRegistry.putBoxes(emptyBag, unspentBoxes).transact(store).get
        val registry = new WalletRegistry(store)(ws)
        registry.processSpentBoxes(emptyBag, unspentBoxes.map(txId -> _), spendingHeight).transact(store).get
        registry.walletSpentBoxes().toList should contain theSameElementsAs transitedBoxes
      }
    }
  }

  it should "updateOnBlock() in correct way - only outputs" in {
    forAll(Gen.nonEmptyListOf(trackedBoxGen)) { boxes =>
      withVersionedStore(10) { store =>
        val registry = new WalletRegistry(store)(settings.walletSettings)
        val blockId = modifierIdGen.sample.get
        val unspentBoxes = boxes.map(unspentWalletBox)
        registry.updateOnBlock(ScanResults(unspentBoxes, ArraySeq.empty, ArraySeq.empty), blockId, 100).get
        registry.walletUnspentBoxes().toList should contain theSameElementsAs unspentBoxes
      }
    }
  }

  it should "report no last version for an empty registry" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)

      registry.lastVersionId shouldBe None
    }
  }

  it should "report the exact block id after updateOnBlock" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val blockId = modifierIdGen.sample.get

      registry.updateOnBlock(
        ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty),
        blockId,
        blockHeight = 100
      ).get

      registry.lastVersionId shouldBe Some(blockId)
    }
  }

  it should "report the exact snapshot block id after the final snapshot chunk" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get

      registry.updateOnSnapshotChunk(
        ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty),
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        finalChunk = true
      ).get

      registry.lastVersionId shouldBe Some(snapshotBlockId)
    }
  }

  it should "fail closed when the stored last version is not a modifier id" in {
    withVersionedStore(10) { store =>
      store.update(Array.fill(31)(1.toByte), Seq.empty, Seq.empty).get
      val registry = new WalletRegistry(store)(settings.walletSettings)

      registry.lastVersionId shouldBe None
    }

    withVersionedStore(10) { store =>
      store.update(Array.fill(33)(1.toByte), Seq.empty, Seq.empty).get
      val registry = new WalletRegistry(store)(settings.walletSettings)

      registry.lastVersionId shouldBe None
    }
  }

  it should "close an opened store when registry initialization fails" in {
    val registryDir = createTempDir
    val initializationFailure = new IllegalStateException("injected initialization failure")
    val closeCount = new AtomicInteger(0)
    val store = new LDBVersionedStore(registryDir, 10) {
      override def versionIdExists(versionID: Array[Byte]): Boolean =
        throw initializationFailure

      override def close(): Unit = {
        closeCount.incrementAndGet()
        super.close()
      }
    }

    val result = WalletRegistry.initializeOpenedStore(store, settings.walletSettings)

    try {
      result.isFailure shouldBe true
      (result.failed.get eq initializationFailure) shouldBe true
      closeCount.get() shouldBe 1

      val reopenedStore = new LDBVersionedStore(registryDir, 10)
      reopenedStore.close()
    } finally {
      if (closeCount.get() == 0) {
        Try(store.close())
      }
    }
  }

  it should "preserve the initialization failure when closing the opened store also fails" in {
    val registryDir = createTempDir
    val initializationFailure = new IllegalStateException("injected initialization failure")
    val closeFailure = new IllegalStateException("injected close failure")
    val closeCount = new AtomicInteger(0)
    val store = new LDBVersionedStore(registryDir, 10) {
      override def versionIdExists(versionID: Array[Byte]): Boolean =
        throw initializationFailure

      override def close(): Unit = {
        closeCount.incrementAndGet()
        super.close()
        throw closeFailure
      }
    }

    val result = WalletRegistry.initializeOpenedStore(store, settings.walletSettings)

    try {
      result.isFailure shouldBe true
      val reportedFailure = result.failed.get
      (reportedFailure eq initializationFailure) shouldBe true
      reportedFailure.getSuppressed.length shouldBe 1
      (reportedFailure.getSuppressed.head eq closeFailure) shouldBe true
      closeCount.get() shouldBe 1

      val reopenedStore = new LDBVersionedStore(registryDir, 10)
      reopenedStore.close()
    } finally {
      if (closeCount.get() == 0) {
        Try(store.close())
      }
    }
  }

  it should "keep successful registry initialization through apply unchanged" in {
    val isolatedSettings = settings.copy(directory = createTempDir.getAbsolutePath)
    val registry = WalletRegistry(isolatedSettings).get

    try {
      registry.lastVersionId shouldBe Some(PreGenesisHeader.id)
    } finally {
      registry.close()
    }
  }

  it should "updateOnSnapshotChunk accumulates balances and tokens until final snapshot chunk" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val box1 = unspentWalletBox(trackedBoxGen.sample.get)
      val box2 = unspentWalletBox(trackedBoxGen.sample.get)

      registry.fetchDigest().height shouldBe EmptyHistoryHeight
      registry.updateOnSnapshotChunk(
        ScanResults(Seq(box1), ArraySeq.empty, ArraySeq.empty),
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        finalChunk = false
      ).get

      registry.walletUnspentBoxes().toList should contain theSameElementsAs Seq(box1)
      registry.fetchDigest().height shouldBe EmptyHistoryHeight
      registry.fetchDigest().walletBalance shouldBe box1.box.value
      registry.fetchDigest().walletAssetBalances.toMap shouldBe walletTokenBalances(Seq(box1))
      val intermediateVersionId = registry.lastVersionId
      intermediateVersionId.isDefined shouldBe true
      intermediateVersionId should not equal Some(snapshotBlockId)

      registry.updateOnSnapshotChunk(
        ScanResults(Seq(box2), ArraySeq.empty, ArraySeq.empty),
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 1,
        nextSubtreeIndex = 2,
        finalChunk = true
      ).get

      val allBoxes = Seq(box1, box2)
      registry.walletUnspentBoxes().toList should contain theSameElementsAs allBoxes
      registry.fetchDigest().height shouldBe 100
      registry.fetchDigest().walletBalance shouldBe allBoxes.map(_.box.value).sum
      registry.fetchDigest().walletAssetBalances.toMap shouldBe walletTokenBalances(allBoxes)
      registry.lastVersionId shouldBe Some(snapshotBlockId)
    }
  }

  it should "updateOnSnapshotChunk be idempotent when the same snapshot chunk is delivered twice" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val box = unspentWalletBox(trackedBoxGen.sample.get)
      val scanResults = ScanResults(Seq(box), ArraySeq.empty, ArraySeq.empty)

      registry.updateOnSnapshotChunk(
        scanResults,
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        nextSubtreeIndex = 1,
        finalChunk = false
      ).get
      registry.updateOnSnapshotChunk(
        scanResults,
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        finalChunk = false
      ).get

      registry.walletUnspentBoxes() shouldBe Seq(box)
      registry.fetchDigest().walletBalance shouldBe box.box.value
      registry.fetchDigest().walletAssetBalances.toMap shouldBe walletTokenBalances(Seq(box))
    }
  }

  it should "updateOnSnapshotChunk reject different contents for an already applied snapshot chunk" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val box1 = unspentWalletBox(trackedBoxGen.sample.get)
      val box2 = unspentWalletBox(trackedBoxGen.suchThat(box => !box.box.id.sameElements(box1.box.id)).sample.get)

      registry.updateOnSnapshotChunk(
        ScanResults(Seq(box1), ArraySeq.empty, ArraySeq.empty),
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        finalChunk = false
      ).get

      registry.updateOnSnapshotChunk(
        ScanResults(Seq(box2), ArraySeq.empty, ArraySeq.empty),
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        finalChunk = false
      ).isFailure shouldBe true

      registry.walletUnspentBoxes() shouldBe Seq(box1)
      registry.fetchDigest().walletBalance shouldBe box1.box.value
      registry.fetchDigest().walletAssetBalances.toMap shouldBe walletTokenBalances(Seq(box1))
    }
  }

  it should "updateOnSnapshotChunk reject a replay at a different snapshot height" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val box = unspentWalletBox(trackedBoxGen.sample.get)
      val scanResults = ScanResults(Seq(box), ArraySeq.empty, ArraySeq.empty)

      registry.updateOnSnapshotChunk(scanResults, snapshotBlockId, 100, 0, finalChunk = false).get
      registry.updateOnSnapshotChunk(scanResults, snapshotBlockId, 101, 0, finalChunk = false).isFailure shouldBe true

      registry.fetchDigest().walletBalance shouldBe box.box.value
      registry.cache.keySet shouldBe Set(box.boxId)
    }
  }

  it should "updateOnSnapshotChunk reject a replay with a different final-chunk flag" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val box = unspentWalletBox(trackedBoxGen.sample.get)
      val scanResults = ScanResults(Seq(box), ArraySeq.empty, ArraySeq.empty)

      registry.updateOnSnapshotChunk(scanResults, snapshotBlockId, 100, 0, finalChunk = false).get
      registry.updateOnSnapshotChunk(scanResults, snapshotBlockId, 100, 0, finalChunk = true).isFailure shouldBe true

      registry.fetchDigest().height shouldBe EmptyHistoryHeight
      registry.fetchDigest().walletBalance shouldBe box.box.value
    }
  }

  it should "updateOnSnapshotChunk reject a replay claiming a different part range" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val box = unspentWalletBox(trackedBoxGen.sample.get)
      val scanResults = ScanResults(Seq(box), ArraySeq.empty, ArraySeq.empty)

      registry.updateOnSnapshotChunk(
        scanResults,
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        nextSubtreeIndex = 32,
        finalChunk = false
      ).get
      registry.updateOnSnapshotChunk(
        scanResults,
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        nextSubtreeIndex = 31,
        finalChunk = false
      ).isFailure shouldBe true

      registry.walletUnspentBoxes() shouldBe Seq(box)
      registry.fetchDigest().walletBalance shouldBe box.box.value
    }
  }

  it should "updateOnSnapshotChunk reject an empty or backwards part range" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val scanResults = ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty)

      registry.updateOnSnapshotChunk(
        scanResults,
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 3,
        nextSubtreeIndex = 3,
        finalChunk = false
      ).isFailure shouldBe true
      registry.updateOnSnapshotChunk(
        scanResults,
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 3,
        nextSubtreeIndex = 2,
        finalChunk = false
      ).isFailure shouldBe true
      registry.walletUnspentBoxes() shouldBe empty
      registry.fetchDigest() shouldBe WalletDigest.empty
    }
  }

  it should "updateOnSnapshotChunk reject changed tracked-box metadata for the same box id" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val box = unspentWalletBox(trackedBoxGen.sample.get).copy(inclusionHeightOpt = Some(100))
      val changedMetadata = box.copy(inclusionHeightOpt = Some(101))

      registry.updateOnSnapshotChunk(
        ScanResults(Seq(box), ArraySeq.empty, ArraySeq.empty), snapshotBlockId, 100, 0, finalChunk = false
      ).get
      registry.updateOnSnapshotChunk(
        ScanResults(Seq(changedMetadata), ArraySeq.empty, ArraySeq.empty), snapshotBlockId, 100, 0, finalChunk = false
      ).isFailure shouldBe true

      registry.getBox(box.box.id).get.inclusionHeightOpt shouldBe Some(100)
      registry.cache(box.boxId).inclusionHeightOpt shouldBe Some(100)
    }
  }

  it should "updateOnSnapshotChunk treat output order as part of the snapshot chunk identity" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val box1 = unspentWalletBox(trackedBoxGen.sample.get)
      val box2 = unspentWalletBox(trackedBoxGen.suchThat(box => !box.box.id.sameElements(box1.box.id)).sample.get)

      registry.updateOnSnapshotChunk(
        ScanResults(Seq(box1, box2), ArraySeq.empty, ArraySeq.empty), snapshotBlockId, 100, 0, finalChunk = false
      ).get
      registry.updateOnSnapshotChunk(
        ScanResults(Seq(box2, box1), ArraySeq.empty, ArraySeq.empty), snapshotBlockId, 100, 0, finalChunk = false
      ).isFailure shouldBe true

      registry.walletUnspentBoxes().toSet shouldBe Set(box1, box2)
      registry.fetchDigest().walletBalance shouldBe box1.box.value + box2.box.value
    }
  }

  it should "updateOnSnapshotChunk leave the cache unchanged after rejecting a replay" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val accepted = unspentWalletBox(trackedBoxGen.sample.get)
      val rejected = unspentWalletBox(
        trackedBoxGen.suchThat(box => !box.box.id.sameElements(accepted.box.id)).sample.get
      )

      registry.updateOnSnapshotChunk(
        ScanResults(Seq(accepted), ArraySeq.empty, ArraySeq.empty), snapshotBlockId, 100, 0, finalChunk = false
      ).get
      val cacheBefore = registry.cache.toMap

      registry.updateOnSnapshotChunk(
        ScanResults(Seq(rejected), ArraySeq.empty, ArraySeq.empty), snapshotBlockId, 100, 0, finalChunk = false
      ).isFailure shouldBe true

      registry.cache.toMap shouldBe cacheBefore
      registry.getBox(rejected.box.id) shouldBe None
    }
  }

  it should "updateOnSnapshotChunk allow replay after rollback removes the marker" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val previousVersion = scorex.utils.Random.randomBytes()
      val snapshotBlockId = modifierIdGen.sample.get
      val box = unspentWalletBox(trackedBoxGen.sample.get)
      val scanResults = ScanResults(Seq(box), ArraySeq.empty, ArraySeq.empty)

      WalletRegistry.putDigest(KeyValuePairsBag.empty, WalletDigest.empty).transact(store, previousVersion).get
      registry.updateOnSnapshotChunk(scanResults, snapshotBlockId, 100, 0, finalChunk = false).get
      registry.rollback(VersionTag @@ Base16.encode(previousVersion)).get

      registry.walletUnspentBoxes() shouldBe empty
      registry.fetchDigest() shouldBe WalletDigest.empty
      registry.updateOnSnapshotChunk(scanResults, snapshotBlockId, 100, 0, finalChunk = false).get
      registry.walletUnspentBoxes() shouldBe Seq(box)
      registry.fetchDigest().walletBalance shouldBe box.box.value
    }
  }

  it should "updateOnSnapshotChunk replay a logically identical multi-scan chunk after registry reopen" in {
    val isolatedSettings = settings.copy(directory = createTempDir.getAbsolutePath)
    val registryDir = WalletRegistry.registryFolder(isolatedSettings)
    val snapshotBlockId = modifierIdGen.sample.get
    val scanIds = Seq(
      PaymentsScanId,
      ScanId @@ 50.toShort,
      ScanId @@ 51.toShort)
    val initialScans = scanIds.foldLeft(Set.empty[ScanId])(_ + _)
    val replayScans = scanIds.reverse.foldLeft(Set.empty[ScanId])(_ + _)
    initialScans shouldBe replayScans
    initialScans.toSeq should not equal replayScans.toSeq

    val trackedBox = unspentWalletBox(trackedBoxGen.sample.get).copy(scans = initialScans)
    val initialResults = ScanResults(Seq(trackedBox), ArraySeq.empty, ArraySeq.empty)
    val initialStore = new LDBVersionedStore(registryDir, isolatedSettings.nodeSettings.keepVersions)
    val initialRegistry = WalletRegistry.initializeOpenedStore(initialStore, isolatedSettings.walletSettings).get
    try {
      initialRegistry.updateOnSnapshotChunk(
        initialResults,
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        nextSubtreeIndex = 32,
        finalChunk = false
      ).get
    } finally {
      initialRegistry.close()
    }

    val reopenedStore = new LDBVersionedStore(registryDir, isolatedSettings.nodeSettings.keepVersions)
    val reopened = WalletRegistry.initializeOpenedStore(reopenedStore, isolatedSettings.walletSettings).get
    try {
      val persisted = reopened.getBox(trackedBox.box.id).get
      val replayBox = persisted.copy(scans = replayScans)
      val boxKey = Array(0x01.toByte) ++ trackedBox.box.id
      persisted.scans shouldBe replayBox.scans
      java.util.Arrays.equals(
        TrackedBoxSerializer.toBytes(persisted),
        TrackedBoxSerializer.toBytes(replayBox)
      ) shouldBe false
      val digestBeforeReplay = WalletDigestSerializer.toBytes(reopened.fetchDigest())
      val versionBeforeReplay = reopened.lastVersionId
      val persistedBytesBeforeReplay = reopenedStore.get(boxKey).get
      val cacheBytesBeforeReplay = TrackedBoxSerializer.toBytes(reopened.cache(trackedBox.boxId))
      val unspentBytesBeforeReplay = reopened.walletUnspentBoxes().map(TrackedBoxSerializer.toBytes)

      reopened.updateOnSnapshotChunk(
        ScanResults(Seq(replayBox), ArraySeq.empty, ArraySeq.empty),
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        nextSubtreeIndex = 32,
        finalChunk = false
      ).get

      WalletDigestSerializer.toBytes(reopened.fetchDigest()) should contain theSameElementsInOrderAs digestBeforeReplay
      reopened.lastVersionId shouldBe versionBeforeReplay
      reopenedStore.get(boxKey).get should contain theSameElementsInOrderAs persistedBytesBeforeReplay
      TrackedBoxSerializer.toBytes(reopened.cache(trackedBox.boxId)) should contain theSameElementsInOrderAs cacheBytesBeforeReplay
      reopened.walletUnspentBoxes().map(TrackedBoxSerializer.toBytes) should have size 1
      reopened.walletUnspentBoxes().map(TrackedBoxSerializer.toBytes).head should contain theSameElementsInOrderAs unspentBytesBeforeReplay.head
      reopened.walletUnspentBoxes().map(_.boxId) shouldBe Seq(trackedBox.boxId)
    } finally {
      reopened.close()
    }
  }

  it should "updateOnSnapshotChunk reject a replay when only the scan set changes for the same box and marker scanSetDeltaMarker" in {
    val isolatedSettings = settings.copy(directory = createTempDir.getAbsolutePath)
    val registryDir = WalletRegistry.registryFolder(isolatedSettings)
    val snapshotBlockId = modifierIdGen.sample.get
    val acceptedScans = Set(
      PaymentsScanId,
      ScanId @@ 50.toShort,
      ScanId @@ 51.toShort)
    val changedScans = Set(
      PaymentsScanId,
      ScanId @@ 50.toShort,
      ScanId @@ 52.toShort)
    acceptedScans should not equal changedScans

    val trackedBox = unspentWalletBox(trackedBoxGen.sample.get).copy(scans = acceptedScans)
    val initialStore = new LDBVersionedStore(registryDir, isolatedSettings.nodeSettings.keepVersions)
    val initialRegistry = WalletRegistry.initializeOpenedStore(initialStore, isolatedSettings.walletSettings).get
    try {
      initialRegistry.updateOnSnapshotChunk(
        ScanResults(Seq(trackedBox), ArraySeq.empty, ArraySeq.empty),
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        nextSubtreeIndex = 32,
        finalChunk = false
      ).get
    } finally {
      initialRegistry.close()
    }

    val reopenedStore = new LDBVersionedStore(registryDir, isolatedSettings.nodeSettings.keepVersions)
    val reopened = WalletRegistry.initializeOpenedStore(reopenedStore, isolatedSettings.walletSettings).get
    try {
      val persisted = reopened.getBox(trackedBox.box.id).get
      val replayBox = persisted.copy(scans = changedScans)
      val boxKey = Array(0x01.toByte) ++ trackedBox.box.id
      val digestBeforeReplay = WalletDigestSerializer.toBytes(reopened.fetchDigest())
      val versionBeforeReplay = reopened.lastVersionId
      val persistedBytesBeforeReplay = reopenedStore.get(boxKey).get
      val cacheBytesBeforeReplay = TrackedBoxSerializer.toBytes(reopened.cache(trackedBox.boxId))
      val unspentBeforeReplay = reopened.walletUnspentBoxes()
      val unspentBytesBeforeReplay = unspentBeforeReplay.map(TrackedBoxSerializer.toBytes)

      reopened.updateOnSnapshotChunk(
        ScanResults(Seq(replayBox), ArraySeq.empty, ArraySeq.empty),
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        nextSubtreeIndex = 32,
        finalChunk = false
      ).isFailure shouldBe true

      WalletDigestSerializer.toBytes(reopened.fetchDigest()) should contain theSameElementsInOrderAs digestBeforeReplay
      reopened.lastVersionId shouldBe versionBeforeReplay
      reopenedStore.get(boxKey).get should contain theSameElementsInOrderAs persistedBytesBeforeReplay
      TrackedBoxSerializer.toBytes(reopened.cache(trackedBox.boxId)) should contain theSameElementsInOrderAs cacheBytesBeforeReplay
      reopened.walletUnspentBoxes().map(TrackedBoxSerializer.toBytes) should have size 1
      reopened.walletUnspentBoxes().map(TrackedBoxSerializer.toBytes).head should contain theSameElementsInOrderAs unspentBytesBeforeReplay.head
      reopened.walletUnspentBoxes().map(_.boxId) shouldBe unspentBeforeReplay.map(_.boxId)
    } finally {
      reopened.close()
    }
  }

  it should "updateOnSnapshotChunk persist an explicitly versioned marker value" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val box = unspentWalletBox(trackedBoxGen.sample.get)

      registry.updateOnSnapshotChunk(
        ScanResults(Seq(box), ArraySeq.empty, ArraySeq.empty), snapshotBlockId, 100, 0, finalChunk = false
      ).get

      val markerValues = store.getAll.collect {
        case (key, value) if key.length == 37 && key.head == 0x09.toByte => value
      }.toSeq
      markerValues should have size 1
      markerValues.head should have length 33
      markerValues.head.head shouldBe 2.toByte
    }
  }

  it should "updateOnSnapshotChunk serialize competing payloads for the same marker key" in {
    val markerReadCount = new AtomicInteger(0)
    val firstMarkerReadEntered = new CountDownLatch(1)
    val releaseFirstMarkerRead = new CountDownLatch(1)
    val secondMarkerReadEntered = new CountDownLatch(1)
    val store = new LDBVersionedStore(createTempDir, 10) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = {
        if (key.length == 37 && key.head == 0x09.toByte) {
          if (markerReadCount.incrementAndGet() == 1) {
            firstMarkerReadEntered.countDown()
            releaseFirstMarkerRead.await(5, TimeUnit.SECONDS)
          } else {
            secondMarkerReadEntered.countDown()
          }
          super.get(key)
        } else {
          super.get(key)
        }
      }
    }

    try {
      implicit val ec: ExecutionContext = ExecutionContext.global
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val box1 = unspentWalletBox(trackedBoxGen.sample.get)
      val box2 = unspentWalletBox(trackedBoxGen.suchThat(box => !box.box.id.sameElements(box1.box.id)).sample.get)

      def submit(box: TrackedBox): Future[(TrackedBox, Try[Unit])] =
        Future(box -> registry.updateOnSnapshotChunk(
          ScanResults(Seq(box), ArraySeq.empty, ArraySeq.empty), snapshotBlockId, 100, 0, finalChunk = false
        ))

      val firstAttempt = submit(box1)
      firstMarkerReadEntered.await(5, TimeUnit.SECONDS) shouldBe true
      val secondAttempt = submit(box2)

      // The first call is parked inside marker get while holding the snapshot lock.
      // A competing call must not reach its marker read until the first transaction completes.
      secondMarkerReadEntered.await(1, TimeUnit.SECONDS) shouldBe false
      secondAttempt.isCompleted shouldBe false
      releaseFirstMarkerRead.countDown()

      val attempts = Seq(firstAttempt, secondAttempt)
      val outcomes = Await.result(Future.sequence(attempts), 10.seconds)
      val winners = outcomes.collect { case (box, Success(_)) => box }

      winners should have size 1
      outcomes.count(_._2.isFailure) shouldBe 1
      registry.walletUnspentBoxes() shouldBe winners
      registry.fetchDigest().walletBalance shouldBe winners.head.box.value
      registry.fetchDigest().walletAssetBalances.toMap shouldBe walletTokenBalances(winners)
      registry.cache.keySet shouldBe winners.map(_.boxId).toSet
    } finally {
      releaseFirstMarkerRead.countDown()
      store.close()
    }
  }

  it should "updateOnSnapshotChunk writes a final snapshot version even if no wallet boxes are found" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get

      registry.updateOnSnapshotChunk(
        ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty),
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        finalChunk = true
      ).get

      registry.fetchDigest().height shouldBe 100
      registry.rollback(idToVersion(snapshotBlockId)).isSuccess shouldBe true
      registry.fetchDigest().height shouldBe 100
    }
  }

  it should "updateOnSnapshotChunk rejects block scan data" in {
    withVersionedStore(10) { store =>
      val registry = new WalletRegistry(store)(settings.walletSettings)
      val snapshotBlockId = modifierIdGen.sample.get
      val box = unspentWalletBox(trackedBoxGen.sample.get)
      val spent = SpentInputData(modifierIdGen.sample.get, box)

      registry.updateOnSnapshotChunk(
        ScanResults(Seq(box), Seq(spent), ArraySeq.empty),
        snapshotBlockId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        finalChunk = false
      ).isFailure shouldBe true
    }
  }

  private def outputsSpentTest(keepSpent: Boolean): Unit = forAll(Gen.nonEmptyListOf(trackedBoxGen)) { boxes =>
    withVersionedStore(10) { store =>
      val fakeTxId = modifierIdGen.sample.get
      val registry = new WalletRegistry(store)(settings.walletSettings.copy(keepSpentBoxes = keepSpent))
      val blockId = modifierIdGen.sample.get
      val outs = boxes.map { bx =>
        bx.copy(spendingHeightOpt = None, spendingTxIdOpt = None, scans = walletBoxStatus)
      }
      val inputs = outs.map(tb => SpentInputData(fakeTxId, tb))
      registry.updateOnBlock(ScanResults(outs, inputs, ArraySeq.empty), blockId, 100).get
      registry.walletUnspentBoxes() shouldBe Seq.empty
    }
  }

  it should "updateOnBlock() in correct way - outputs spent" in {
    outputsSpentTest(keepSpent = false)
    outputsSpentTest(keepSpent = true)
  }

  it should "putBox/getBox/removeBox" in {
    forAll(trackedBoxGen) { tb =>
      withVersionedStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putBox(emptyBag, tb).transact(store).get
        reg.getBox(tb.box.id) shouldBe Some(tb)
        reg.cache -= tb.boxId
        WalletRegistry.removeBoxes(emptyBag, Seq(tb)).transact(store).get
        reg.getBox(tb.box.id) shouldBe None
      }
    }
  }

  it should "putBox/removeBox - 2 versions" in {
    forAll(trackedBoxGen) { tb =>
      withVersionedStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        val tb1 = tb.copy(spendingHeightOpt = None, spendingTxIdOpt = None)
        val bag1 = WalletRegistry.putBox(emptyBag, tb1)

        val tb2 = tb.copy(spendingHeightOpt = Some(5000), spendingTxIdOpt = Some(modifierIdGen.sample.get))
        val bag2 = WalletRegistry.removeBox(bag1, tb1)
        WalletRegistry.putBox(bag2, tb2).transact(store).get
        reg.getBox(tb.box.id) shouldBe Some(tb2)
        reg.walletUnspentBoxes() shouldBe Seq.empty
      }
    }
  }

  it should "putBoxes/getBoxes/removeBoxes" in {
    forAll(Gen.listOf(trackedBoxGen)) { tbs =>
      withVersionedStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putBoxes(emptyBag, tbs).transact(store).get
        reg.getBoxes(tbs.map(_.box.id)) should contain theSameElementsAs tbs.map(Some.apply)
        val updateFn = (tb: TrackedBox) => tb.copy(spendingHeightOpt = Some(0),
          scans = Set(PaymentsScanId, ScanId @@ 2.toShort))
        val updatedBoxes = tbs.map(updateFn)
        reg.getBoxes(tbs.map(_.box.id)) should contain theSameElementsAs updatedBoxes.map(Some.apply)
        reg.cache --= tbs.map(_.boxId)
        WalletRegistry.removeBoxes(emptyBag, tbs).transact(store).get
        reg.getBoxes(tbs.map(_.box.id)).flatten shouldBe Seq()
      }
    }
  }

  it should "putTx/getTx/getAllTxs/removeTxs" in {
    forAll(walletTransactionGen) { wtx =>
      withVersionedStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putTx(emptyBag, wtx).transact(store).get
        reg.getTx(wtx.id) shouldEqual Some(wtx)
        reg.allWalletTxs() shouldEqual Seq(wtx)
        WalletRegistry.removeTxs(emptyBag, Seq(wtx)).transact(store).get
        reg.allWalletTxs() should not contain wtx
      }
    }
  }

  it should "putTxs/getAllTxs" in {
    forAll(Gen.listOf(walletTransactionGen)) { wtxs =>
      withVersionedStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putTxs(emptyBag, wtxs).transact(store).get
        reg.allWalletTxs() should contain theSameElementsAs wtxs
      }
    }
  }

  it should "putIndex/digest/updateIndex" in {
    forAll(registrySummaryGen) { index =>
      withVersionedStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)

        WalletRegistry.putDigest(emptyBag, index).transact(store).get
        reg.fetchDigest() shouldBe index
        val updatedIndex = index.copy(height = 0, walletBalance = 0)
        reg.updateDigest(emptyBag)(_ => Success(updatedIndex)).get.transact(store).get
        reg.fetchDigest() shouldBe updatedIndex
      }
    }
  }

  it should "update scans correctly" in {
    val appId1: ScanId = ScanId @@ 21.toShort
    val appId2: ScanId = ScanId @@ 22.toShort

    forAll(trackedBoxGen) { tb0 =>
      withVersionedStore(10) { store =>
        val tb1 = tb0.copy(scans = Set(appId1, appId2), spendingHeightOpt = None, spendingTxIdOpt = None)

        val reg = new WalletRegistry(store)(ws)
        WalletRegistry.putBox(emptyBag, tb1).transact(store).get
        reg.getBox(tb1.box.id).get.scans shouldBe Set(appId1, appId2)
        reg.unspentBoxes(appId1).length shouldBe 1
        reg.unspentBoxes(appId2).length shouldBe 1
        reg.updateScans(Set(appId1), tb1.box)
        reg.getBox(tb1.box.id).get.scans shouldBe Set(appId1)
        reg.unspentBoxes(appId1).length shouldBe 1
        reg.unspentBoxes(appId2).length shouldBe 0
        // limit should by applied
        reg.unspentBoxes(appId1, limit = 1).length shouldBe 1
        reg.unspentBoxes(appId1, limit = 0).length shouldBe 0
      }
    }
  }

  it should "get unspent boxes by height from/to inclusive" in {
    val appId1: ScanId = ScanId @@ 21.toShort
    val appId2: ScanId = ScanId @@ 22.toShort
    forAll(trackedBoxGen) { tb0 =>
      withVersionedStore(10) { store =>
        val tb1 = tb0.copy(scans = Set(appId1), inclusionHeightOpt = Some(5), spendingHeightOpt = None)
        val reg = new WalletRegistry(store)(ws)
        WalletRegistry.putBox(emptyBag, tb1).transact(store).get
        reg.getBox(tb1.box.id).get.scans shouldBe Set(appId1)
        reg.boxesByInclusionHeight(appId1, 1, 4).length shouldBe 0
        reg.boxesByInclusionHeight(appId1, 6, 10).length shouldBe 0
        reg.boxesByInclusionHeight(appId1, 4, 6).length shouldBe 1
        reg.boxesByInclusionHeight(appId1, 5, 6).length shouldBe 1
        reg.boxesByInclusionHeight(appId1, 5, 5).length shouldBe 1
        reg.boxesByInclusionHeight(appId1, 4, 5).length shouldBe 1
        // put another box under the same scan id should result in 2 matches
        val tb2 = trackedBoxGen.sample.get.copy(scans = Set(appId1), inclusionHeightOpt = Some(6), spendingHeightOpt = None)
        WalletRegistry.putBox(emptyBag, tb2).transact(store).get
        reg.boxesByInclusionHeight(appId1, 4, 7).length shouldBe 2
        reg.boxesByInclusionHeight(appId1, 4, 5).length shouldBe 1
        // search should differentiate between scan ids
        val tb3 = trackedBoxGen.sample.get.copy(scans = Set(appId2), inclusionHeightOpt = Some(6), spendingHeightOpt = None)
        WalletRegistry.putBox(emptyBag, tb3).transact(store).get
        reg.boxesByInclusionHeight(appId1, 4, 7).length shouldBe 2
        reg.boxesByInclusionHeight(appId2, 4, 7).length shouldBe 1
        // putting 2 different boxes under same height should result in 2 matches
        val tb4 = trackedBoxGen.sample.get.copy(scans = Set(appId2), inclusionHeightOpt = Some(6), spendingHeightOpt = None)
        WalletRegistry.putBox(emptyBag, tb4).transact(store).get
        reg.boxesByInclusionHeight(appId2, 4, 7).length shouldBe 2
        // putting 2 identical boxes should be idempotent operation
        WalletRegistry.putBox(emptyBag, tb4).transact(store).get
        reg.boxesByInclusionHeight(appId2, 4, 7).length shouldBe 2
        // spent boxes should be included
        val tb5 = trackedBoxGen.sample.get.copy(scans = Set(appId2), inclusionHeightOpt = Some(5), spendingHeightOpt = Some(6))
        WalletRegistry.putBox(emptyBag, tb5).transact(store).get
        reg.boxesByInclusionHeight(appId2, 4, 7).length shouldBe 3
        // one spent box and 2 unspent boxes should be present
        reg.spentBoxesByInclusionHeight(appId2, 4, 7).length shouldBe 1
        reg.unspentBoxesByInclusionHeight(appId2, 4, 7).length shouldBe 2
      }
    }
  }

  it should "remove application from a box correctly" in {
    val appId: ScanId = ScanId @@ 20.toShort

    forAll(trackedBoxGen) { tb0 =>
      val tb = tb0.copy(scans = Set(appId))
      withVersionedStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)
        WalletRegistry.putBox(emptyBag, tb).transact(store).get
        reg.getBox(tb.box.id).isDefined shouldBe true
        reg.removeScan(tb.box.id, appId).isSuccess shouldBe true
        reg.getBox(tb.box.id).isDefined shouldBe false
      }
    }

  }

  it should "remove box-scan correspondence and then rollback - one app" in {
    val scanId: ScanId = ScanId @@ 20.toShort

    forAll(trackedBoxGen) { tb0 =>
      val tb = tb0.copy(scans = Set(scanId))
      withVersionedStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)
        val version = scorex.utils.Random.randomBytes()

        WalletRegistry.putBox(emptyBag, tb).transact(store, version).get
        reg.getBox(tb.box.id).isDefined shouldBe true
        reg.removeScan(tb.box.id, scanId).isSuccess shouldBe true
        reg.getBox(tb.box.id).isDefined shouldBe false
        reg.rollback(VersionTag @@ Base16.encode(version)).isSuccess shouldBe true
        reg.getBox(tb.box.id).isDefined shouldBe false
      }
    }
  }

  it should "remove box-scan correspondence and then rollback - multiple apps" in {
    val scanId: ScanId = ScanId @@ 20.toShort

    forAll(trackedBoxGen) { tb0 =>
      val tb = tb0.copy(scans = Set(PaymentsScanId, scanId))
      withVersionedStore(10) { store =>
        val reg = new WalletRegistry(store)(ws)
        val version = scorex.utils.Random.randomBytes()

        WalletRegistry.putBox(emptyBag, tb).transact(store, version).get
        reg.getBox(tb.box.id).get.scans.size shouldBe 2
        reg.removeScan(tb.box.id, scanId).isSuccess shouldBe true
        reg.getBox(tb.box.id).get.scans.size shouldBe 1
        reg.rollback(VersionTag @@ Base16.encode(version)).isSuccess shouldBe true
        reg.getBox(tb.box.id).get.scans.size shouldBe 1
        reg.getBox(tb.box.id).get.scans shouldBe Set(PaymentsScanId)
      }
    }
  }

  it should "compose keys correctly" in {
    val box = trackedBoxGen.sample.get

    forAll { (prefix: Byte, scanId: Short, height: Int, suffix: Byte) =>
      val key1 = (prefix +: Shorts.toByteArray(scanId)) ++ Array.fill(32)(suffix)
      WalletRegistry.composeKey(prefix, ScanId @@ scanId, suffix) shouldBe key1

      val key2 = (prefix +: Shorts.toByteArray(scanId)) ++ Ints.toByteArray(height) ++ Array.fill(32)(suffix)
      WalletRegistry.composeKey(prefix, ScanId @@ scanId, height, suffix) shouldBe key2

      val id = box.box.id
      val key3 = (prefix +: Shorts.toByteArray(scanId)) ++ id
      WalletRegistry.composeKeyWithId(prefix, ScanId @@ scanId, id) shouldBe key3

      val key4 = (prefix +: Shorts.toByteArray(scanId)) ++ Ints.toByteArray(height) ++ id
      WalletRegistry.composeKeyWithHeightAndId(prefix, ScanId @@ scanId, height, id) shouldBe key4
    }
  }

}
