package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.{ADProofs, HistoryModifierSerializer}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, IndexedErgoBox, StorageRentBox}
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.crypto.authds.SerializedAdProof
import scorex.db.{ByteArrayWrapper, LDBFactory, LDBKVStore}
import scorex.util.{ModifierId, bytesToId, idToBytes}
import sigmastate.helpers.TestingHelpers.testBox

import java.io.IOException
import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import org.iq80.leveldb.Options
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class HistoryStorageSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  val db = HistoryStorage(settings)

  property("Write Read Remove") {
    val headers: Array[Header] = Gen.listOfN(20, defaultHeaderGen).sample.get.toArray
    val modifiers: Array[ADProofs] = Gen.listOfN(20, randomADProofsGen).sample.get.toArray
    def validityKey(id: ModifierId) = ByteArrayWrapper(Algos.hash("validity".getBytes(CharsetName) ++ idToBytes(id)))
    val indexes = headers.flatMap(h => Array(validityKey(h.id) -> Array(1.toByte)))
    db.insert(indexes, (headers ++ modifiers).asInstanceOf[Array[BlockSection]]) shouldBe 'success

    headers.forall(h => db.contains(h.id)) shouldBe true
    modifiers.forall(m => db.contains(m.id)) shouldBe true

    headers.forall(h => db.get(h.id).exists(_.nonEmpty)) shouldBe true
    modifiers.forall(m => db.get(m.id).exists(_.nonEmpty)) shouldBe true
    indexes.forall(i => db.getIndex(i._1).exists(_.nonEmpty)) shouldBe true

    db.remove(indexes.map(_._1), headers.map(_.id) ++ modifiers.map(_.id))

    headers.forall(h => !db.contains(h.id)) shouldBe true
    modifiers.forall(m => !db.contains(m.id)) shouldBe true

    headers.forall(h => !db.get(h.id).exists(_.nonEmpty)) shouldBe true
    modifiers.forall(m => !db.get(m.id).exists(_.nonEmpty)) shouldBe true
    indexes.forall(i => !db.getIndex(i._1).exists(_.nonEmpty)) shouldBe true
  }

  /** Insert a distinct box and its storage-rent eligibility entry, return the box row. */
  private def insertRentBox(globalIndex: Long): IndexedErgoBox = {
    val creationHeight = 1000 + globalIndex.toInt
    val box = testBox(1000000000L, Constants.TrueTree, creationHeight)
    val iEb = new IndexedErgoBox(creationHeight, None, None, None, box, globalIndex)
    db.insertExtra(Array.empty, Array[ExtraIndex](iEb, StorageRentBox(iEb)))
    iEb
  }

  private def rentBoxIndexesInIndex(limit: Int): Seq[Long] =
    db.storageRentBoxesAtOrBefore(Int.MaxValue, limit).map(_.globalIndex).toSeq

  property("storage rent entries are removable by box id") {
    val iEbs = (0L until 3).map(insertRentBox)
    val boxIds = iEbs.map(_.id)
    rentBoxIndexesInIndex(10) shouldBe Seq(0L, 1L, 2L)

    // removing a subset removes exactly those entries, in key order for the rest
    db.removeStorageRentBoxes(boxIds.take(2))
    rentBoxIndexesInIndex(10) shouldBe Seq(2L)

    // repeated removal is a no-op; unknown box ids are ignored
    db.removeStorageRentBoxes(boxIds.take(2) :+ bytesToId(Array.fill(32)(42.toByte)))
    rentBoxIndexesInIndex(10) shouldBe Seq(2L)

    // an entry whose IndexedErgoBox is gone can not be located, so it is left in place
    db.removeExtra(Array(boxIds(2)))
    db.removeStorageRentBoxes(Seq(boxIds(2)))
    rentBoxIndexesInIndex(10) shouldBe Seq(2L)
  }

  property("rent scan stops at the creation-height cutoff and skips foreign keys") {
    val globalIndexes = Seq(100L, 200L, 300L)
    val heights = Seq(10, 20, 30)
    val rentRowIds = heights.zip(globalIndexes).map { case (h, gi) =>
      val box = testBox(1000000000L, Constants.TrueTree, h)
      val iEb = new IndexedErgoBox(h, None, None, None, box, gi)
      val srb = StorageRentBox(iEb)
      db.insertExtra(Array.empty, Array[ExtraIndex](iEb, srb))
      srb.id
    }
    // a foreign 32-byte key starting with the marker byte, sorting between the
    // height-20 and height-30 rent rows inside the marker namespace
    val foreignKey = Array.fill(32)(0.toByte)
    foreignKey(0) = StorageRentBox.KeyMarker
    foreignKey(4) = 25.toByte // between height 20 (0x14) and 30 (0x1e) at the height's last byte
    db.insertExtra(Array(foreignKey -> Array[Byte](1)), Array.empty)
    // keys past the rent namespace: a rent-shaped key with the next marker byte and one
    // far away - they must never leak into results (the scan stops at the marker change)
    val laterMarkerKey = StorageRentBox.key(15, 150L)
    laterMarkerKey(0) = (StorageRentBox.KeyMarker + 1).toByte
    val farKey = Array.fill(32)(0xff.toByte)
    db.insertExtra(Array(laterMarkerKey -> Array[Byte](1), farKey -> Array[Byte](1)), Array.empty)

    try {
      // the cutoff stops the scan at the first key in the namespace whose height
      // bytes pass it - nothing eligible can sort after such a key
      db.storageRentBoxesAtOrBefore(20, 10).map(_.globalIndex).toSeq shouldBe Seq(100L, 200L)
      // the foreign key is skipped over (not returned, and not truncating the scan)
      db.storageRentBoxesAtOrBefore(30, 10).map(_.globalIndex).toSeq shouldBe Seq(100L, 200L, 300L)
      // out-of-namespace keys never appear even with an unlimited cutoff; the trailing
      // row (globalIndex 2, height 1002) is the one the sibling property leaves in place
      db.storageRentBoxesAtOrBefore(Int.MaxValue, 10).map(_.globalIndex).toSeq shouldBe
        Seq(100L, 200L, 300L, 2L)
    } finally {
      // the spec's db is shared and persisted: leave no rows behind for other runs
      db.removeExtra((rentRowIds ++ Seq(foreignKey, laterMarkerKey, farKey).map(bytesToId)).toArray)
    }
  }

  property("recursive extra index deletion propagates a file failure") {
    val root = Files.createTempDirectory("extra-index-delete-failure")
    val sentinel = Files.createFile(root.resolve("sentinel"))

    val result = HistoryStorage.deleteRecursively(root, path => {
      if (path == sentinel) throw new IOException("injected deletion failure")
      Files.delete(path)
    })

    result shouldBe 'failure
    Files.exists(sentinel) shouldBe true
    Files.delete(sentinel)
    Files.delete(root)
  }

  property("extra serialization failure invalidates mutated cached objects") {
    import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.ergoBoxGenNoProp

    val indexedBox = new IndexedErgoBox(1, None, None, None, ergoBoxGenNoProp.sample.get, 0L)
    db.insertExtraTry(Array.empty, Array(indexedBox)).get
    val cachedBox = db.getExtraIndex(indexedBox.id).get.asInstanceOf[IndexedErgoBox]
    cachedBox.spendingHeightOpt = Some(2)
    val unsupported = new ExtraIndex {
      override def serializedId: Array[Byte] = Array.fill[Byte](32)(0x55.toByte)
    }

    db.insertExtraTry(Array.empty, Array[ExtraIndex](cachedBox, unsupported)) shouldBe 'failure
    db.getExtraIndex(indexedBox.id).get.asInstanceOf[IndexedErgoBox].spendingHeightOpt shouldBe None
    db.insertExtraTry(Array.empty, Array(cachedBox)).get
    db.getExtraIndex(indexedBox.id).get.asInstanceOf[IndexedErgoBox].spendingHeightOpt shouldBe Some(2)
  }

  property("extra rows and checkpoint metadata share one batch and preserve returned write failures") {
    import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.ergoBoxGenNoProp

    val root = Files.createTempDirectory("extra-index-batch-failure")
    val indexStore = LDBFactory.createKvDb(root.resolve("index").toString)
    val objectsStore = LDBFactory.createKvDb(root.resolve("objects").toString)
    val rawExtraDb = LDBFactory.factory.open(root.resolve("extra").toFile, new Options().createIfMissing(true))
    val writeError = new IOException("injected extra batch write failure")
    var failWrite = false
    var batchCount = 0
    val extraStore = new LDBKVStore(rawExtraDb) {
      override def update(keys: Array[Array[Byte]], values: Array[Array[Byte]], remove: Array[Array[Byte]]): Try[Unit] = {
        batchCount += 1
        if (failWrite) Failure(writeError) else super.update(keys, values, remove)
      }
    }
    val storage = new HistoryStorage(indexStore, objectsStore, extraStore, settings.cacheSettings)
    val checkpointKey = Algos.hash("test-extra-checkpoint".getBytes(CharsetName))
    val indexedBox = new IndexedErgoBox(1, None, None, None, ergoBoxGenNoProp.sample.get, 0L)

    try {
      storage.insertExtraTry(Array(checkpointKey -> Array[Byte](1)), Array(indexedBox)).get
      batchCount shouldBe 1
      val cached = storage.getExtraIndex(indexedBox.id).get.asInstanceOf[IndexedErgoBox]
      cached.spendingHeightOpt = Some(2)
      failWrite = true
      val result = storage.insertExtraTry(Array(checkpointKey -> Array[Byte](2)), Array(cached))
      result.failed.get shouldBe writeError
      batchCount shouldBe 2
      extraStore.get(checkpointKey).get.toSeq shouldBe Seq[Byte](1)
      storage.getExtraIndex(indexedBox.id).get.asInstanceOf[IndexedErgoBox].spendingHeightOpt shouldBe None

      failWrite = false
      storage.insertExtraTry(Array(checkpointKey -> Array[Byte](2)), Array(cached)).get
      batchCount shouldBe 3
      extraStore.get(checkpointKey).get.toSeq shouldBe Seq[Byte](2)
      storage.getExtraIndex(indexedBox.id).get.asInstanceOf[IndexedErgoBox].spendingHeightOpt shouldBe Some(2)

      failWrite = true
      storage.removeExtraTry(Array(indexedBox.id)).failed.get shouldBe writeError
      extraStore.get(indexedBox.serializedId).isDefined shouldBe true
      storage.getExtraIndex(indexedBox.id).isDefined shouldBe true
      failWrite = false
      storage.removeExtraTry(Array(indexedBox.id)).get
      storage.getExtraIndex(indexedBox.id) shouldBe None
    } finally storage.close()
  }

  property("an in-flight cache miss cannot restore stale data after a successful write") {
    import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.ergoBoxGenNoProp

    implicit val executionContext: ExecutionContext = ExecutionContext.global
    val root = Files.createTempDirectory("extra-index-cache-race")
    val oldValueRead = new CountDownLatch(1)
    val resumeOldRead = new CountDownLatch(1)
    val writerStarted = new CountDownLatch(1)
    val writeCommitted = new CountDownLatch(1)
    @volatile var pauseNextRead = false
    @volatile var observeWrite = false

    val options = new Options().createIfMissing(true)
    val indexStore = LDBFactory.createKvDb(root.resolve("index").toString)
    val objectsStore = LDBFactory.createKvDb(root.resolve("objects").toString)
    val rawExtraDb = LDBFactory.factory.open(root.resolve("extra").toFile, options)
    val extraStore = new LDBKVStore(rawExtraDb) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = {
        val value = super.get(key)
        if (pauseNextRead) {
          pauseNextRead = false
          oldValueRead.countDown()
          require(resumeOldRead.await(5, TimeUnit.SECONDS), "timed out waiting to resume cache-miss read")
        }
        value
      }

      override def update(toInsertKeys: Array[Array[Byte]],
                          toInsertValues: Array[Array[Byte]],
                          toRemove: Array[Array[Byte]]): Try[Unit] = {
        val result = super.update(toInsertKeys, toInsertValues, toRemove)
        if (observeWrite && result.isSuccess) writeCommitted.countDown()
        result
      }
    }
    val concurrentStorage = new HistoryStorage(indexStore, objectsStore, extraStore, settings.cacheSettings)

    try {
      val indexedBox = new IndexedErgoBox(1, None, None, None, ergoBoxGenNoProp.sample.get, 0L)
      concurrentStorage.insertExtraTry(Array.empty, Array(indexedBox)).get
      pauseNextRead = true
      val staleRead = Future(concurrentStorage.getExtraIndex(indexedBox.id).get.asInstanceOf[IndexedErgoBox])
      oldValueRead.await(5, TimeUnit.SECONDS) shouldBe true

      indexedBox.spendingHeightOpt = Some(2)
      observeWrite = true
      val write = Future {
        writerStarted.countDown()
        concurrentStorage.insertExtraTry(Array.empty, Array(indexedBox)).get
      }
      writerStarted.await(5, TimeUnit.SECONDS) shouldBe true
      val committedWhileReadWasPaused = writeCommitted.await(200, TimeUnit.MILLISECONDS)
      resumeOldRead.countDown()

      Await.result(staleRead, 5.seconds).spendingHeightOpt shouldBe None
      Await.result(write, 5.seconds)
      committedWhileReadWasPaused shouldBe false
      concurrentStorage.getExtraIndex(indexedBox.id).get.asInstanceOf[IndexedErgoBox].spendingHeightOpt shouldBe Some(2)
    } finally {
      resumeOldRead.countDown()
      concurrentStorage.close()
    }
  }

  property("paired insert rolls back objectsStore and avoids cache pollution when indexStore write fails") {
    val root = Files.createTempDirectory("history-atomic-insert-rollback")
    val options = new Options().createIfMissing(true)
    val rawIndexDb = LDBFactory.factory.open(root.resolve("index").toFile, options)
    val indexFailure = new IOException("injected index batch write failure")
    var failIndexWrite = false
    val indexStore = new LDBKVStore(rawIndexDb) {
      override def update(keys: Array[Array[Byte]], values: Array[Array[Byte]], remove: Array[Array[Byte]]): Try[Unit] = {
        if (failIndexWrite) Failure(indexFailure) else super.update(keys, values, remove)
      }
    }
    val objectsStore = LDBFactory.createKvDb(root.resolve("objects").toString)
    val extraStore = LDBFactory.createKvDb(root.resolve("extra").toString)
    val journalPath = root.resolve(HistoryStorage.PendingJournalFileName)
    val storage = new HistoryStorage(indexStore, objectsStore, extraStore, settings.cacheSettings, Some(journalPath))

    try {
      val header = defaultHeaderGen.sample.get
      val priorProof = ADProofs(bytesToId(Array.fill(32)(11.toByte)), SerializedAdProof @@ Array[Byte](1, 2, 3))
      val overwrittenTarget = ADProofs(bytesToId(Array.fill(32)(12.toByte)), SerializedAdProof @@ Array[Byte](9, 8, 7))
      val existingKey = ByteArrayWrapper(Algos.hash("existing-idx".getBytes(CharsetName)))
      val removableKey = ByteArrayWrapper(Algos.hash("removable-idx".getBytes(CharsetName)))
      val newKey = ByteArrayWrapper(Algos.hash("new-idx".getBytes(CharsetName)))

      // Seed baseline state: overwrittenTarget's key initially holds priorProof's bytes, plus two index entries.
      storage.insert(overwrittenTarget.serializedId, HistoryModifierSerializer.toBytes(priorProof)).get
      storage.insert(
        Array(existingKey -> Array[Byte](10), removableKey -> Array[Byte](20)),
        Array.empty[BlockSection]
      ).get
      storage.modifierById(overwrittenTarget.id) shouldBe Some(priorProof)
      storage.getIndex(existingKey).map(_.toSeq) shouldBe Some(Seq[Byte](10))
      storage.getIndex(removableKey).map(_.toSeq) shouldBe Some(Seq[Byte](20))

      failIndexWrite = true
      val failedRes = storage.insert(
        Array(existingKey -> Array[Byte](99), newKey -> Array[Byte](42)),
        Array[BlockSection](overwrittenTarget, header),
        Array(removableKey)
      )
      failedRes.failed.get shouldBe indexFailure

      // Neither new nor overwritten objects may remain committed or cached.
      storage.contains(header.id) shouldBe false
      storage.modifierById(header.id) shouldBe None
      objectsStore.get(header.serializedId) shouldBe None
      storage.modifierById(overwrittenTarget.id) shouldBe Some(priorProof)
      objectsStore.get(overwrittenTarget.serializedId).map(_.toSeq) shouldBe
        Some(HistoryModifierSerializer.toBytes(priorProof).toSeq)

      // Indexes remain at their pre-batch values.
      storage.getIndex(existingKey).map(_.toSeq) shouldBe Some(Seq[Byte](10))
      storage.getIndex(removableKey).map(_.toSeq) shouldBe Some(Seq[Byte](20))
      storage.getIndex(newKey) shouldBe None

      // Subsequent normal write succeeds cleanly.
      failIndexWrite = false
      storage.insert(
        Array(existingKey -> Array[Byte](99), newKey -> Array[Byte](42)),
        Array[BlockSection](overwrittenTarget, header),
        Array(removableKey)
      ) shouldBe Success(())
      storage.contains(header.id) shouldBe true
      storage.modifierById(header.id) shouldBe Some(header)
      storage.modifierById(overwrittenTarget.id) shouldBe Some(overwrittenTarget)
      storage.getIndex(existingKey).map(_.toSeq) shouldBe Some(Seq[Byte](99))
      storage.getIndex(newKey).map(_.toSeq) shouldBe Some(Seq[Byte](42))
      storage.getIndex(removableKey) shouldBe None
    } finally {
      storage.close()
    }
    Files.exists(journalPath) shouldBe false
  }

  property("modifier serialization failure aborts paired insert before touching stores or caches") {
    val root = Files.createTempDirectory("history-serialization-abort")
    val storageSettings = settings.copy(directory = root.toString)
    val storage = HistoryStorage(storageSettings)
    try {
      val validHeader = defaultHeaderGen.sample.get
      val malformedProof = ADProofs(bytesToId(Array.fill(32)(7.toByte)), null)
      val indexKey = ByteArrayWrapper(Algos.hash("serialization-abort-idx".getBytes(CharsetName)))

      val res = storage.insert(
        Array(indexKey -> Array[Byte](1, 2)),
        Array[BlockSection](validHeader, malformedProof)
      )
      res.isFailure shouldBe true
      storage.contains(validHeader.id) shouldBe false
      storage.modifierById(validHeader.id) shouldBe None
      storage.getIndex(indexKey) shouldBe None
    } finally {
      storage.close()
    }
  }

  property("journaled paired remove restores objectsStore when indexStore remove fails") {
    val root = Files.createTempDirectory("history-atomic-remove-rollback")
    val options = new Options().createIfMissing(true)
    val rawIndexDb = LDBFactory.factory.open(root.resolve("index").toFile, options)
    val removeError = new IOException("injected index remove failure")
    var failIndexRemove = false
    val indexStore = new LDBKVStore(rawIndexDb) {
      override def remove(keys: Array[Array[Byte]]): Try[Unit] = {
        if (failIndexRemove) Failure(removeError) else super.remove(keys)
      }
    }
    val objectsStore = LDBFactory.createKvDb(root.resolve("objects").toString)
    val extraStore = LDBFactory.createKvDb(root.resolve("extra").toString)
    val journalPath = root.resolve(HistoryStorage.PendingJournalFileName)
    val storage = new HistoryStorage(indexStore, objectsStore, extraStore, settings.cacheSettings, Some(journalPath))

    try {
      val header = defaultHeaderGen.sample.get
      val indexKey = ByteArrayWrapper(Algos.hash("remove-rollback-idx".getBytes(CharsetName)))
      storage.insert(Array(indexKey -> Array[Byte](7)), Array[BlockSection](header)) shouldBe Success(())
      storage.contains(header.id) shouldBe true

      failIndexRemove = true
      storage.remove(Array(indexKey), Array(header.id)) shouldBe Failure(removeError)

      storage.contains(header.id) shouldBe true
      storage.modifierById(header.id) shouldBe Some(header)
      storage.getIndex(indexKey).map(_.toSeq) shouldBe Some(Seq[Byte](7))

      failIndexRemove = false
      storage.remove(Array(indexKey), Array(header.id)) shouldBe Success(())
      storage.contains(header.id) shouldBe false
      storage.modifierById(header.id) shouldBe None
      storage.getIndex(indexKey) shouldBe None
    } finally {
      storage.close()
    }
  }

  property("startup journal recovery repairs interrupted paired insert and remove batches across restart") {
    val root = Files.createTempDirectory("history-crash-journal-recovery")
    val storageSettings = settings.copy(directory = root.toString)
    val historyDir = root.resolve("history")
    val journalPath = historyDir.resolve(HistoryStorage.PendingJournalFileName)

    val baselineProof = ADProofs(bytesToId(Array.fill(32)(21.toByte)), SerializedAdProof @@ Array[Byte](3, 4, 5))
    val overwriteTarget = ADProofs(bytesToId(Array.fill(32)(22.toByte)), SerializedAdProof @@ Array[Byte](6, 7, 8))
    val crashedHeader = defaultHeaderGen.sample.get
    val baseIdx = ByteArrayWrapper(Algos.hash("crash-base-idx".getBytes(CharsetName)))
    val crashedIdx = ByteArrayWrapper(Algos.hash("crash-new-idx".getBytes(CharsetName)))

    // Step 1: establish clean committed baseline.
    val initial = HistoryStorage(storageSettings)
    initial.insert(overwriteTarget.serializedId, HistoryModifierSerializer.toBytes(baselineProof)).get
    initial.insert(Array(baseIdx -> Array[Byte](11)), Array.empty[BlockSection]).get
    initial.close()

    // Step 2: simulate crash right after objectsStore.insert commits to LevelDB before indexStore updates
    // (both indexStore write and in-process objectsStore rollback fail due to abrupt termination).
    val options = new Options().createIfMissing(true)
    val rawIdx1 = LDBFactory.factory.open(historyDir.resolve("index").toFile, options)
    val rawObj1 = LDBFactory.factory.open(historyDir.resolve("objects").toFile, options)
    val extra1 = LDBFactory.createKvDb(historyDir.resolve("extra").toString)
    var blockObjectRollback = false
    val crashingIndexStore = new LDBKVStore(rawIdx1) {
      override def update(keys: Array[Array[Byte]], values: Array[Array[Byte]], remove: Array[Array[Byte]]): Try[Unit] = {
        blockObjectRollback = true
        Failure(new IOException("simulated process crash before indexStore write"))
      }
    }
    val crashingObjectsStore = new LDBKVStore(rawObj1) {
      override def update(keys: Array[Array[Byte]], values: Array[Array[Byte]], remove: Array[Array[Byte]]): Try[Unit] = {
        if (blockObjectRollback) Failure(new IOException("simulated process crash before rollback"))
        else super.update(keys, values, remove)
      }
    }
    val crashingStorage = new HistoryStorage(
      crashingIndexStore,
      crashingObjectsStore,
      extra1,
      storageSettings.cacheSettings,
      Some(journalPath)
    )
    crashingStorage.insert(
      Array(baseIdx -> Array[Byte](88), crashedIdx -> Array[Byte](99)),
      Array[BlockSection](overwriteTarget, crashedHeader)
    ).isFailure shouldBe true

    // Confirm raw LevelDB state is genuinely torn before startup recovery runs, and journal was retained.
    Option(rawObj1.get(crashedHeader.serializedId)).isDefined shouldBe true
    Option(rawIdx1.get(crashedIdx.data)) shouldBe None
    crashingStorage.close()
    Files.exists(journalPath) shouldBe true

    // Step 3: reopen via HistoryStorage(storageSettings) and verify automatic journal recovery.
    val recovered = HistoryStorage(storageSettings)
    try {
      Files.exists(journalPath) shouldBe false
      recovered.contains(crashedHeader.id) shouldBe false
      recovered.modifierById(crashedHeader.id) shouldBe None
      recovered.modifierById(overwriteTarget.id) shouldBe Some(baselineProof)
      recovered.getIndex(baseIdx).map(_.toSeq) shouldBe Some(Seq[Byte](11))
      recovered.getIndex(crashedIdx) shouldBe None
    } finally {
      recovered.close()
    }

    // Step 4: simulate an interrupted paired remove where objectsStore removed overwriteTarget before crash.
    val rawIdx2 = LDBFactory.factory.open(historyDir.resolve("index").toFile, options)
    val rawObj2 = LDBFactory.factory.open(historyDir.resolve("objects").toFile, options)
    val extra2 = LDBFactory.createKvDb(historyDir.resolve("extra").toString)
    var blockRemoveRollback = false
    val crashingRemoveIdx = new LDBKVStore(rawIdx2) {
      override def remove(keys: Array[Array[Byte]]): Try[Unit] = {
        blockRemoveRollback = true
        Failure(new IOException("simulated crash during indexStore remove"))
      }
    }
    val crashingRemoveObj = new LDBKVStore(rawObj2) {
      override def insert(keys: Array[Array[Byte]], values: Array[Array[Byte]]): Try[Unit] = {
        if (blockRemoveRollback) Failure(new IOException("simulated crash before remove rollback"))
        else super.insert(keys, values)
      }
    }
    val crashingRemoveStorage = new HistoryStorage(
      crashingRemoveIdx,
      crashingRemoveObj,
      extra2,
      storageSettings.cacheSettings,
      Some(journalPath)
    )
    crashingRemoveStorage.remove(Array(baseIdx), Array(overwriteTarget.id)).isFailure shouldBe true
    Option(rawObj2.get(overwriteTarget.serializedId)) shouldBe None
    Option(rawIdx2.get(baseIdx.data)).isDefined shouldBe true
    crashingRemoveStorage.close()
    Files.exists(journalPath) shouldBe true

    val recoveredAfterRemove = HistoryStorage(storageSettings)
    try {
      Files.exists(journalPath) shouldBe false
      recoveredAfterRemove.contains(overwriteTarget.id) shouldBe true
      recoveredAfterRemove.modifierById(overwriteTarget.id) shouldBe Some(baselineProof)
      recoveredAfterRemove.getIndex(baseIdx).map(_.toSeq) shouldBe Some(Seq[Byte](11))
    } finally {
      recoveredAfterRemove.close()
    }
  }

  property("concurrent readers cannot observe objectsStore committed ahead of indexStore during paired insert") {
    implicit val executionContext: ExecutionContext = ExecutionContext.global
    val root = Files.createTempDirectory("history-concurrent-atomic-visibility")
    val options = new Options().createIfMissing(true)
    val objectsWritten = new CountDownLatch(1)
    val allowIndexWrite = new CountDownLatch(1)
    val readerStarted = new CountDownLatch(1)
    @volatile var pauseOnIndexInsert = false

    val rawIndexDb = LDBFactory.factory.open(root.resolve("index").toFile, options)
    val indexStore = new LDBKVStore(rawIndexDb) {
      override def update(keys: Array[Array[Byte]], values: Array[Array[Byte]], remove: Array[Array[Byte]]): Try[Unit] = {
        if (pauseOnIndexInsert) {
          pauseOnIndexInsert = false
          objectsWritten.countDown()
          require(allowIndexWrite.await(5, TimeUnit.SECONDS), "timed out waiting to finish indexStore write")
        }
        super.update(keys, values, remove)
      }
    }
    val objectsStore = LDBFactory.createKvDb(root.resolve("objects").toString)
    val extraStore = LDBFactory.createKvDb(root.resolve("extra").toString)
    val storage = new HistoryStorage(
      indexStore,
      objectsStore,
      extraStore,
      settings.cacheSettings,
      Some(root.resolve(HistoryStorage.PendingJournalFileName))
    )

    try {
      val header = defaultHeaderGen.sample.get
      val idxKey = ByteArrayWrapper(Algos.hash("atomic-visibility-idx".getBytes(CharsetName)))
      pauseOnIndexInsert = true

      val writer = Future {
        storage.insert(Array(idxKey -> Array[Byte](1)), Array[BlockSection](header)).get
      }
      objectsWritten.await(5, TimeUnit.SECONDS) shouldBe true

      val reader = Future {
        readerStarted.countDown()
        val hasMod = storage.contains(header.id)
        val hasIdx = storage.getIndex(idxKey).isDefined
        (hasMod, hasIdx)
      }
      readerStarted.await(5, TimeUnit.SECONDS) shouldBe true
      Thread.sleep(50)
      reader.isCompleted shouldBe false

      allowIndexWrite.countDown()
      Await.result(writer, 5.seconds)
      Await.result(reader, 5.seconds) shouldBe ((true, true))
    } finally {
      allowIndexWrite.countDown()
      storage.close()
    }
  }

}
