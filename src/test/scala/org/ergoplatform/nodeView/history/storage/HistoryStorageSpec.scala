package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, IndexedErgoBox}
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.db.{ByteArrayWrapper, LDBFactory, LDBKVStore}
import scorex.util.{ModifierId, idToBytes}

import java.io.IOException
import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import org.iq80.leveldb.Options
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

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

}
