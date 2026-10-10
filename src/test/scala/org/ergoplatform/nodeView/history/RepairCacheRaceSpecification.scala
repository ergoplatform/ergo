package org.ergoplatform.nodeView.history

import com.google.common.primitives.Ints
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.GenesisHeight
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockSectionProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.{ErgoCorePropertyTest, ErgoNodeTestConstants}
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain, nextHeader}
import org.iq80.leveldb.Options
import scorex.db.{ByteArrayWrapper, LDBFactory, LDBKVStore}
import scorex.util.{ModifierId, bytesToId, idToBytes}

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Arrays
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

class RepairCacheRaceSpecification extends ErgoCorePropertyTest {
  private def queuedOnIndexLock(thread: Thread, lockType: String): Boolean =
    thread != null && thread.getState == Thread.State.WAITING &&
      thread.getStackTrace.exists(frame =>
        frame.getClassName.contains(s"ReentrantReadWriteLock$$$lockType") &&
          frame.getMethodName == "lock"
      )

  property("a read racing repair cannot restore a removed height row") {
    implicit val executionContext: ExecutionContext = ExecutionContext.global
    val root = Files.createTempDirectory("history-repair-cache-race")
    val taskSettings = ErgoNodeTestConstants.initSettings.copy(
      directory = root.toString,
      nodeSettings = ErgoNodeTestConstants.initSettings.nodeSettings.copy(
        stateType = StateType.Digest,
        verifyTransactions = true,
        blocksToKeep = 100,
        extraIndex = false
      )
    )
    val dbRoot = Files.createDirectories(root.resolve("history"))
    val oldRowRead = new CountDownLatch(1)
    val resumeOldRead = new CountDownLatch(1)
    val heightRowRemoved = new CountDownLatch(1)
    val pauseNextHeightRead = new AtomicBoolean(false)
    val pausedReaderThread = new AtomicReference[Thread]()
    var targetHeightKey: ByteArrayWrapper = null
    var repairThread: Thread = null

    val rawIndexDb = LDBFactory.factory.open(dbRoot.resolve("index").toFile, new Options().createIfMissing(true))
    val indexStore = new LDBKVStore(rawIndexDb) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = {
        val value = super.get(key)
        if (targetHeightKey != null && Arrays.equals(key, targetHeightKey.data) &&
            pauseNextHeightRead.compareAndSet(true, false)) {
          pausedReaderThread.set(Thread.currentThread())
          oldRowRead.countDown()
          require(resumeOldRead.await(10, TimeUnit.SECONDS), "timed out waiting to resume old row read")
        }
        value
      }

      override def remove(keys: Array[Array[Byte]]): Try[Unit] = {
        val result = super.remove(keys)
        if (result.isSuccess && targetHeightKey != null &&
            keys.exists(key => Arrays.equals(key, targetHeightKey.data))) {
          heightRowRemoved.countDown()
        }
        result
      }
    }
    val objectsStore = LDBFactory.createKvDb(dbRoot.resolve("objects").toString)
    val extraStore = LDBFactory.createKvDb(dbRoot.resolve("extra").toString)
    val storage = new HistoryStorage(indexStore, objectsStore, extraStore, taskSettings.cacheSettings)
    val history = new ErgoHistory with FullBlockSectionProcessor {
      override protected val settings: ErgoSettings = taskSettings
      override protected[history] val historyStorage: HistoryStorage = storage
      override val powScheme: AutolykosPowScheme = chainSettings.powScheme
    }

    try {
      history.writeMinimalFullBlockHeight(GenesisHeight)
      history.isHeadersChainSyncedVar = true
      val fullTip = genChain(1, history).head
      applyChain(history, Seq(fullTip))
      history.bestHeaderIdOpt shouldBe Some(fullTip.id)
      history.bestFullBlockIdOpt shouldBe Some(fullTip.id)

      val nextHeight = fullTip.height + 1
      targetHeightKey = ByteArrayWrapper(Algos.hash(Ints.toByteArray(nextHeight)))
      val interval = history.difficultyCalculator.desiredInterval.toMillis
      val nBits = DifficultySerializer.encodeCompactBits(history.requiredDifficultyAfter(fullTip.header))
      val removedHeader = nextHeader(
        Some(fullTip.header), history.difficultyCalculator,
        tsOpt = Some(fullTip.header.timestamp + interval),
        diffBitsOpt = Some(nBits), useRealTs = true
      )
      val replacement = nextHeader(
        Some(fullTip.header), history.difficultyCalculator,
        tsOpt = Some(fullTip.header.timestamp + 2 * interval),
        diffBitsOpt = Some(nBits), useRealTs = true
      )
      removedHeader.id should not be replacement.id

      val invalidKey = ByteArrayWrapper(Algos.hash(
        "validity".getBytes(StandardCharsets.UTF_8) ++ idToBytes(removedHeader.id)
      ))
      objectsStore.insert(removedHeader.serializedId, HistoryModifierSerializer.toBytes(removedHeader)).get
      indexStore.insert(invalidKey.data, Array(0.toByte)).get
      indexStore.insert(targetHeightKey.data, idToBytes(removedHeader.id)).get
      history.headersHeight shouldBe fullTip.height
      history.bestFullBlockOpt.map(_.id) shouldBe Some(fullTip.id)

      pauseNextHeightRead.set(true)
      val oldRead = Future(history.headerIdsAtHeight(nextHeight))
      oldRowRead.await(10, TimeUnit.SECONDS) shouldBe true

      val repairDone = new CountDownLatch(1)
      val repairResult = new AtomicReference[Try[Boolean]]()
      repairThread = new Thread(new Runnable {
        override def run(): Unit = {
          try repairResult.set(Try(ErgoHistory.repairIfNeeded(history)))
          finally repairDone.countDown()
        }
      }, "history-repair-race")
      repairThread.start()

      def repairBlockedOnReader: Boolean = {
        pausedReaderThread.get() != null &&
          queuedOnIndexLock(repairThread, "WriteLock")
      }
      val deadline = 10.seconds.fromNow
      var removedBeforeResume = false
      var repairBlocked = false
      while (!removedBeforeResume && !repairBlocked && deadline.hasTimeLeft()) {
        removedBeforeResume = heightRowRemoved.await(10, TimeUnit.MILLISECONDS)
        repairBlocked = repairBlockedOnReader
      }
      withClue("repair must remove the row or block on the paused reader: ") {
        (removedBeforeResume || repairBlocked) shouldBe true
      }
      if (removedBeforeResume) repairDone.await(10, TimeUnit.SECONDS) shouldBe true

      resumeOldRead.countDown()
      repairDone.await(10, TimeUnit.SECONDS) shouldBe true
      repairResult.get().get shouldBe true
      indexStore.get(targetHeightKey.data) shouldBe None
      Await.result(oldRead, 10.seconds) shouldBe Seq(removedHeader.id)
      val cachedAfterRepair = history.headerIdsAtHeight(nextHeight)
      history.append(replacement).get

      def rawIds: Seq[ModifierId] = indexStore.get(targetHeightKey.data)
        .toSeq.flatMap(_.grouped(32).map(bytesToId))
      withClue(s"cached after repair: $cachedAfterRepair; raw after append: $rawIds: ") {
        rawIds shouldBe Seq(replacement.id)
        history.headerIdsAtHeight(nextHeight) shouldBe Seq(replacement.id)
      }
    } finally {
      resumeOldRead.countDown()
      if (repairThread != null) repairThread.join(10000)
      history.closeStorage()
    }
  }

  property("a cached hit stays available while an index removal finishes") {
    val root = Files.createTempDirectory("history-index-cache-hit-race")
    val dbRoot = Files.createDirectories(root.resolve("history"))
    val indexKey = ByteArrayWrapper(Algos.hash("height row".getBytes(StandardCharsets.UTF_8)))
    val row = Array(1.toByte)
    val indexRemoved = new CountDownLatch(1)
    val resumeRemoval = new CountDownLatch(1)
    val pauseAfterRemove = new AtomicBoolean(false)

    val rawIndexDb = LDBFactory.factory.open(dbRoot.resolve("index").toFile, new Options().createIfMissing(true))
    val indexStore = new LDBKVStore(rawIndexDb) {
      override def remove(keys: Array[Array[Byte]]): Try[Unit] = {
        val result = super.remove(keys)
        if (result.isSuccess && pauseAfterRemove.compareAndSet(true, false)) {
          indexRemoved.countDown()
          require(resumeRemoval.await(10, TimeUnit.SECONDS), "timed out waiting to finish index removal")
        }
        result
      }
    }
    val objectsStore = LDBFactory.createKvDb(dbRoot.resolve("objects").toString)
    val extraStore = LDBFactory.createKvDb(dbRoot.resolve("extra").toString)
    val storage = new HistoryStorage(indexStore, objectsStore, extraStore,
      ErgoNodeTestConstants.initSettings.cacheSettings)
    var removalThread: Thread = null
    var readerThread: Thread = null

    try {
      indexStore.insert(indexKey.data, row).get
      storage.getIndex(indexKey).map(_.toSeq) shouldBe Some(row.toSeq)
      pauseAfterRemove.set(true)
      val removalResult = new AtomicReference[Try[Unit]]()
      val removalDone = new CountDownLatch(1)
      removalThread = new Thread(new Runnable {
        override def run(): Unit = {
          try removalResult.set(Try(storage.remove(Array(indexKey), Array.empty[ModifierId])).flatten)
          finally removalDone.countDown()
        }
      }, "history-index-removal")
      removalThread.start()
      indexRemoved.await(10, TimeUnit.SECONDS) shouldBe true

      val readResult = new AtomicReference[Try[Option[Array[Byte]]]]()
      val readDone = new CountDownLatch(1)
      readerThread = new Thread(new Runnable {
        override def run(): Unit = {
          try readResult.set(Try(storage.getIndex(indexKey)))
          finally readDone.countDown()
        }
      }, "history-index-cached-reader")
      readerThread.start()
      withClue("a cached hit should finish before the removal returns: ") {
        readDone.await(10, TimeUnit.SECONDS) shouldBe true
        readResult.get().get.map(_.toSeq) shouldBe Some(row.toSeq)
      }

      resumeRemoval.countDown()
      removalDone.await(10, TimeUnit.SECONDS) shouldBe true
      removalResult.get().get
      storage.getIndex(indexKey) shouldBe None
    } finally {
      resumeRemoval.countDown()
      if (removalThread != null) removalThread.join(10000)
      if (readerThread != null) readerThread.join(10000)
      require(removalThread == null || !removalThread.isAlive, "removal thread did not finish")
      require(readerThread == null || !readerThread.isAlive, "reader thread did not finish")
      storage.close()
    }
  }
}
