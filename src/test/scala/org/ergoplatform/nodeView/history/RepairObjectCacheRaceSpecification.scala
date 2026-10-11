package org.ergoplatform.nodeView.history

import com.google.common.primitives.Ints
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.GenesisHeight
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockSectionProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.{ErgoCorePropertyTest, ErgoNodeTestConstants}
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain, nextHeader}
import org.iq80.leveldb.Options
import scorex.db.{ByteArrayWrapper, LDBFactory, LDBKVStore}
import scorex.util.{ModifierId, idToBytes}

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Arrays
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

class RepairObjectCacheRaceSpecification extends ErgoCorePropertyTest {

  property("a raw header read cannot repopulate the cache after live repair removes it") {
    implicit val executionContext: ExecutionContext = ExecutionContext.global

    val root = Files.createTempDirectory("history-repair-object-cache-race")
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
    val staleReadStarted = new CountDownLatch(1)
    val resumeStaleRead = new CountDownLatch(1)
    val repairStarted = new CountDownLatch(1)
    val removedFromDb = new CountDownLatch(1)
    val pauseNextRead = new AtomicBoolean(false)
    val targetReads = new AtomicInteger(0)
    var targetId: ModifierId = null
    var repairThread: Thread = null

    val indexStore = LDBFactory.createKvDb(dbRoot.resolve("index").toString)
    val rawObjectsDb = LDBFactory.factory.open(dbRoot.resolve("objects").toFile,
      new Options().createIfMissing(true))
    val objectsStore = new LDBKVStore(rawObjectsDb) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = {
        val value = super.get(key)
        if (targetId != null && Arrays.equals(key, idToBytes(targetId)) &&
            pauseNextRead.get() && targetReads.incrementAndGet() == 2 &&
            pauseNextRead.compareAndSet(true, false)) {
          staleReadStarted.countDown()
          require(resumeStaleRead.await(10, TimeUnit.SECONDS), "timed out waiting to resume stale read")
        }
        value
      }

      override def remove(keys: Array[Array[Byte]]): Try[Unit] = {
        val result = super.remove(keys)
        if (result.isSuccess && targetId != null &&
            keys.exists(key => Arrays.equals(key, idToBytes(targetId)))) {
          removedFromDb.countDown()
        }
        result
      }
    }
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

      val height = fullTip.height + 1
      val interval = history.difficultyCalculator.desiredInterval.toMillis
      val nBits = DifficultySerializer.encodeCompactBits(history.requiredDifficultyAfter(fullTip.header))
      val removedHeader = nextHeader(
        Some(fullTip.header), history.difficultyCalculator,
        tsOpt = Some(fullTip.header.timestamp + interval),
        diffBitsOpt = Some(nBits), useRealTs = true
      )
      targetId = removedHeader.id
      val invalidKey = ByteArrayWrapper(Algos.hash(
        "validity".getBytes(StandardCharsets.UTF_8) ++ idToBytes(targetId)
      ))
      val heightKey = ByteArrayWrapper(Algos.hash(Ints.toByteArray(height)))
      objectsStore.insert(removedHeader.serializedId,
        HistoryModifierSerializer.toBytes(removedHeader)).get
      indexStore.insert(heightKey.data, idToBytes(targetId)).get
      history.headerIdsAtHeight(height) shouldBe Seq(targetId)

      pauseNextRead.set(true)
      val staleRead = Future(history.typedModifierById[Header](targetId))
      staleReadStarted.await(10, TimeUnit.SECONDS) shouldBe true
      indexStore.insert(invalidKey.data, Array(0.toByte)).get

      val repairDone = new CountDownLatch(1)
      val repairResult = new AtomicReference[Try[Boolean]]()
      repairThread = new Thread(new Runnable {
        override def run(): Unit = {
          repairStarted.countDown()
          try repairResult.set(Try(ErgoHistory.repairIfNeeded(history)))
          finally repairDone.countDown()
        }
      }, "history-repair-object-cache-race")
      repairThread.start()
      repairStarted.await(10, TimeUnit.SECONDS) shouldBe true
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10)
      while (repairThread.getState != Thread.State.BLOCKED && System.nanoTime() < deadline) {
        Thread.sleep(10)
      }
      repairThread.getState shouldBe Thread.State.BLOCKED
      // Repair waits on HistoryStorage while the database read and cache fill are in progress.
      removedFromDb.await(1, TimeUnit.SECONDS) shouldBe false
      resumeStaleRead.countDown()
      Await.result(staleRead, 10.seconds) shouldBe Some(removedHeader)
      removedFromDb.await(10, TimeUnit.SECONDS) shouldBe true
      repairDone.await(10, TimeUnit.SECONDS) shouldBe true
      repairResult.get().get shouldBe true
      objectsStore.get(idToBytes(targetId)) shouldBe None
      history.typedModifierById[Header](targetId) shouldBe None
    } finally {
      resumeStaleRead.countDown()
      if (repairThread != null) repairThread.join(10000)
      history.closeStorage()
    }
  }
}
