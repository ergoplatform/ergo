package org.ergoplatform.nodeView.history

import ch.qos.logback.classic.{Level, Logger => LogbackLogger}
import ch.qos.logback.classic.spi.{ILoggingEvent, ThrowableProxy}
import ch.qos.logback.core.AppenderBase
import com.google.common.primitives.Ints
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{ChainIsStuck, ChainProgress}
import org.ergoplatform.nodeView.history.extra.ExtraIndex
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{RentIndexEnabledKey, SchemaVersionKey}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockSectionProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.initSettings
import org.ergoplatform.utils.generators.ErgoCoreGenerators.{defaultHeaderGen, randomADProofsGen}
import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.invalidErgoTransactionGen
import org.slf4j.LoggerFactory
import scorex.db.ByteArrayWrapper
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class HistoryRepairSpec extends ErgoCorePropertyTest {
  private val testSettings = initSettings.copy(nodeSettings = initSettings.nodeSettings.copy(
    stateType = StateType.Utxo, verifyTransactions = true, extraIndex = false))
  private val bootstrapSettings = testSettings.copy(nodeSettings = testSettings.nodeSettings.copy(
    utxoSettings = testSettings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))

  private def captureHistoryErrors[A](body: => A): (A, Vector[ILoggingEvent]) = {
    val logger = LoggerFactory.getLogger(classOf[ErgoHistory].getPackage.getName).asInstanceOf[LogbackLogger]
    val previousLevel = logger.getLevel
    val events = mutable.ArrayBuffer.empty[ILoggingEvent]
    val appender = new AppenderBase[ILoggingEvent] {
      override def append(event: ILoggingEvent): Unit = {
        if (event.getLevel == Level.ERROR) events += event
      }
    }
    appender.setContext(logger.getLoggerContext)
    appender.start()
    logger.setLevel(Level.ERROR)
    logger.addAppender(appender)
    try {
      val result = body
      (result, events.toVector)
    } finally {
      logger.detachAppender(appender)
      logger.setLevel(previousLevel)
      appender.stop()
    }
  }

  // Storage-return failures and pre-return throws isolate each caller join.
  private class TestStorage extends HistoryStorage(null, null, null, testSettings.cacheSettings) {
    val indexes: mutable.Map[ByteArrayWrapper, Array[Byte]] = mutable.Map.empty
    val objects: mutable.Map[ModifierId, BlockSection] = mutable.Map.empty
    val removals: mutable.Buffer[(Seq[ByteArrayWrapper], Seq[ModifierId])] = mutable.Buffer.empty
    val failure = new IllegalStateException("history removal sentinel")
    val readFailure = new IllegalStateException("history read sentinel")
    var failAt: Int = -1
    var throwAt: Int = -1
    var failAfterObjectsAt: Int = -1
    var throwOnReadId: Option[ModifierId] = None
    var throwOnReadAt: Int = -1
    var targetedReads: Int = 0
    var throwOnIndexKey: Option[ByteArrayWrapper] = None
    var targetedIndexReads: Int = 0
    var closed: Boolean = false
    var closeCount: Int = 0
    var closeFailure: Option[RuntimeException] = None
    override def close(): Unit = {
      closed = true
      closeCount += 1
      closeFailure.foreach(error => throw error)
    }

    override def getIndex(key: ByteArrayWrapper): Option[Array[Byte]] = {
      if (throwOnIndexKey.contains(key)) {
        targetedIndexReads += 1
        throw readFailure
      }
      indexes.get(key)
    }
    override def modifierById(id: ModifierId): Option[BlockSection] = {
      if (throwOnReadId.contains(id)) {
        targetedReads += 1
        if (targetedReads == throwOnReadAt) throw readFailure
      }
      objects.get(id)
    }
    override def contains(id: ModifierId): Boolean = objects.contains(id)
    override def remove(keys: Array[ByteArrayWrapper], ids: Array[ModifierId]): Try[Unit] = {
      removals += ((keys.toSeq, ids.toSeq))
      if (removals.size == throwAt) throw failure
      if (removals.size == failAt) Failure(failure)
      else if (removals.size == failAfterObjectsAt) {
        ids.foreach(objects.remove)
        Failure(failure)
      }
      else {
        keys.foreach(indexes.remove)
        ids.foreach(objects.remove)
        Success(())
      }
    }
  }

  private class Fixture(stateType: StateType = StateType.Utxo,
                        verifyTransactions: Boolean = true,
                        freshTip: Boolean = false) {
    val fixtureSettings: ErgoSettings = testSettings.copy(nodeSettings = testSettings.nodeSettings.copy(
      stateType = stateType, verifyTransactions = verifyTransactions))
    val db = new TestStorage
    val history: ErgoHistory = new ErgoHistory with FullBlockSectionProcessor {
      override protected val settings: ErgoSettings = fixtureSettings
      override protected[history] val historyStorage: HistoryStorage = db
      override val powScheme: AutolykosPowScheme = testSettings.chainSettings.powScheme
    }
    val seed: Header = defaultHeaderGen.sample.get.copy(height = 10)
    val txs = BlockTransactions(seed.id, seed.version, Seq(invalidErgoTransactionGen.sample.get))
    val ext = Extension(seed.id, Seq.empty)
    val proofs = randomADProofsGen.sample.get
    val tip: Header = seed.copy(transactionsRoot = txs.digest, extensionRoot = ext.digest,
      ADProofsRoot = proofs.digest, timestamp = if (freshTip) System.currentTimeMillis() else seed.timestamp)
    val children: Seq[Header] = (1 to 2).map(i => tip.copy(
      parentId = tip.id, height = tip.height + 1, timestamp = tip.timestamp + i))
    val heightKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(tip.height + 1)))

    db.objects += tip.id -> tip
    db.objects += tip.transactionsId -> txs.copy(headerId = tip.id)
    db.objects += tip.extensionId -> ext.copy(headerId = tip.id)
    if (stateType.requireProofs) db.objects += tip.ADProofsId -> proofs.copy(headerId = tip.id)
    children.foreach { h =>
      db.objects += h.id -> h
      db.indexes += ByteArrayWrapper(Algos.hash("validity".getBytes("UTF-8") ++ idToBytes(h.id))) -> Array(0.toByte)
      db.objects += h.transactionsId -> txs.copy(headerId = h.id)
      db.objects += h.extensionId -> ext.copy(headerId = h.id)
      if (stateType.requireProofs || stateType == StateType.Utxo)
        db.objects += h.ADProofsId -> proofs.copy(headerId = h.id)
    }
    db.indexes += ByteArrayWrapper(Array.fill[Byte](32)(Header.modifierTypeId)) -> idToBytes(tip.id)
    db.indexes += ByteArrayWrapper(Array.fill[Byte](32)(-1)) -> idToBytes(tip.id)
    db.indexes += ByteArrayWrapper(Algos.hash("height".getBytes("UTF-8") ++ idToBytes(tip.id))) -> Ints.toByteArray(tip.height)
    db.indexes += heightKey -> children.flatMap(h => idToBytes(h.id)).toArray

    def healthReason: String = {
      val progress = ChainProgress(tip, tip.height, tip.height, 0L)
      ErgoNodeViewHolder.checkChainIsHealthy(progress, history, testSettings) match {
        case ChainIsStuck(reason) => reason
        case result => fail(s"Expected a stuck chain, got $result")
      }
    }
  }

  property("UTXO cleanup removes a stored proof before its invalid header") {
    val f = new Fixture
    val child = f.children.head
    f.db.objects should contain key (child.ADProofsId)
    f.history.forgetHeader(child.id) shouldBe Success(())
    child.sectionIds.foreach { case (_, id) => f.db.objects should not contain key (id) }
    f.db.objects should not contain key (child.id)
  }

  (1 to 4).foreach { failedRemoval =>
    property(s"forgetHeader propagates removal $failedRemoval and stops subsequent deletions") {
      val f = new Fixture
      f.db.failAt = failedRemoval
      f.history.forgetHeader(f.children.head.id) shouldBe Failure(f.db.failure)
      f.db.removals.size shouldBe failedRemoval
    }
  }

  Seq((1, "modifier"), (4, "header")).foreach { case (failedRemoval, kind) =>
    Seq(false, true).foreach { throws =>
      property(s"$kind removal ${if (throws) "throw" else "Failure"} logs its original error") {
        val f = new Fixture
        val child = f.children.head
        if (throws) f.db.throwAt = failedRemoval else f.db.failAt = failedRemoval

        val (result, events) = captureHistoryErrors { f.history.forgetHeader(child.id) }
        result shouldBe Failure(f.db.failure)
        f.db.removals.size shouldBe failedRemoval
        f.db.objects should contain key (child.id)

        val failedId = if (failedRemoval == 1) child.sectionIds.head._2 else child.id
        val matching = events.filter(_.getFormattedMessage.contains(s"Failed to remove $kind $failedId"))
        matching should have size 1
        matching.head.getThrowableProxy.asInstanceOf[ThrowableProxy].getThrowable should be theSameInstanceAs f.db.failure
      }
    }
  }

  (1 to 9).foreach { failedRemoval =>
    property(s"repair propagates removal $failedRemoval without clearing the retry index") {
      val f = new Fixture
      f.db.failAt = failedRemoval
      ErgoHistory.repairIfNeeded(f.history) shouldBe Failure(f.db.failure)
      f.db.removals.size shouldBe failedRemoval
      f.db.indexes should contain key f.heightKey
    }
  }

  (1 to 4).foreach { failedRemoval =>
    property(s"retry discovers all sections after removal $failedRemoval fails") {
      val f = new Fixture
      val child = f.children.head
      f.db.failAt = failedRemoval
      f.history.forgetHeader(child.id)
      f.db.objects should contain key (child.id)
      f.db.failAt = -1
      f.history.forgetHeader(child.id) shouldBe Success(())
      (child.id +: child.sectionIds.map(_._2)).foreach { id =>
        f.db.objects should not contain key (id)
      }
    }
  }

  (1 to 9).foreach { failedRemoval =>
    property(s"repair retry finishes after partial progress at removal $failedRemoval") {
      val f = new Fixture
      f.db.failAt = failedRemoval
      ErgoHistory.repairIfNeeded(f.history) shouldBe Failure(f.db.failure)
      f.db.failAt = -1
      ErgoHistory.repairIfNeeded(f.history) shouldBe Success(true)
      f.children.flatMap(h => h.id +: h.sectionIds.map(_._2)).foreach { id =>
        f.db.objects should not contain key (id)
      }
      f.db.indexes should not contain key (f.heightKey)
      f.history.bestFullBlockOpt.map(_.id) shouldBe Some(f.tip.id)
    }
  }

  Seq(1, 4).foreach { failedRemoval =>
    property(s"repair retries when object removal commits but index removal $failedRemoval fails") {
      val f = new Fixture
      val child = f.children.head
      f.db.failAfterObjectsAt = failedRemoval

      ErgoHistory.repairIfNeeded(f.history) shouldBe Failure(f.db.failure)
      f.db.indexes should contain key f.heightKey
      if (failedRemoval == 1) {
        f.db.objects should contain key (child.id)
      } else {
        f.db.objects should not contain key (child.id)
      }

      f.db.failAfterObjectsAt = -1
      ErgoHistory.repairIfNeeded(f.history) shouldBe Success(true)
      f.children.flatMap(h => h.id +: h.sectionIds.map(_._2)).foreach { id =>
        f.db.objects should not contain key (id)
      }
      f.db.indexes should not contain key (f.heightKey)
      f.history.bestFullBlockOpt.map(_.id) shouldBe Some(f.tip.id)
    }
  }

  (1 to 4).foreach { failedRemoval =>
    property(s"digest history propagates required section or header removal $failedRemoval") {
      val f = new Fixture(StateType.Digest)
      f.db.failAt = failedRemoval
      f.history.forgetHeader(f.children.head.id) shouldBe Failure(f.db.failure)
      f.db.removals.size shouldBe failedRemoval
      f.db.objects should contain key (f.children.head.id)
      f.db.failAt = -1
      f.history.forgetHeader(f.children.head.id) shouldBe Success(())
      f.children.head.sectionIds.foreach { case (_, id) => f.db.objects should not contain key (id) }
    }
  }

  property("header-only mode removes any stored sections with the header") {
    val f = new Fixture(verifyTransactions = false)
    f.history.forgetHeader(f.children.head.id) shouldBe Success(())
    f.db.removals.map(_._2).toSeq shouldBe
      (f.children.head.sectionIds.map { case (_, id) => Seq(id) } :+ Seq(f.children.head.id))
  }

  property("missing headers still have their header indexes removed") {
    val f = new Fixture
    f.db.objects.remove(f.children.head.id)
    f.history.forgetHeader(f.children.head.id) shouldBe Success(())
    f.db.removals.size shouldBe 1
    f.db.removals.head._1.size shouldBe 3
    f.db.removals.head._2 shouldBe Seq(f.children.head.id)
  }

  property("repair reports completion only after all removals") {
    val f = new Fixture
    ErgoHistory.repairIfNeeded(f.history) shouldBe Success(true)
    f.db.removals.size shouldBe 9
    f.db.removals.last shouldBe ((Seq(f.heightKey), Seq.empty))
    f.db.indexes should not contain key (f.heightKey)
    ErgoHistory.repairIfNeeded(f.history) shouldBe Success(false)
    f.db.removals.size shouldBe 9
  }

  property("repair is unnecessary when the full block tip is behind the header tip") {
    val f = new Fixture
    f.db.indexes.remove(ByteArrayWrapper(Array.fill[Byte](32)(-1)))
    ErgoHistory.repairIfNeeded(f.history) shouldBe Success(false)
    f.db.removals shouldBe empty
  }

  property("health owner reports the repair failure") {
    val f = new Fixture
    f.db.failAt = 1
    f.healthReason should include ("repair failed: java.lang.IllegalStateException: history removal sentinel")
    f.db.removals.size shouldBe 1
  }

  property("health owner distinguishes completed repair from an unnecessary repair") {
    val f = new Fixture
    f.healthReason should include ("repair completed")
    f.healthReason should include ("repair not needed")
    f.db.removals.size shouldBe 9
  }

  Seq(1, 9).foreach { failedRemoval =>
    property(s"startup owner propagates removal $failedRemoval failure and closes storage") {
      val f = new Fixture
      f.db.failAt = failedRemoval
      val error = intercept[IllegalStateException] {
        ErgoHistory.readOrGenerate(testSettings, f.db)(null)
      }
      error should be theSameInstanceAs f.db.failure
      f.db.closed shouldBe true
      f.db.indexes should contain key f.heightKey
    }
  }

  property("startup returns repaired history after all removals succeed") {
    val f = new Fixture
    val loaded = ErgoHistory.readOrGenerate(testSettings, f.db)(null)
    loaded.bestFullBlockOpt.map(_.id) shouldBe Some(f.tip.id)
    loaded.headerIdsAtHeight(f.tip.height + 1) shouldBe empty
    f.db.closed shouldBe false
  }

  property("best full block repair returns a storage read failure") {
    val f = new Fixture
    f.db.throwOnReadId = Some(f.tip.transactionsId)
    f.db.throwOnReadAt = 1

    f.history.repairIncompleteBestFullBlock() shouldBe Failure(f.db.readFailure)
    f.db.targetedReads shouldBe 1
    f.db.removals shouldBe empty
  }

  property("startup closes storage and logs a best full block read failure") {
    val f = new Fixture
    // The first read belongs to repairIfNeeded; fail only in the following repair.
    f.db.indexes.remove(f.heightKey)
    f.db.throwOnReadId = Some(f.tip.transactionsId)
    f.db.throwOnReadAt = 2

    val (error, events) = captureHistoryErrors {
      intercept[IllegalStateException] { ErgoHistory.readOrGenerate(testSettings, f.db)(null) }
    }
    error should be theSameInstanceAs f.db.readFailure
    f.db.targetedReads shouldBe 2
    f.db.closed shouldBe true
    f.db.removals shouldBe empty
    val startupErrors = events.filter(_.getFormattedMessage.startsWith("Best full block repair failed; stop this node."))
    startupErrors should have size 1
    startupErrors.head.getFormattedMessage should include ("keep history/index and history/objects intact")
    startupErrors.head.getThrowableProxy.asInstanceOf[ThrowableProxy].getThrowable should be theSameInstanceAs f.db.readFailure
  }

  property("startup closes storage and logs a UTXO bootstrap recovery read failure") {
    val f = new Fixture(freshTip = true)
    val bestFullKey = ByteArrayWrapper(Array.fill[Byte](32)(-1))
    val minFullHeightKey = ByteArrayWrapper(Algos.hash("minfullheight".getBytes("UTF-8")))
    f.db.indexes.remove(bestFullKey)
    f.tip.isNew(bootstrapSettings.chainSettings.blockInterval *
      bootstrapSettings.nodeSettings.headerChainDiff) shouldBe true
    f.history.bestHeaderOpt.map(_.id) shouldBe Some(f.tip.id)
    f.history.isUtxoSnapshotApplied shouldBe false
    f.db.throwOnIndexKey = Some(minFullHeightKey)

    val (error, events) = captureHistoryErrors {
      intercept[IllegalStateException] { ErgoHistory.readOrGenerate(bootstrapSettings, f.db)(null) }
    }
    error should be theSameInstanceAs f.db.readFailure
    f.db.targetedIndexReads shouldBe 1
    f.db.closeCount shouldBe 1
    f.db.removals shouldBe empty
    val startupErrors = events.filter(_.getFormattedMessage.startsWith("UTXO bootstrap recovery failed; stop this node."))
    startupErrors should have size 1
    startupErrors.head.getFormattedMessage should include ("keep history/index and history/objects intact")
    startupErrors.head.getThrowableProxy.asInstanceOf[ThrowableProxy].getThrowable should be theSameInstanceAs f.db.readFailure
  }

  property("startup preserves the repair error when closing storage also fails") {
    val f = new Fixture
    val closeError = new IllegalStateException("history close sentinel")
    f.db.failAt = 1
    f.db.closeFailure = Some(closeError)
    val (error, events) = captureHistoryErrors {
      intercept[IllegalStateException] { ErgoHistory.readOrGenerate(testSettings, f.db)(null) }
    }
    error should be theSameInstanceAs f.db.failure
    error.getSuppressed.toSeq shouldBe Seq(closeError)
    val startupErrors = events.filter(_.getFormattedMessage.startsWith("History repair failed; stop this node."))
    startupErrors should have size 1
    startupErrors.head.getFormattedMessage should include ("keep history/index and history/objects intact")
    startupErrors.head.getThrowableProxy.asInstanceOf[ThrowableProxy].getThrowable should be theSameInstanceAs f.db.failure
  }

  property("startup avoids self-suppression if closing returns the repair error") {
    val f = new Fixture
    f.db.failAt = 1
    f.db.closeFailure = Some(f.db.failure)
    val error = intercept[IllegalStateException] { ErgoHistory.readOrGenerate(testSettings, f.db)(null) }
    error should be theSameInstanceAs f.db.failure
    error.getSuppressed shouldBe empty
  }

  property("extra-index preflight read failure closes storage and preserves its cause") {
    val settings = testSettings.copy(nodeSettings = testSettings.nodeSettings.copy(extraIndex = true))
    val readError = new IllegalStateException("extra-index preflight read sentinel")
    val closeError = new IllegalStateException("extra-index preflight close sentinel")
    val db = new TestStorage {
      override def modifierBytesById(id: ModifierId): Option[Array[Byte]] =
        if (id == bytesToId(SchemaVersionKey)) throw readError else None
    }
    db.closeFailure = Some(closeError)

    val (error, events) = captureHistoryErrors {
      intercept[IllegalStateException] { ErgoHistory.readOrGenerate(settings, db)(null) }
    }
    error should be theSameInstanceAs readError
    error.getSuppressed.toSeq shouldBe Seq(closeError)
    db.closed shouldBe true
    val startupErrors = events.filter(_.getFormattedMessage.startsWith("Extra-index preflight read failed;"))
    startupErrors should have size 1
    startupErrors.head.getFormattedMessage should include ("keep history/index and history/objects intact")
    startupErrors.head.getThrowableProxy.asInstanceOf[ThrowableProxy].getThrowable should be theSameInstanceAs readError
  }

  property("extra-index preflight read failure after rebuild closes only the current storage") {
    val settings = testSettings.copy(nodeSettings = testSettings.nodeSettings.copy(extraIndex = true))
    val readError = new IllegalStateException("rebuilt extra-index read sentinel")
    val rebuilt = new TestStorage {
      override def insertExtraTry(
        indexesToInsert: Array[(Array[Byte], Array[Byte])],
        objectsToInsert: Array[ExtraIndex]
      ): Try[Unit] =
        Success(())
      override def modifierBytesById(id: ModifierId): Option[Array[Byte]] =
        if (id == bytesToId(RentIndexEnabledKey)) throw readError else None
    }
    val original = new TestStorage {
      override def modifierBytesById(id: ModifierId): Option[Array[Byte]] = None
      override def deleteExtraDBTry(ergoSettings: ErgoSettings): Try[HistoryStorage] = {
        close()
        Success(rebuilt)
      }
    }

    val (error, events) = captureHistoryErrors {
      intercept[IllegalStateException] { ErgoHistory.readOrGenerate(settings, original)(null) }
    }
    error should be theSameInstanceAs readError
    original.closeCount shouldBe 1
    rebuilt.closeCount shouldBe 1
    rebuilt.closed shouldBe true
    events.filter(_.getFormattedMessage.startsWith("Extra-index preflight read failed;")) should have size 1
  }
}
