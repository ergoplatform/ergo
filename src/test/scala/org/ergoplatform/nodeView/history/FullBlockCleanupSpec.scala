package org.ergoplatform.nodeView.history

import com.google.common.primitives.Ints
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockSectionProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.initSettings
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain}
import scorex.db.{ByteArrayWrapper, LDBFactory}
import scorex.util.ModifierId

import java.nio.file.Files
import scala.util.{Failure, Try}

class FullBlockCleanupSpec extends ErgoCorePropertyTest {
  private val cursorKey = ByteArrayWrapper(Algos.hash("pruned_height".getBytes("UTF-8")))
  private val deletionError = new IllegalStateException("test deletion failure")
  private val cursorError = new IllegalStateException("test cursor failure")

  private class ControlledStorage(config: ErgoSettings) extends HistoryStorage(
    LDBFactory.createKvDb(s"${config.directory}/index"),
    LDBFactory.createKvDb(s"${config.directory}/objects"),
    LDBFactory.createKvDb(s"${config.directory}/extra"), config.cacheSettings) {
    var failDeletion = false
    var failCursor = false
    var deletionAttempts = Vector.empty[Vector[ModifierId]]

    override def insert(indexesToInsert: Array[(ByteArrayWrapper, Array[Byte])],
                        objectsToInsert: Array[BlockSection]): Try[Unit] = {
      if (failCursor && indexesToInsert.exists(_._1 == cursorKey)) Failure(cursorError)
      else super.insert(indexesToInsert, objectsToInsert)
    }

    override def remove(indicesToRemove: Array[ByteArrayWrapper], idsToRemove: Array[ModifierId]): Try[Unit] = {
      deletionAttempts :+= idsToRemove.toVector
      if (failDeletion) Failure(deletionError) else super.remove(indicesToRemove, idsToRemove)
    }
  }

  private def withHistory(test: (ErgoHistory, ControlledStorage) => Unit): Unit = {
    val config = initSettings.copy(
      directory = Files.createTempDirectory("full-block-cleanup").toString,
      chainSettings = initSettings.chainSettings.copy(genesisId = None, epochLength = 100000000),
      nodeSettings = initSettings.nodeSettings.copy(stateType = StateType.Digest,
        verifyTransactions = true, blocksToKeep = 3))
    val storage = new ControlledStorage(config)
    val history = new ErgoHistory with FullBlockSectionProcessor {
      override protected val settings: ErgoSettings = config
      override protected[history] val historyStorage: HistoryStorage = storage
      override val powScheme: AutolykosPowScheme = config.chainSettings.powScheme
    }
    history.isHeadersChainSyncedVar = true
    try test(history, storage)
    finally history.closeStorage()
  }

  property("cleanup retries a returned deletion failure without failing best-chain progress") {
    withHistory { (history, storage) =>
      val chain = genChain(7, history)
      applyChain(history, chain.take(4))
      val before = history.readPrunedHeight()
      storage.failDeletion = true
      applyChain(history, chain.slice(4, 5))
      history.bestFullBlockIdOpt shouldBe Some(chain(4).id)
      history.readPrunedHeight() shouldBe before
      val failedIds = storage.deletionAttempts.last
      failedIds.nonEmpty shouldBe true
      failedIds.foreach(id => history.modifierById(id).isDefined shouldBe true)
      storage.failDeletion = false
      applyChain(history, chain.slice(5, 6))
      storage.deletionAttempts.last should contain allElementsOf failedIds
      failedIds.foreach(id => history.modifierById(id) shouldBe None)
      history.readPrunedHeight() should be > before
    }
  }

  property("cleanup retries deletion after cursor persistence fails") {
    withHistory { (history, storage) =>
      val chain = genChain(7, history)
      applyChain(history, chain.take(4))
      val before = history.readPrunedHeight()
      storage.failCursor = true
      applyChain(history, chain.slice(4, 5))
      history.bestFullBlockIdOpt shouldBe Some(chain(4).id)
      history.readPrunedHeight() shouldBe before
      val deletedIds = storage.deletionAttempts.last
      deletedIds.nonEmpty shouldBe true
      deletedIds.foreach(id => history.modifierById(id) shouldBe None)
      storage.failCursor = false
      applyChain(history, chain.slice(5, 6))
      storage.deletionAttempts.last should contain allElementsOf deletedIds
      history.readPrunedHeight() should be > before
      storage.getIndex(cursorKey).map(Ints.fromByteArray) shouldBe Some(history.readPrunedHeight())
    }
  }

  property("cleanup retains the fallback floor when initial cursor persistence fails") {
    withHistory { (history, storage) =>
      val chain = genChain(6, history)
      val before = history.readMinimalFullBlockHeight()
      storage.failCursor = true
      applyChain(history, chain.take(5))
      history.bestFullBlockIdOpt shouldBe Some(chain(4).id)
      history.readMinimalFullBlockHeight() shouldBe before
      storage.getIndex(cursorKey) shouldBe None
      storage.deletionAttempts shouldBe empty
      storage.failCursor = false
      applyChain(history, chain.drop(5))
      history.readPrunedHeight() should be > before
      storage.deletionAttempts.last should contain(chain.head.blockTransactions.id)
    }
  }

  property("cursor writes report persistence failure to their caller") {
    withHistory { (history, storage) =>
      history.writePrunedHeight(2)
      storage.failCursor = true
      Try(history.writePrunedHeight(3)) shouldBe Failure(cursorError)
      history.readPrunedHeight() shouldBe 2
    }
  }

  property("wallet-bound cleanup waits then resumes the deferred range") {
    withHistory { (history, storage) =>
      val chain = genChain(8, history)
      history.updateWalletScannedHeight(2)
      applyChain(history, chain.take(6))
      history.readPrunedHeight() shouldBe 3
      val attempts = storage.deletionAttempts.size
      applyChain(history, chain.slice(6, 7))
      storage.deletionAttempts.size shouldBe attempts
      history.modifierById(chain(2).blockTransactions.id).isDefined shouldBe true
      history.updateWalletScannedHeight(7)
      applyChain(history, chain.drop(7))
      history.readPrunedHeight() should be > 3
      history.modifierById(chain(2).blockTransactions.id) shouldBe None
    }
  }
}
