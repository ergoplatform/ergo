package org.ergoplatform.nodeView.history

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.EmptyBlockSectionProcessor
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoNodeTestConstants
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.db.{ByteArrayWrapper, LDBKVStore}
import scorex.util.ModifierId

import scala.util.{Failure, Success, Try}

/** Failure injection for the reset boundary and both stores behind HistoryStorage.remove. */
class HistoryResetFailureSpec extends AnyFlatSpec with Matchers {
  it should "propagate index-store failure instead of wrapping a nested Failure as success" in {
    val failure = new IllegalStateException("injected index failure")
    val objects = new LDBKVStore(null) {
      override def remove(keys: Array[Array[Byte]]): Try[Unit] = Success(())
    }
    val indices = new LDBKVStore(null) {
      override def remove(keys: Array[Array[Byte]]): Try[Unit] = Failure(failure)
    }
    val storage = new HistoryStorage(indices, objects, null, ErgoNodeTestConstants.settings.cacheSettings)
    storage.remove(Array.empty[ByteArrayWrapper], Array.empty[ModifierId]) shouldBe Failure(failure)
  }

  it should "skip index deletion when object-store deletion fails" in {
    val failure = new IllegalStateException("injected object failure")
    var touchedIndices = false
    val objects = new LDBKVStore(null) {
      override def remove(keys: Array[Array[Byte]]): Try[Unit] = Failure(failure)
    }
    val indices = new LDBKVStore(null) {
      override def remove(keys: Array[Array[Byte]]): Try[Unit] = { touchedIndices = true; Success(()) }
    }
    val storage = new HistoryStorage(indices, objects, null, ErgoNodeTestConstants.settings.cacheSettings)
    storage.remove(Array.empty[ByteArrayWrapper], Array.empty[ModifierId]) shouldBe Failure(failure)
    touchedIndices shouldBe false
  }

  it should "propagate failure when removing reset best-pointer indices" in {
    val config = ErgoNodeTestConstants.settings
    val failure = new IllegalStateException("injected index removal failure")
    var attemptedRemoval = false
    val storage = new HistoryStorage(null, null, null, config.cacheSettings) {
      override def remove(indices: Array[ByteArrayWrapper], ids: Array[ModifierId]): Try[Unit] = {
        attemptedRemoval = true
        Failure(failure)
      }
    }
    val history = new ErgoHistory with EmptyBlockSectionProcessor {
      override protected val settings: ErgoSettings = config
      override protected[history] val historyStorage: HistoryStorage = storage
      override val powScheme: AutolykosPowScheme = config.chainSettings.powScheme
      override def headersHeight: Int = 0
      override def headerIdsAtHeight(height: Int): Seq[ModifierId] = Seq.empty
      override def bestFullBlockOpt: Option[ErgoFullBlock] = None
    }
    val result = history.resetToHeight(0)
    attemptedRemoval shouldBe true
    result.failed.toOption shouldBe Some(failure)
  }
}
