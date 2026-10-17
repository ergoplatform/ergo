package org.ergoplatform.nodeView.history

import com.google.common.primitives.Ints
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockSectionProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.initSettings
import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen
import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.invalidErgoTransactionGen
import scorex.db.ByteArrayWrapper
import scorex.util.{ModifierId, idToBytes}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class HistoryRepairWriteFailureSpec extends ErgoCorePropertyTest {
  private val testSettings = initSettings.copy(nodeSettings = initSettings.nodeSettings.copy(
    stateType = StateType.Utxo, verifyTransactions = true, extraIndex = false))

  private class TestStorage extends HistoryStorage(null, null, null, testSettings.cacheSettings) {
    val indexes: mutable.Map[ByteArrayWrapper, Array[Byte]] = mutable.Map.empty
    val objects: mutable.Map[ModifierId, BlockSection] = mutable.Map.empty
    val unreadableIds: mutable.Set[ModifierId] = mutable.Set.empty
    val failure = new IllegalStateException("history repair write sentinel")
    var failRemove: Boolean = false
    var failRemoveAt: Int = -1
    var removes: Int = 0
    var failInsert: Boolean = false

    override def getIndex(key: ByteArrayWrapper): Option[Array[Byte]] = indexes.get(key)
    override def modifierById(id: ModifierId): Option[BlockSection] = objects.get(id)
    override def modifierByIdFromDb(id: ModifierId): Option[BlockSection] =
      if (unreadableIds.contains(id)) None else objects.get(id)
    override def contains(id: ModifierId): Boolean = objects.contains(id)

    override def insert(indexesToInsert: Array[(ByteArrayWrapper, Array[Byte])],
                        objectsToInsert: Array[BlockSection]): Try[Unit] = {
      if (failInsert) Failure(failure)
      else {
        indexesToInsert.foreach { case (key, value) => indexes.update(key, value) }
        objectsToInsert.foreach(mod => objects.update(mod.id, mod))
        Success(())
      }
    }

    override def remove(keys: Array[ByteArrayWrapper], ids: Array[ModifierId]): Try[Unit] = {
      removes += 1
      if (failRemove || removes == failRemoveAt) Failure(failure)
      else {
        keys.foreach(indexes.remove)
        ids.foreach(objects.remove)
        Success(())
      }
    }
  }

  private class Fixture {
    val db = new TestStorage
    val history: ErgoHistory = new ErgoHistory with FullBlockSectionProcessor {
      override protected val settings: ErgoSettings = testSettings
      override protected[history] val historyStorage: HistoryStorage = db
      override val powScheme: AutolykosPowScheme = testSettings.chainSettings.powScheme
    }
    val seed: Header = defaultHeaderGen.sample.get.copy(height = 10)
    val txs = BlockTransactions(seed.id, seed.version, Seq(invalidErgoTransactionGen.sample.get))
    val ext = Extension(seed.id, Seq.empty)
    val tip: Header = seed.copy(transactionsRoot = txs.digest, extensionRoot = ext.digest)
    val child: Header = tip.copy(parentId = tip.id, height = tip.height + 1, timestamp = tip.timestamp + 1)
    val grandchild: Header = child.copy(parentId = child.id, height = child.height + 1,
      timestamp = child.timestamp + 1)

    def heightKey(height: Int): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(height)))
    def headerHeightKey(id: ModifierId): ByteArrayWrapper =
      ByteArrayWrapper(Algos.hash("height".getBytes("UTF-8") ++ idToBytes(id)))

    Seq(tip, child, grandchild).foreach { header =>
      db.objects += header.id -> header
      db.objects += header.transactionsId -> txs.copy(headerId = header.id)
      db.objects += header.extensionId -> ext.copy(headerId = header.id)
      db.indexes += headerHeightKey(header.id) -> Ints.toByteArray(header.height)
    }
    db.indexes += ByteArrayWrapper(Array.fill[Byte](32)(-1)) -> idToBytes(tip.id)
    db.indexes += ByteArrayWrapper(Array.fill[Byte](32)(Header.modifierTypeId)) -> idToBytes(child.id)
    db.indexes += heightKey(child.height) -> idToBytes(child.id)
    db.indexes += heightKey(grandchild.height) -> idToBytes(grandchild.id)

    def selectGrandchild(): Unit =
      db.indexes.update(ByteArrayWrapper(Array.fill[Byte](32)(Header.modifierTypeId)), idToBytes(grandchild.id))
  }

  property("damaged header truncation propagates the first failed removal") {
    val f = new Fixture
    f.selectGrandchild()
    f.db.unreadableIds += f.child.id
    f.db.failRemove = true

    val error = intercept[IllegalStateException] { ErgoHistory.repairIfNeeded(f.history) }
    error should be theSameInstanceAs f.db.failure
    f.history.bestHeaderIdOpt shouldBe Some(f.grandchild.id)
    f.db.objects.contains(f.grandchild.id) shouldBe true
  }

  property("partial truncation does not report an unreadable selected header as repaired") {
    val f = new Fixture
    f.selectGrandchild()
    f.db.unreadableIds += f.child.id
    f.db.failRemoveAt = 4

    val firstError = intercept[IllegalStateException] { ErgoHistory.repairIfNeeded(f.history) }
    firstError should be theSameInstanceAs f.db.failure
    f.db.removes shouldBe 4
    f.history.bestHeaderIdOpt shouldBe Some(f.grandchild.id)
    f.db.objects.contains(f.grandchild.id) shouldBe false

    f.db.failRemoveAt = -1
    val retryError = intercept[IllegalStateException] { ErgoHistory.repairIfNeeded(f.history) }
    retryError.getMessage should include ("Selected best header")
  }

  property("unapplied full block section removal failure leaves the section retryable") {
    val f = new Fixture
    f.db.failRemove = true

    val error = intercept[IllegalStateException] { ErgoHistory.repairIfNeeded(f.history) }
    error should be theSameInstanceAs f.db.failure
    f.history.bestHeaderIdOpt shouldBe Some(f.child.id)
    f.db.objects.contains(f.child.transactionsId) shouldBe true

    f.db.failRemove = false
    ErgoHistory.repairIfNeeded(f.history) shouldBe true
    f.db.objects.contains(f.child.transactionsId) shouldBe false
  }

  property("missing selected height row insertion failure leaves the row retryable") {
    val f = new Fixture
    f.db.indexes.remove(f.heightKey(f.child.height))
    f.db.objects.remove(f.child.transactionsId)
    f.db.objects.remove(f.child.extensionId)
    f.db.failInsert = true

    val error = intercept[IllegalStateException] { ErgoHistory.repairIfNeeded(f.history) }
    error should be theSameInstanceAs f.db.failure
    f.history.bestHeaderIdOpt shouldBe Some(f.child.id)
    f.history.headerIdsAtHeight(f.child.height) shouldBe empty

    f.db.failInsert = false
    ErgoHistory.repairIfNeeded(f.history) shouldBe true
    f.history.headerIdsAtHeight(f.child.height) shouldBe Seq(f.child.id)
  }
}
