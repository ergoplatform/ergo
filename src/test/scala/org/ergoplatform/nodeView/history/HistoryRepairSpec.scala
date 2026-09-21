package org.ergoplatform.nodeView.history

import com.google.common.primitives.Ints
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{ChainIsStuck, ChainProgress}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockSectionProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.initSettings
import org.ergoplatform.utils.generators.ErgoCoreGenerators.{defaultHeaderGen, randomADProofsGen}
import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.invalidErgoTransactionGen
import scorex.db.ByteArrayWrapper
import scorex.util.{ModifierId, idToBytes}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class HistoryRepairSpec extends ErgoCorePropertyTest {
  private val testSettings = initSettings.copy(nodeSettings = initSettings.nodeSettings.copy(
    stateType = StateType.Utxo, verifyTransactions = true, extraIndex = false))

  // Ordinary storage-return failures isolate each caller join, independently of the storage implementation.
  private class TestStorage extends HistoryStorage(null, null, null, testSettings.cacheSettings) {
    val indexes: mutable.Map[ByteArrayWrapper, Array[Byte]] = mutable.Map.empty
    val objects: mutable.Map[ModifierId, BlockSection] = mutable.Map.empty
    val removals: mutable.Buffer[(Seq[ByteArrayWrapper], Seq[ModifierId])] = mutable.Buffer.empty
    val failure = new IllegalStateException("history removal sentinel")
    var failAt: Int = -1
    var closed: Boolean = false
    var closeFailure: Option[RuntimeException] = None
    override def close(): Unit = {
      closed = true
      closeFailure.foreach(error => throw error)
    }

    override def getIndex(key: ByteArrayWrapper): Option[Array[Byte]] = indexes.get(key)
    override def modifierById(id: ModifierId): Option[BlockSection] = objects.get(id)
    override def contains(id: ModifierId): Boolean = objects.contains(id)
    override def remove(keys: Array[ByteArrayWrapper], ids: Array[ModifierId]): Try[Unit] = {
      removals += ((keys.toSeq, ids.toSeq))
      if (removals.size == failAt) Failure(failure)
      else {
        keys.foreach(indexes.remove)
        ids.foreach(objects.remove)
        Success(())
      }
    }
  }

  private class Fixture(stateType: StateType = StateType.Utxo, verifyTransactions: Boolean = true) {
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
    val tip: Header = seed.copy(transactionsRoot = txs.digest, extensionRoot = ext.digest, ADProofsRoot = proofs.digest)
    val children: Seq[Header] = (1 to 2).map(i => tip.copy(
      parentId = tip.id, height = tip.height + 1, timestamp = tip.timestamp + i))
    val heightKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(tip.height + 1)))

    db.objects += tip.id -> tip
    db.objects += tip.transactionsId -> txs.copy(headerId = tip.id)
    db.objects += tip.extensionId -> ext.copy(headerId = tip.id)
    if (stateType.requireProofs) db.objects += tip.ADProofsId -> proofs.copy(headerId = tip.id)
    children.foreach { h =>
      db.objects += h.id -> h
      db.objects += h.transactionsId -> txs.copy(headerId = h.id)
      db.objects += h.extensionId -> ext.copy(headerId = h.id)
      if (stateType.requireProofs) db.objects += h.ADProofsId -> proofs.copy(headerId = h.id)
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

  (1 to 3).foreach { failedRemoval =>
    property(s"forgetHeader propagates removal $failedRemoval and stops subsequent deletions") {
      val f = new Fixture
      f.db.failAt = failedRemoval
      f.history.forgetHeader(f.children.head.id) shouldBe Failure(f.db.failure)
      f.db.removals.size shouldBe failedRemoval
    }
  }

  (1 to 7).foreach { failedRemoval =>
    property(s"repair propagates removal $failedRemoval without clearing the retry index") {
      val f = new Fixture
      f.db.failAt = failedRemoval
      ErgoHistory.repairIfNeeded(f.history) shouldBe Failure(f.db.failure)
      f.db.removals.size shouldBe failedRemoval
      f.db.indexes should contain key f.heightKey
    }
  }

  (1 to 3).foreach { failedRemoval =>
    property(s"retry discovers all sections after removal $failedRemoval fails") {
      val f = new Fixture
      val child = f.children.head
      f.db.failAt = failedRemoval
      f.history.forgetHeader(child.id)
      f.db.objects.contains(child.id) shouldBe true
      f.db.failAt = -1
      f.history.forgetHeader(child.id) shouldBe Success(())
      (child.id +: child.sectionIdsWithNoProof.map(_._2)).foreach { id =>
        f.db.objects should not contain key (id)
      }
    }
  }

  (1 to 7).foreach { failedRemoval =>
    property(s"repair retry finishes after partial progress at removal $failedRemoval") {
      val f = new Fixture
      f.db.failAt = failedRemoval
      ErgoHistory.repairIfNeeded(f.history) shouldBe Failure(f.db.failure)
      f.db.failAt = -1
      ErgoHistory.repairIfNeeded(f.history) shouldBe Success(true)
      f.children.flatMap(h => h.id +: h.sectionIdsWithNoProof.map(_._2)).foreach { id =>
        f.db.objects.contains(id) shouldBe false
      }
      f.db.indexes.contains(f.heightKey) shouldBe false
      f.history.bestFullBlockOpt.map(_.id) shouldBe Some(f.tip.id)
    }
  }

  (1 to 4).foreach { failedRemoval =>
    property(s"digest history propagates required section or header removal $failedRemoval") {
      val f = new Fixture(StateType.Digest)
      f.db.failAt = failedRemoval
      f.history.forgetHeader(f.children.head.id) shouldBe Failure(f.db.failure)
      f.db.removals.size shouldBe failedRemoval
      f.db.objects.contains(f.children.head.id) shouldBe true
      f.db.failAt = -1
      f.history.forgetHeader(f.children.head.id) shouldBe Success(())
      f.children.head.sectionIds.foreach { case (_, id) => f.db.objects.contains(id) shouldBe false }
    }
  }

  property("header-only mode removes just the header") {
    val f = new Fixture(verifyTransactions = false)
    f.history.forgetHeader(f.children.head.id) shouldBe Success(())
    f.db.removals.map(_._2).toSeq shouldBe Seq(Seq(f.children.head.id))
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
    f.db.removals.size shouldBe 7
    f.db.removals.last shouldBe ((Seq(f.heightKey), Seq.empty))
    f.db.indexes should not contain key (f.heightKey)
    ErgoHistory.repairIfNeeded(f.history) shouldBe Success(false)
    f.db.removals.size shouldBe 7
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
    f.db.removals.size shouldBe 7
  }

  Seq(1, 7).foreach { failedRemoval =>
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

  property("startup preserves the repair error when closing storage also fails") {
    val f = new Fixture
    val closeError = new IllegalStateException("history close sentinel")
    f.db.failAt = 1
    f.db.closeFailure = Some(closeError)
    val error = intercept[IllegalStateException] { ErgoHistory.readOrGenerate(testSettings, f.db)(null) }
    error should be theSameInstanceAs f.db.failure
    error.getSuppressed.toSeq shouldBe Seq(closeError)
  }

  property("startup avoids self-suppression if closing returns the repair error") {
    val f = new Fixture
    f.db.failAt = 1
    f.db.closeFailure = Some(f.db.failure)
    val error = intercept[IllegalStateException] { ErgoHistory.readOrGenerate(testSettings, f.db)(null) }
    error should be theSameInstanceAs f.db.failure
    error.getSuppressed shouldBe empty
  }
}
