package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.http.api.SortDirection.{ASC, DESC, Direction}
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.{BlockSection, NonHeaderBlockSection}
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.SegmentSerializer.boxSegmentId
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.util.{ModifierId, bytesToId}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

class SegmentUtxoPaginationSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants.{settings => testSettings}
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.validErgoTransactionGenTemplate

  private class Fixture {
    private val boxes = validErgoTransactionGenTemplate(0, maxInputs = 6,
      propositionGen = Gen.const(TrueTree)).sample.get._1
    val records: Vector[IndexedErgoBox] = boxes.zipWithIndex.map { case (box, i) =>
      new IndexedErgoBox(i + 1, None, None, None, box, i + 1L)
    }.toVector
    private val transactions = Vector.fill(3) {
      validErgoTransactionGenTemplate(0, maxInputs = 1,
        propositionGen = Gen.const(TrueTree)).sample.get._2
    }
    val pool: ErgoMemPool = ErgoMemPool.empty(testSettings)
      .put(transactions.map(tx => UnconfirmedTransaction(tx, None)))
    private val mempoolBoxes = pool.getAll.flatMap(_.transaction.outputs).toVector
    val spent: Set[ModifierId] = Set(records(4).id, bytesToId(mempoolBoxes.head.id))
    val unconfirmedIds: Vector[ModifierId] = mempoolBoxes.map(box => bytesToId(box.id)).filterNot(spent)
    val confirmedIds: Vector[ModifierId] = records.zipWithIndex.collect {
      case (box, i) if i != 2 && !spent(box.id) => box.id
    }

    val address: IndexedErgoAddress = IndexedErgoAddress(hashErgoTree(TrueTree), boxes = ArrayBuffer(6L))
    address.boxSegmentCount = 2
    private val first = IndexedErgoAddress(boxSegmentId(address.id, 0), boxes = ArrayBuffer(1L, 2L, -3L))
    private val second = IndexedErgoAddress(boxSegmentId(address.id, 1), boxes = ArrayBuffer(4L, 5L))
    private val entries: Map[ModifierId, ExtraIndex] = (records.flatMap { box =>
      Vector[ExtraIndex](box, NumericBoxIndex(box.globalIndex, box.id))
    } ++ Vector[ExtraIndex](first, second)).map(index => index.id -> index).toMap
    var historyReads: Int = 0
    val history: ErgoHistoryReader = new ErgoHistoryReader {
      override protected[history] val historyStorage: HistoryStorage = null
      override protected val settings: ErgoSettings = testSettings
      override protected def requireProofs: Boolean = false
      override protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] =
        Failure(new UnsupportedOperationException("read-only fixture"))
      override protected def validate(m: NonHeaderBlockSection): Try[Unit] =
        Failure(new UnsupportedOperationException("read-only fixture"))
      override val powScheme: AutolykosPowScheme = null
      override def typedExtraIndexById[T <: ExtraIndex : ClassTag](id: ModifierId): Option[T] = {
        historyReads += 1
        entries.get(id).collect { case index: T => index }
      }
    }

    def expected(direction: Direction, includeUnconfirmed: Boolean = true): Vector[ModifierId] = direction match {
      case ASC => confirmedIds ++ (if (includeUnconfirmed) unconfirmedIds else Vector.empty)
      case DESC => (if (includeUnconfirmed) unconfirmedIds else Vector.empty) ++ confirmedIds.reverse
    }

    def page(direction: Direction, offset: Int, limit: Int,
             includeUnconfirmed: Boolean = true): Seq[ModifierId] =
      address.retrieveUtxos(history, pool, offset, limit, direction, includeUnconfirmed, spent).map(_.id)
  }

  property("paginate the combined filtered sequence across memory and stored segments in both directions") {
    val fixture = new Fixture
    for {
      direction <- Seq(ASC, DESC)
      includeUnconfirmed <- Seq(false, true)
      offset <- 0 to (fixture.expected(direction, includeUnconfirmed).length + 1)
      limit <- Seq(0, 1, 2, 4)
    } {
      val expected = fixture.expected(direction, includeUnconfirmed).drop(offset).take(limit)
      val result = fixture.page(direction, offset, limit, includeUnconfirmed)
      result shouldBe expected
      result.length should be <= limit
    }
  }

  property("consecutive pages cover each eligible box once without repeating mempool boxes") {
    val fixture = new Fixture
    Seq(ASC, DESC).foreach { direction =>
      val expected = fixture.expected(direction)
      val paged = (0 until expected.length by 2).flatMap(offset => fixture.page(direction, offset, 2))
      paged shouldBe expected
      paged.distinct shouldBe paged
      paged.toSet.intersect(fixture.spent) shouldBe empty
    }
  }

  property("keep membership filtering and exhausted or empty result windows") {
    val fixture = new Fixture
    val unmatched = IndexedErgoAddress(bytesToId(Array.fill(32)(42.toByte)))
    Seq(ASC, DESC).foreach { direction =>
      unmatched.retrieveUtxos(fixture.history, fixture.pool, 0, 10, direction,
        unconfirmed = true, spentBoxesIdsInMempool = Set.empty) shouldBe empty
      fixture.page(direction, Int.MaxValue, 1) shouldBe empty
      fixture.page(direction, 1, Int.MaxValue) shouldBe fixture.expected(direction).drop(1)
    }
  }

  property("avoid loading confirmed history when the page ends within unconfirmed boxes") {
    val fixture = new Fixture
    fixture.page(DESC, 0, 1) shouldBe fixture.unconfirmedIds.take(1)
    fixture.historyReads shouldBe 0
    fixture.page(ASC, 0, 0) shouldBe empty
    fixture.historyReads shouldBe 0
  }
}
