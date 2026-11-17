package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.{BlockSection, NonHeaderBlockSection}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.SegmentSerializer.boxSegmentId
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.util.ModifierId
import scorex.util.encode.Base16

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
  * [[Segment.boxLookup]] tells whether an address lists a box number, from the box numbers in its
  * segments and in its row, without reading boxes and without modifying the address.
  */
class SegmentBoxLookupSpec extends ErgoCorePropertyTest {

  private val hash: ModifierId = ModifierId @@ Base16.encode(Array.fill(32)(7.toByte))

  /** History whose only extra indexes are `rows`, counting the reads of them. */
  private class CountingHistory(rows: Map[ModifierId, ExtraIndex]) {
    var reads: Int = 0
    val reader: ErgoHistoryReader = new ErgoHistoryReader {
      override protected[history] val historyStorage: HistoryStorage = new HistoryStorage(null, null, null, null) {
        override def getExtraIndex(id: ModifierId): Option[ExtraIndex] = {
          reads += 1
          rows.get(id)
        }
      }
      override protected val settings: ErgoSettings = null
      override protected def requireProofs: Boolean = ???
      override protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] = ???
      override protected def validate(m: NonHeaderBlockSection): Try[Unit] = ???
      override val powScheme: AutolykosPowScheme = null
    }
  }

  /**
    * An address listing `segments` in its box segments and `inRow` in its row.
    * @param missing - numbers of the segments the database does not have
    */
  private def address(segments: Seq[Seq[Long]], inRow: Seq[Long], missing: Set[Int] = Set.empty): (IndexedErgoAddress, CountingHistory) = {
    val row = IndexedErgoAddress(hash, ArrayBuffer.empty[Long], ArrayBuffer(inRow: _*))
    row.boxSegmentCount = segments.size
    val stored = segments.zipWithIndex.collect { case (boxes, n) if !missing(n) =>
      val id = boxSegmentId(hash, n)
      id -> (IndexedErgoAddress(id, ArrayBuffer.empty[Long], ArrayBuffer(boxes: _*)): ExtraIndex)
    }.toMap
    row -> new CountingHistory(stored)
  }

  private val segments: Seq[Seq[Long]] = Seq(Seq(1L, -3L, 4L, -7L), Seq(10L, 12L, -15L), Seq(20L, -21L))
  private val inRow: Seq[Long] = Seq(30L, -31L, 40L)
  private val listed: Set[Long] = (segments.flatten ++ inRow).map(math.abs).toSet

  property("tells the listed numbers, spent or not, from the others") {
    val (row, history) = address(segments, inRow)
    val lookup = row.boxLookup(history.reader)
    (-45L to 45L).foreach { n =>
      withClue(s"box number $n: ") {
        lookup(n) shouldBe Some(listed(math.abs(n)))
      }
    }
    // every segment is read once, whatever the number of lookups
    history.reads shouldBe segments.size
  }

  property("reads no segment for a number at or above the first one in the row") {
    val (row, history) = address(segments, inRow)
    val lookup = row.boxLookup(history.reader)
    (30L to 45L).foreach(n => lookup(n) shouldBe Some(listed(n)))
    history.reads shouldBe 0
  }

  property("reads nothing for an address without box segments") {
    val (row, history) = address(Seq.empty, inRow)
    val lookup = row.boxLookup(history.reader)
    (0L to 45L).foreach(n => lookup(n) shouldBe Some(inRow.map(math.abs).contains(n)))
    history.reads shouldBe 0

    val (empty, emptyHistory) = address(Seq.empty, Seq.empty)
    empty.boxLookup(emptyHistory.reader)(5L) shouldBe Some(false)
    emptyHistory.reads shouldBe 0
  }

  property("searches the segments when the row is empty") {
    val (row, history) = address(segments, Seq.empty)
    val lookup = row.boxLookup(history.reader)
    (0L to 45L).foreach(n => lookup(n) shouldBe Some(segments.flatten.map(math.abs).contains(n)))
  }

  property("cannot tell when a segment it needs is missing") {
    val (row, history) = address(segments, inRow, missing = Set(1))
    val lookup = row.boxLookup(history.reader)
    // the binary search starts with the middle segment, the missing one
    lookup(12L) shouldBe None
    lookup(1L) shouldBe None
    // the row needs no segment
    lookup(31L) shouldBe Some(true)
  }

  property("does not modify the address") {
    val (row, history) = address(segments, inRow)
    val lookup = row.boxLookup(history.reader)
    (0L to 45L).foreach(lookup)
    row.boxes.toList shouldBe inRow.toList
    row.boxSegmentCount shouldBe segments.size
    row.buffer shouldBe empty
  }

  property("agrees with the set of listed numbers for any split into segments") {
    val gen = for {
      numbers <- Gen.containerOf[Set, Long](Gen.choose(1L, 400L))
      signs <- Gen.listOfN(numbers.size, Gen.oneOf(1L, -1L))
      threshold <- Gen.choose(1, 6)
      queries <- Gen.listOf(Gen.choose(-420L, 420L))
    } yield (numbers.toSeq.sorted.zip(signs).map { case (n, s) => n * s }, threshold, queries)
    forAll(gen) { case (boxes, threshold, queries) =>
      val segmentCount = boxes.size / threshold
      val (inSegments, rest) = boxes.splitAt(segmentCount * threshold)
      val (row, history) = address(inSegments.grouped(threshold).toSeq, rest)
      val lookup = row.boxLookup(history.reader)
      val set = boxes.map(math.abs).toSet
      (queries ++ boxes).foreach { n =>
        lookup(n) shouldBe Some(set(math.abs(n)))
      }
      history.reads should be <= segmentCount
    }
  }
}
