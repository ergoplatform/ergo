package org.ergoplatform.nodeView.mempool

import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Try
import scorex.util.bytesToId

class MemPoolStatisticsClockSpec extends AnyFlatSpec with Matchers {

  private val insertedAt = 1000000L
  private val minute = 60000L
  private val weighted = WeightedTxId(
    bytesToId(Array.fill[Byte](32)(1.toByte)), 1L, 1L, insertedAt)

  private def removedAt(currTime: Long): MemPoolStatistics =
    MemPoolStatistics(currTime, 0L, currTime).add(currTime, weighted)

  "Mempool statistics" should "not fail removal accounting after a backward minute" in {
    val justBefore = Try(removedAt(insertedAt - minute + 1L))
    justBefore.isSuccess shouldBe true

    val atMinute = Try(removedAt(insertedAt - minute))
    withClue(s"removal accounting failed: $atMinute") {
      atMinute.isSuccess shouldBe true
    }
  }

  it should "omit negative residence times without losing removal counts" in {
    val shortBackstep = removedAt(insertedAt - 1L)
    val fullBackstep = removedAt(insertedAt - minute)

    Seq(shortBackstep, fullBackstep).foreach { stats =>
      stats.takenTxns shouldBe 1L
      stats.histogram shouldBe MemPoolStatistics.defaultPoolHistogram
    }
  }

  it should "omit wrapped elapsed times in both directions without losing counts" in {
    val forwardWrap = MemPoolStatistics(Long.MaxValue, 0L, Long.MaxValue)
      .add(Long.MaxValue, weighted.copy(created = Long.MinValue))
    forwardWrap.takenTxns shouldBe 1L
    forwardWrap.histogram shouldBe MemPoolStatistics.defaultPoolHistogram

    val backwardWrap = MemPoolStatistics(Long.MinValue, 0L, Long.MinValue)
      .add(Long.MinValue, weighted.copy(created = Long.MaxValue))
    backwardWrap.takenTxns shouldBe 1L
    backwardWrap.histogram shouldBe MemPoolStatistics.defaultPoolHistogram
  }

  it should "preserve earlier fee samples when a later clock reading is invalid" in {
    val priorTx = WeightedTxId(
      bytesToId(Array.fill[Byte](32)(2.toByte)), 1L, 7L, insertedAt)
    val prior = MemPoolStatistics(insertedAt, 0L, insertedAt).add(insertedAt, priorTx)
    prior.histogram.head.nTxns shouldBe 1
    prior.histogram.head.totalFee shouldBe 7L

    val after = prior.add(insertedAt - 1L, weighted)
    after.takenTxns shouldBe prior.takenTxns + 1L
    after.histogram shouldBe prior.histogram
  }

  it should "retain the valid duration bins through the one-hour boundary" in {
    val zero = removedAt(insertedAt)
    zero.takenTxns shouldBe 1L
    zero.histogram.head.nTxns shouldBe 1

    val beforeHour = removedAt(insertedAt + 60L * minute - 1L)
    beforeHour.takenTxns shouldBe 1L
    beforeHour.histogram(59).nTxns shouldBe 1

    val atHour = removedAt(insertedAt + 60L * minute)
    atHour.takenTxns shouldBe 1L
    atHour.histogram shouldBe MemPoolStatistics.defaultPoolHistogram
  }
}
