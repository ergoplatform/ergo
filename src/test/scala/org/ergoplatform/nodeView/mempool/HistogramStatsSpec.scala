package org.ergoplatform.nodeView.mempool

import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.bytesToId

class HistogramStatsSpec extends AnyFlatSpec with Matchers {

  "Fee histogram" should "put all transactions in the overflow bin for a zero range" in {
    val now = 10000L
    val futureTx = WeightedTxId(bytesToId(Array.fill(32)(7.toByte)), 1L, 7L, now + 1000L)
    val oldTx = WeightedTxId(bytesToId(Array.fill(32)(8.toByte)), 1L, 11L, now - 1000L)

    HistogramStats.getFeeHistogram(now, 2, 0L, Seq(futureTx, oldTx)).toSeq shouldBe Seq(
      FeeHistogramBin(0, 0L),
      FeeHistogramBin(0, 0L),
      FeeHistogramBin(2, 18L)
    )
  }

  it should "place future-created transactions in the first bin" in {
    val now = 10000L
    val futureTx = WeightedTxId(bytesToId(Array.fill(32)(1.toByte)), 1L, 7L, now + 1000L)
    val oldTx = WeightedTxId(bytesToId(Array.fill(32)(2.toByte)), 1L, 11L, now - 1000L)

    HistogramStats.getFeeHistogram(now, 2, 1000L, Seq(futureTx, oldTx)).toSeq shouldBe Seq(
      FeeHistogramBin(1, 7L),
      FeeHistogramBin(0, 0L),
      FeeHistogramBin(1, 11L)
    )
  }

  it should "keep non-divisible intervals inside the documented bins" in {
    val now = 100L
    val fourMsOld = WeightedTxId(bytesToId(Array.fill(32)(3.toByte)), 1L, 7L, now - 4L)
    HistogramStats.getFeeHistogram(now, 3, 5L, Seq(fourMsOld)).toSeq shouldBe Seq(
      FeeHistogramBin(0, 0L),
      FeeHistogramBin(0, 0L),
      FeeHistogramBin(1, 7L),
      FeeHistogramBin(0, 0L)
    )

    val twoMsOld = WeightedTxId(bytesToId(Array.fill(32)(4.toByte)), 1L, 11L, now - 2L)
    HistogramStats.getFeeHistogram(now, 2, 3L, Seq(twoMsOld)).toSeq shouldBe Seq(
      FeeHistogramBin(0, 0L),
      FeeHistogramBin(1, 11L),
      FeeHistogramBin(0, 0L)
    )
  }

  it should "preserve the last regular bin when scaling a large wait time" in {
    val nearlyMaxAge = WeightedTxId(bytesToId(Array.fill(32)(5.toByte)), 1L, 13L, 1L)
    val histogram = HistogramStats.getFeeHistogram(Long.MaxValue, 1000, Long.MaxValue, Seq(nearlyMaxAge))
    histogram(999) shouldBe FeeHistogramBin(1, 13L)
    histogram(1000) shouldBe FeeHistogramBin(0, 0L)
  }

  it should "handle a positive range shorter than the number of bins" in {
    val justCreated = WeightedTxId(bytesToId(Array.fill(32)(6.toByte)), 1L, 17L, 100L)
    val histogram = HistogramStats.getFeeHistogram(100L, 1000, 1L, Seq(justCreated))
    histogram(0) shouldBe FeeHistogramBin(1, 17L)
    histogram(1000) shouldBe FeeHistogramBin(0, 0L)
  }
}
