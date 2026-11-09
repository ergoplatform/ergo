package org.ergoplatform.nodeView.mempool

import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.bytesToId

class MemPoolStatisticsSpec extends AnyFlatSpec with Matchers {

  "Mempool statistics" should "rebase both counts across successive snapshots" in {
    val start = 1000000L
    val interval = MemPoolStatistics.measurementIntervalMsec.toLong

    def removed(id: Int): WeightedTxId =
      WeightedTxId(bytesToId(Array.fill[Byte](32)(id.toByte)), 1L, 1L, start)

    def snapshot(stats: MemPoolStatistics): (Long, Long, Long, Long) =
      (stats.takenTxns, stats.snapTakenTxns, stats.startMeasurement, stats.snapTime)

    val empty = MemPoolStatistics(start, 0L, start)
    val beforeFirst = empty.add(start + 1000L, removed(1))
    snapshot(beforeFirst) shouldBe ((1L, 0L, start, start))

    val atBoundary = beforeFirst.add(start + interval, removed(2))
    snapshot(atBoundary) shouldBe ((2L, 0L, start, start))

    val first = atBoundary.add(start + interval + 1L, removed(3))
    snapshot(first) shouldBe ((3L, 3L, start, start + interval + 1L))

    val withinWindow = first.add(start + interval + 2L, removed(4))
    snapshot(withinWindow) shouldBe ((4L, 3L, start, start + interval + 1L))

    val second = withinWindow.add(start + 2L * interval + 2L, removed(5))
    snapshot(second) shouldBe ((2L, 2L, start + interval + 1L,
      start + 2L * interval + 2L))

    val third = second.add(start + 3L * interval + 3L, removed(6))
    snapshot(third) shouldBe ((1L, 1L, start + 2L * interval + 2L,
      start + 3L * interval + 3L))
  }
}
