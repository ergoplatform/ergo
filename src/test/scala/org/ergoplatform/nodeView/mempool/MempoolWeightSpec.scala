package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.OrderedTxPool.{WeightedTxId, feeForFactor, saturatedAverage, saturatingAdd}
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MempoolWeightSpec extends AnyFlatSpec
  with Matchers
  with ErgoTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val monetarySettings = settings.chainSettings.monetary

  it should "keep high fee weights at top priority when precision scaling would overflow" in {
    val feeOut = new ErgoBoxCandidate(Long.MaxValue / 1024 + 1, feeProp, creationHeight = 0)
    val tx = ErgoTransaction(IndexedSeq.empty, IndexedSeq(feeOut))

    val weightedTx = OrderedTxPool.weighted(tx, feeFactor = 1)(monetarySettings)

    weightedTx.feePerFactor shouldBe Long.MaxValue
    weightedTx.weight shouldBe Long.MaxValue
  }

  it should "saturate mempool weight arithmetic" in {
    saturatingAdd(Long.MaxValue - 1, 2) shouldBe Long.MaxValue
    saturatedAverage(Seq(Long.MaxValue, Long.MaxValue)) shouldBe Long.MaxValue
    feeForFactor(Long.MaxValue, 2048) shouldBe Long.MaxValue
  }

  it should "saturate fee histogram totals" in {
    val now = System.currentTimeMillis()
    val tx = ErgoTransaction(IndexedSeq.empty, IndexedSeq(new ErgoBoxCandidate(1L, feeProp, creationHeight = 0)))
    val wtx = WeightedTxId(tx.id, Long.MaxValue, Long.MaxValue, now)

    val stats = MemPoolStatistics(
      startMeasurement = now,
      snapTime = now,
      histogram = FeeHistogramBin(0, Long.MaxValue - 1) :: MemPoolStatistics.defaultPoolHistogram.tail
    ).add(now, wtx)
    stats.histogram.head.totalFee shouldBe Long.MaxValue

    val histogram = HistogramStats.getFeeHistogram(now, nBins = 1, maxWaitTimeMsec = 60000L, Seq(wtx, wtx))
    histogram.head.totalFee shouldBe Long.MaxValue
  }
}
