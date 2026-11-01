package org.ergoplatform.nodeView.mempool

import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId

object HistogramStats {

  def getFeeHistogram(currTime: Long, nBins : Int, maxWaitTimeMsec: Long, wtxs : Seq[WeightedTxId]): Array[FeeHistogramBin] = {
    val histogram = Array.fill(nBins + 1)(FeeHistogramBin(0,0))
    for (wtx <- wtxs) {
      val waitTime = Math.max(0L, currTime - wtx.created)
      val bin = if (waitTime >= maxWaitTimeMsec) {
        nBins
      } else if (waitTime <= Long.MaxValue / nBins) {
        (waitTime * nBins / maxWaitTimeMsec).toInt
      } else {
        ((BigInt(waitTime) * nBins) / maxWaitTimeMsec).toInt
      }
      histogram.update(bin, FeeHistogramBin(histogram(bin).nTxns + 1, histogram(bin).totalFee + wtx.feePerFactor))
    }
    histogram
  }
}
