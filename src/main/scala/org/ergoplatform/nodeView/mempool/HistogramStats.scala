package org.ergoplatform.nodeView.mempool

object HistogramStats {

  def getFeeHistogram(currTime: Long, nBins : Int, maxWaitTimeMsec: Long, wtxs : Seq[(Long,Long)]): Array[FeeHistogramBin] = {
    val histogram = Array.fill(nBins + 1)(FeeHistogramBin(0,0))
    val interval = maxWaitTimeMsec / nBins
    for (wtx <- wtxs) {
      val waitTime = currTime - wtx._1
      val bin = if (waitTime < maxWaitTimeMsec) (waitTime/interval).toInt else nBins
      histogram.update(bin, FeeHistogramBin(histogram(bin).nTxns + 1, histogram(bin).totalFee + wtx._2))
    }
    histogram
  }
}
