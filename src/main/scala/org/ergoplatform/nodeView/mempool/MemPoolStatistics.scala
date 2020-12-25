package org.ergoplatform.nodeView.mempool

import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId


/**
  * Immutable implementation of mempool statistics
  *
  * @param startMeasurement - start of measurement interval
  * @param takenTxns        - amount of taken transaction since start of measurement
  * @param snapTime         - last snapshot time
  * @param snapTakenTxns    - amount of transaction at the moment of last snapshot
  */
case class MemPoolStatistics(startMeasurement: Long,
                             takenTxns: Long = 0,
                             snapTime: Long,
                             snapTakenTxns: Long = 0,
                             histogram: List[FeeHistogramBin] =
                             List.fill(MemPoolStatistics.nHistogramBins)(FeeHistogramBin(0, 0))) {

  /**
    * Add new entry to mempool statistics. This method is called when transaction is taken from mempool
    * and placed in blockchain. It is not called when transaction in thrown away from the pool by replacement policy.
    * To make statistic better represent most recent system behaviour, we periodically (each measurementIntervalMsec)
    * prune statistic. To avoid siutation when we do not have statistic at all, we actually keep data up to
    * 2*measurementIntervalMsec and periodically cut half of range.
    */
  def add(currTime: Long, wtx: WeightedTxId): MemPoolStatistics = {
    val curTakenTx = takenTxns + 1
    val (newTakenTx, newMeasurement, newSnapTxs, newSnapTime) =
      if (currTime - snapTime > MemPoolStatistics.measurementIntervalMsec) {
        if (snapTakenTxns != 0) {
          (curTakenTx - snapTakenTxns, snapTime, curTakenTx, currTime)
        } else {
          (curTakenTx, startMeasurement, curTakenTx, currTime)
        }
      } else {
        (curTakenTx, startMeasurement, snapTakenTxns, snapTime)
      }
    val durationMinutes = ((currTime - wtx.created) / (60 * 1000)).toInt
    val newHist =
      if (durationMinutes < MemPoolStatistics.nHistogramBins) {
        val (histx, hisfee) = (histogram(durationMinutes).nTxns + 1, histogram(durationMinutes).totalFee + wtx.feePerKb)
        histogram.updated(durationMinutes, FeeHistogramBin(histx, hisfee))
      } else {
        histogram
      }
    MemPoolStatistics(newMeasurement, newTakenTx, newSnapTime, newSnapTxs, newHist)
  }
}

object MemPoolStatistics {
  // Time parameters of mempool statistics
  val nHistogramBins: Int = 60  /* one hour */
  val measurementIntervalMsec: Int = 60 * 1000 /* one hour */
}
