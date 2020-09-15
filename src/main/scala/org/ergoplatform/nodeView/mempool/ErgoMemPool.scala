package org.ergoplatform.nodeView.mempool

import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.transaction.MemoryPool
import scorex.core.transaction.state.TransactionValidation
import scorex.util.{ModifierId, bytesToId}

import scala.util.Try

object MemPoolStatisticsParams {
  val nHistogramBins = 60  /* one hour */
  val measurementIntervalMsec = 60 * 1000 /* one hour */
}

case class FeeHistogramBin(var nTxns: Int, var totalFee: Long)

case class MemPoolStatistics() {
  var startMeasurement: Long = System.currentTimeMillis() // start of measurement interval
  var takenTxns : Long = 0              // amount of taken transaction since start of measurement
  var snapTime: Long = startMeasurement // last snapshot time
  var snapTakenTxns : Long = 0          // amount of transaction at the moment of last snapshot
  val histogram = Array.fill(MemPoolStatisticsParams.nHistogramBins)(FeeHistogramBin(0,0))

  //                  <.............takenTxns...................
  //                                        <....snapTakenTxns..
  // ------------------------------------------------------------> time
  //                  ^                     ^                  ^
  //                  |<measurementInterval>|                  |
  // startMeasurement +            snapTime +      current time+

  /**
    * Add new entry to mempool statistics. This method is called when transaction is taken from mempool
    * and placed in blockchain. It is not called when transaction in thrown away from the pool by replacement policy.
    * To make statistic better represent most recent system behaviour, we periodically (each measurementIntervalMsec)
    * prune statistic. To avoid situtation when we do not have statistic at all, we actually keep data up to
    * 2*measurementIntervalMsec and periodically cut half of range.
    */
  def add(wtx : WeightedTxId) : Unit = {
    val now = System.currentTimeMillis()
    takenTxns += 1
    if (now - snapTime > MemPoolStatisticsParams.measurementIntervalMsec) {
      if (snapTakenTxns != 0) { // snapshot was taken (it is always true for time > measurementIntervalMsec)
        takenTxns -= snapTakenTxns // cut-of statistics
        startMeasurement = snapTime
      }
      snapTakenTxns = takenTxns // create new snapshot
      snapTime = now
    }
    // update histogram of average fee for wait time interval
    val durationMinutes = ((now - wtx.created)/(60*1000)).asInstanceOf[Int]
    if (durationMinutes < MemPoolStatisticsParams.nHistogramBins) {
      histogram(durationMinutes).nTxns += 1
      histogram(durationMinutes).totalFee += wtx.feePerKb
    }
  }
}
/**
  * Immutable memory pool implementation.
  */
class ErgoMemPool private[mempool](pool: OrderedTxPool, stats : MemPoolStatistics)(implicit settings: ErgoSettings)
  extends MemoryPool[ErgoTransaction, ErgoMemPool] with ErgoMemPoolReader {

  import ErgoMemPool._
  import EmissionRules.CoinsInOneErgo

  override type NVCT = ErgoMemPool

  override def size: Int = pool.size

  override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = pool.get(modifierId)

  override def take(limit: Int): Iterable[ErgoTransaction] = pool.orderedTransactions.values.take(limit)

  override def getAll: Seq[ErgoTransaction] = pool.orderedTransactions.values.toSeq

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ids.flatMap(pool.get)

  /**
    * Returns all transactions resided in pool sorted by weight in descending order
    */
  override def getAllPrioritized: Seq[ErgoTransaction] = pool.orderedTransactions.values.toSeq

  override def put(tx: ErgoTransaction): Try[ErgoMemPool] = put(Seq(tx))

  override def put(txs: Iterable[ErgoTransaction]): Try[ErgoMemPool] = Try {
    putWithoutCheck(txs.filterNot(tx => pool.contains(tx.id)))
  }

  override def putWithoutCheck(txs: Iterable[ErgoTransaction]): ErgoMemPool = {
    val updatedPool = txs.toSeq.distinct.foldLeft(pool) { case (acc, tx) => acc.put(tx) }
    new ErgoMemPool(updatedPool,stats)
  }

  override def remove(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(pool.remove(tx, (wtx:WeightedTxId) => stats.add(wtx)),stats)
  }

  override def filter(condition: ErgoTransaction => Boolean): ErgoMemPool = {
    new ErgoMemPool(pool.filter(condition),stats)
  }

  def invalidate(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(pool.invalidate(tx),stats)
  }

  def process(tx: ErgoTransaction, state: ErgoState[_]): (ErgoMemPool, ProcessingOutcome) = {
    val fee = extractFee(tx)
    val minFee = settings.nodeSettings.minimalFeeAmount
    val canAccept = pool.canAccept(tx)

    if (fee >= minFee) {
      if (canAccept) {
        state match {
          case utxo: UtxoState =>
            // Allow proceeded transaction to spend outputs of pooled transactions.
            utxo.withTransactions(getAll).validate(tx).fold(
              new ErgoMemPool(pool.invalidate(tx), stats) -> ProcessingOutcome.Invalidated(_),
              _ => new ErgoMemPool(pool.put(tx), stats) -> ProcessingOutcome.Accepted
            )
          case validator: TransactionValidation[ErgoTransaction@unchecked] =>
            // transaction validation currently works only for UtxoState, so this branch currently
            // will not be triggered probably
            validator.validate(tx).fold(
              new ErgoMemPool(pool.invalidate(tx), stats) -> ProcessingOutcome.Invalidated(_),
              _ => new ErgoMemPool(pool.put(tx), stats) -> ProcessingOutcome.Accepted
            )
          case _ =>
            this -> ProcessingOutcome.Declined(
              new Exception("Transaction validation not supported"))
        }
      } else {
        this -> ProcessingOutcome.Declined(
          new Exception(s"Pool can not accept transaction ${tx.id}, it is invalidated earlier or pool is full"))
      }
    } else {
      this -> ProcessingOutcome.Declined(
        new Exception(s"Min fee not met: ${minFee.toDouble / CoinsInOneErgo} ergs required, " +
          s"${fee.toDouble / CoinsInOneErgo} ergs given")
      )
    }
  }

  def weightedTransactionIds(limit: Int): Seq[WeightedTxId] = pool.orderedTransactions.keysIterator.take(limit).toSeq

  private def extractFee(tx: ErgoTransaction): Long =
    ErgoState.boxChanges(Seq(tx))._2
      .filter(_.ergoTree == settings.chainSettings.monetary.feeProposition)
      .map(_.value)
      .sum

  /**
    * Get average fee for the specified wait time interval
    * @param expectedWaitTimeMinutes maximal amount of time for which transaction can be kept in mempool
    * @param txSize size of transaction (in bytes)
    *  @return recommended fee value for transaction to be proceeded in specified time
    */
  def getRecommendedFee(expectedWaitTimeMinutes : Int, txSize : Int) : Long = {
    if (expectedWaitTimeMinutes < MemPoolStatisticsParams.nHistogramBins) {
      // locate first non-empty histogram bin preceding or equal to the specified wait time
      for (i <- expectedWaitTimeMinutes to 0 by -1) {
        val h = stats.histogram(expectedWaitTimeMinutes)
        if (h.nTxns != 0) {
          return h.totalFee / h.nTxns * txSize / 1024
        }
      }
    }
    settings.nodeSettings.minimalFeeAmount
  }

  /**
    * Calculate position in mempool corresponding to the specified fee and
    * estimate time of serving this transaction based on average rate of placing
    * transactions in blockchain
    * @param txFee transaction fee
    * @param txSize size of transaction (in bytes)
    * @return average time for this transaction to be placed in block
    */
  def getExpectedWaitTime(txFee : Long, txSize : Int): Long  = {
    // Create dummy transaction entry
    val feePerKb = txFee * 1024 / txSize
    val dummyModifierId = bytesToId(Array.fill(32)(0.toByte))
    val wtx = WeightedTxId(dummyModifierId, feePerKb, feePerKb, 0)

    // Find position of entry in mempool
    val posInPool = pool.orderedTransactions.keySet.until(wtx).size

    // Time since statistics measurement interval (needed to calculate average tx rate)
    val elapsed = System.currentTimeMillis() - stats.startMeasurement
    if (stats.takenTxns != 0) {
      elapsed * posInPool / stats.takenTxns
    } else {
      0
    }
  }
}

object ErgoMemPool {

  sealed trait ProcessingOutcome

  object ProcessingOutcome {

    case object Accepted extends ProcessingOutcome

    case class Declined(e: Throwable) extends ProcessingOutcome

    case class Invalidated(e: Throwable) extends ProcessingOutcome

  }

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[ErgoTransaction]

  def empty(settings: ErgoSettings): ErgoMemPool =
    new ErgoMemPool(OrderedTxPool.empty(settings), new MemPoolStatistics())(settings)

}
