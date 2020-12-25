package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBox.BoxId
import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.settings.{ErgoSettings, MonetarySettings}
import scorex.core.transaction.MemoryPool
import scorex.core.transaction.state.TransactionValidation
import scorex.util.{ModifierId, bytesToId}
import OrderedTxPool.weighted

import scala.annotation.tailrec
import scala.util.Try

/**
  *  Time parameters of mempool statistics
  */
object MemPoolStatisticsParams {
  val nHistogramBins = 60  /* one hour */
  val measurementIntervalMsec = 60 * 1000 /* one hour */
}

case class FeeHistogramBin(nTxns: Int, totalFee: Long)

object FeeHistogramBin {

  implicit val encodeHistogramBin: Encoder[FeeHistogramBin] = (bin: FeeHistogramBin) => Json.obj(
    ("nTxns", bin.nTxns.asJson),
    ("totalFee", bin.totalFee.asJson)
  )
}

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
                             List.fill(MemPoolStatisticsParams.nHistogramBins)(FeeHistogramBin(0, 0))) {

  /**
    * Add new entry to mempool statistics. This method is called when transaction is taken from mempool
    * and placed in blockchain. It is not called when transaction in thrown away from the pool by replacement policy.
    * To make statistic better represent most recent system behaviour, we periodically (each measurementIntervalMsec)
    * prune statistic. To avoid situtation when we do not have statistic at all, we actually keep data up to
    * 2*measurementIntervalMsec and periodically cut half of range.
    */
  def add(currTime: Long, wtx: WeightedTxId): MemPoolStatistics = {
    val curTakenTx = takenTxns + 1
    val (newTakenTx, newMeasurement, newSnapTxs, newSnapTime) =
      if (currTime - snapTime > MemPoolStatisticsParams.measurementIntervalMsec) {
        if (snapTakenTxns != 0) (curTakenTx - snapTakenTxns, snapTime, curTakenTx, currTime)
        else (curTakenTx, startMeasurement, curTakenTx, currTime)
      }
      else (curTakenTx, startMeasurement, snapTakenTxns, snapTime)
    val durationMinutes = ((currTime - wtx.created) / (60 * 1000)).toInt
    val newHist =
      if (durationMinutes < MemPoolStatisticsParams.nHistogramBins) {
        val (histx, hisfee) = (histogram(durationMinutes).nTxns + 1, histogram(durationMinutes).totalFee + wtx.feePerKb)
        histogram.updated(durationMinutes, FeeHistogramBin(histx, hisfee))
      }
      else histogram
    MemPoolStatistics(newMeasurement, newTakenTx, newSnapTime, newSnapTxs, newHist)
  }
}

/**
  * Immutable memory pool implementation.
  *
  * @param pool     - Ordered transaction pool
  * @param stats    - Mempool statistics
  * @param settings - Ergo settings
  */
class ErgoMemPool private[mempool](pool: OrderedTxPool, private[mempool] val stats : MemPoolStatistics)(implicit settings: ErgoSettings)
  extends MemoryPool[ErgoTransaction, ErgoMemPool] with ErgoMemPoolReader {

  import ErgoMemPool._
  import EmissionRules.CoinsInOneErgo

  private implicit val monetarySettings: MonetarySettings = settings.chainSettings.monetary

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
    new ErgoMemPool(updatedPool, stats)
  }

  override def remove(tx: ErgoTransaction): ErgoMemPool = {
    val wtx = pool.transactionsRegistry.get(tx.id)
    new ErgoMemPool(pool.remove(tx), wtx.map(wgtx => stats
      .add(System.currentTimeMillis(), wgtx))
      .getOrElse(MemPoolStatistics(System.currentTimeMillis(), 0, System.currentTimeMillis())))
  }

  override def filter(condition: ErgoTransaction => Boolean): ErgoMemPool = {
    new ErgoMemPool(pool.filter(condition), stats)
  }

  /**
    * Invalidate transaction and delete it from pool
    *
    * @param tx - Transaction to invalidate
    */
  def invalidate(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(pool.invalidate(tx), stats)
  }

  /**
    * @return inputs spent by the mempool transactions
    */
  override def spentInputs: Iterator[BoxId] = pool.inputs.keysIterator

  // Check if transaction is double-spending inputs spent in the mempool.
  // If so, the new transacting is replacing older ones if it has bigger weight (fee/byte) than them on average.
  // Otherwise, the new transaction being rejected.
  private def acceptIfNoDoubleSpend(tx: ErgoTransaction): (ErgoMemPool, ProcessingOutcome) = {
    val doubleSpendingWtxs = tx.inputs.flatMap { inp =>
      pool.inputs.get(inp.boxId)
    }.toSet

    if(doubleSpendingWtxs.nonEmpty) {
      val ownWtx = weighted(tx)
      val doubleSpendingTotalWeight = doubleSpendingWtxs.map(_.weight).sum / doubleSpendingWtxs.size
      if (ownWtx.weight > doubleSpendingTotalWeight) {
        val doubleSpendingTxs = doubleSpendingWtxs.map(wtx => pool.orderedTransactions(wtx)).toSeq
        new ErgoMemPool(pool.put(tx).remove(doubleSpendingTxs), stats) -> ProcessingOutcome.Accepted
      } else {
        this -> ProcessingOutcome.DoubleSpendingLoser(doubleSpendingWtxs.map(_.id))
      }
    } else {
      new ErgoMemPool(pool.put(tx), stats) -> ProcessingOutcome.Accepted
    }
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
              _ => acceptIfNoDoubleSpend(tx)
            )
          case validator: TransactionValidation[ErgoTransaction@unchecked] =>
            // transaction validation currently works only for UtxoState, so this branch currently
            // will not be triggered probably
            validator.validate(tx).fold(
              new ErgoMemPool(pool.invalidate(tx), stats) -> ProcessingOutcome.Invalidated(_),
              _ => acceptIfNoDoubleSpend(tx)
            )
          case _ =>
            // Accept transaction in case of "digest" state. Transactions are not downloaded in this mode from other
            // peers though, so such transactions can come from the local wallet only.
            acceptIfNoDoubleSpend(tx)
        }
      } else {
        this -> ProcessingOutcome.Declined(
          new Exception(s"Pool can not accept transaction ${tx.id}, it is invalidated earlier or the pool is full"))
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
    *
    * @param expectedWaitTimeMinutes - maximal amount of time for which transaction can be kept in mempool
    * @param txSize                  - size of transaction (in bytes)
    *  @return recommended fee value for transaction to be proceeded in specified time
    */
  def getRecommendedFee(expectedWaitTimeMinutes: Int, txSize: Int): Long = {
    @tailrec def loop(waitMinutes: Int): Option[Long] =
      Try(stats.histogram(waitMinutes)).toOption match {
        case Some(bin) if bin.nTxns != 0 => Some(bin.totalFee / bin.nTxns * txSize / 1024)
        case _ if waitMinutes < expectedWaitTimeMinutes => loop(waitMinutes + 1)
        case _ => None
      }

    loop(waitMinutes = 0).getOrElse(settings.nodeSettings.minimalFeeAmount)
  }

  /**
    * Calculate position in mempool corresponding to the specified fee and
    * estimate time of serving this transaction based on average rate of placing
    * transactions in blockchain
    * @param txFee  - transaction fee
    * @param txSize - size of transaction (in bytes)
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

    /**
      * Object signalling that a transaction is accepted to the memory pool
      */
    case object Accepted extends ProcessingOutcome

    /**
      * Class signalling that a valid transaction was rejected as it is double-spending inputs of mempool transactions
      * and has no bigger weight (fee/byte) than them on average.
      * @param winnerTxIds - identifiers of transactions won in replace-by-fee auction
      */
    case class DoubleSpendingLoser(winnerTxIds: Set[ModifierId]) extends ProcessingOutcome

    /**
      * Class signalling that a transaction declined from being accepted into the memory pool
      */
    case class Declined(e: Throwable) extends ProcessingOutcome


    /**
      * Class signalling that a transaction turned out to be invalid when checked in the mempool
      */
    case class Invalidated(e: Throwable) extends ProcessingOutcome

  }

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[ErgoTransaction]

  /**
    * Create empty mempool
    * @param settings - node settings (to get mempool settings from)
    * @return empty mempool
    */
  def empty(settings: ErgoSettings): ErgoMemPool =
    new ErgoMemPool(OrderedTxPool.empty(settings),
      MemPoolStatistics(System.currentTimeMillis(), 0, System.currentTimeMillis()))(settings)

}
