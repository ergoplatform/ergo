package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.settings.{ErgoSettings, MonetarySettings, NodeConfigurationSettings}
import scorex.core.transaction.state.TransactionValidation
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import OrderedTxPool.weighted
import org.ergoplatform.nodeView.mempool.ErgoMemPool.SortingOption
import spire.syntax.all.cfor

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Random, Success, Try}


/**
  * Immutable memory pool implementation.
  *
  * @param pool     - Ordered transaction pool. Acts as related transaction storage, and is
  *                 used for implementing all transaction-related methods
  * @param stats    - Mempool statistics, that allows to track
  *                 information about mempool's state and transactions in it.
  * @param sortingOption - this input sets how transactions are sorted in the pool, by fee-per-byte or fee-per-cycle
  */
class ErgoMemPool private[mempool](pool: OrderedTxPool,
                                   private[mempool] val stats : MemPoolStatistics,
                                   private[mempool] val sortingOption: SortingOption)
                                  (implicit settings: ErgoSettings)
  extends ErgoMemPoolReader with ScorexLogging {

  import ErgoMemPool._
  import EmissionRules.CoinsInOneErgo

  /**
    * When there's no reason to re-check transactions immediately, we assign fake fee factor to them
    */
  private val FakeFeeFactor = 1000

  private val nodeSettings: NodeConfigurationSettings = settings.nodeSettings
  private implicit val monetarySettings: MonetarySettings = settings.chainSettings.monetary

  override def size: Int = pool.size

  override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = pool.get(modifierId)

  override def take(limit: Int): Iterable[ErgoTransaction] = pool.orderedTransactions.values.take(limit)

  def random(limit: Int): Iterable[ErgoTransaction] = {
    val result = mutable.WrappedArray.newBuilder[ErgoTransaction]
    val txSeq = pool.orderedTransactions.values.to[Vector]
    val total = txSeq.size
    val start = if (total <= limit) {
      0
    } else {
      val max = total - limit
      // max > 0 always
      scala.util.Random.nextInt(max)
    }

    cfor(start)(_ < Math.min(start + limit, total), _ + 1) { idx =>
      val tx = txSeq.apply(idx)
      result += tx
    }

    result.result()
  }

  override def getAll: Seq[ErgoTransaction] = pool.orderedTransactions.values.toSeq

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ids.flatMap(pool.get)

  /**
    * Returns all transactions resided in pool sorted by weight in descending order
    */
  override def getAllPrioritized: Seq[ErgoTransaction] = pool.orderedTransactions.values.toSeq

  /**
    * Method to put a transaction into the memory pool. Validation of the transactions against
    * the state is done in NodeVieHolder. This put() method can check whether a transaction is valid
    * @param tx
    * @return Success(updatedPool), if transaction successfully added to the pool, Failure(_) otherwise
    */
  def put(tx: ErgoTransaction): Try[ErgoMemPool] = put(Seq(tx))

  def put(txs: Iterable[ErgoTransaction]): Try[ErgoMemPool] = Try {
    putWithoutCheck(txs.filterNot(tx => pool.contains(tx.id)))
  }

  def putWithoutCheck(txs: Iterable[ErgoTransaction]): ErgoMemPool = {
    val updatedPool = txs.toSeq.distinct.foldLeft(pool) { case (acc, tx) => acc.put(tx, FakeFeeFactor) }
    new ErgoMemPool(updatedPool, stats, sortingOption)
  }

  def remove(tx: ErgoTransaction): ErgoMemPool = {
    val wtx = pool.transactionsRegistry.get(tx.id)
    val updStats = wtx.map(wgtx => stats.add(System.currentTimeMillis(), wgtx))
      .getOrElse(MemPoolStatistics(System.currentTimeMillis(), 0, System.currentTimeMillis()))
    new ErgoMemPool(pool.remove(tx), updStats, sortingOption)
  }

  def filter(condition: ErgoTransaction => Boolean): ErgoMemPool = {
    new ErgoMemPool(pool.filter(condition), stats, sortingOption)
  }

  def filter(txs: Seq[ErgoTransaction]): ErgoMemPool = filter(t => !txs.exists(_.id == t.id))

  /**
    * Invalidate transaction and delete it from pool
    *
    * @param tx - Transaction to invalidate
    */
  def invalidate(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(pool.invalidate(tx), stats, sortingOption)
  }

  /**
    * @return inputs spent by the mempool transactions
    */
  override def spentInputs: Iterator[BoxId] = pool.inputs.keysIterator

  // Check if transaction is double-spending inputs spent in the mempool.
  // If so, the new transacting is replacing older ones if it has bigger weight (fee/byte) than them on average.
  // Otherwise, the new transaction being rejected.
  private def acceptIfNoDoubleSpend(tx: ErgoTransaction, cost: Int): (ErgoMemPool, ProcessingOutcome) = {
    val feeFactor = sortingOption match {
      case SortingOption.FeePerByte => tx.size
      case SortingOption.FeePerCostUnit => cost
    }

    val doubleSpendingWtxs = tx.inputs.flatMap { inp =>
      pool.inputs.get(inp.boxId)
    }.toSet

    if (doubleSpendingWtxs.nonEmpty) {
      val ownWtx = weighted(tx, feeFactor)
      val doubleSpendingTotalWeight = doubleSpendingWtxs.map(_.weight).sum / doubleSpendingWtxs.size
      if (ownWtx.weight > doubleSpendingTotalWeight) {
        val doubleSpendingTxs = doubleSpendingWtxs.map(wtx => pool.orderedTransactions(wtx)).toSeq
        new ErgoMemPool(pool.put(tx, feeFactor).remove(doubleSpendingTxs), stats, sortingOption) -> ProcessingOutcome.Accepted
      } else {
        this -> ProcessingOutcome.DoubleSpendingLoser(doubleSpendingWtxs.map(_.id))
      }
    } else {
      val poolSizeLimit = nodeSettings.mempoolCapacity
      if (pool.size == poolSizeLimit &&
        weighted(tx, feeFactor).weight <= pool.orderedTransactions.lastKey.weight) {
        this -> ProcessingOutcome.Declined(new Exception("Transaction pays less than any other in the pool being full"))
      } else {
        new ErgoMemPool(pool.put(tx, feeFactor), stats, sortingOption) -> ProcessingOutcome.Accepted
      }
    }
  }

  def process(tx: ErgoTransaction, state: ErgoState[_]): (ErgoMemPool, ProcessingOutcome) = {
    log.info(s"Processing mempool transaction: $tx")

    val blacklistedTransactions = nodeSettings.blacklistedTransactions
    if (blacklistedTransactions.nonEmpty && blacklistedTransactions.contains(tx.id)) {
      new ErgoMemPool(pool.invalidate(tx), stats, sortingOption) -> ProcessingOutcome.Invalidated(new Exception("blacklisted tx"))
    } else {
      val fee = extractFee(tx)
      val minFee = settings.nodeSettings.minimalFeeAmount
      val canAccept = pool.canAccept(tx)

      if (fee >= minFee) {
        if (canAccept) {
          val costLimit = nodeSettings.maxTransactionCost
          state match {
            case utxo: UtxoState =>
              // Allow proceeded transaction to spend outputs of pooled transactions.
              val utxoWithPool = utxo.withTransactions(getAll)
              if (tx.inputIds.forall(inputBoxId => utxoWithPool.boxById(inputBoxId).isDefined)) {
                utxoWithPool.validateWithCost(tx, Some(utxo.stateContext), costLimit, None) match {
                  case Success(cost) => acceptIfNoDoubleSpend(tx, cost)
                  case Failure(ex) => new ErgoMemPool (pool.invalidate (tx), stats, sortingOption) -> ProcessingOutcome.Invalidated (ex)
                }
              } else {
                this -> ProcessingOutcome.Declined(new Exception("not all utxos in place yet"))
              }
            case validator: TransactionValidation =>
              // transaction validation currently works only for UtxoState, so this branch currently
              // will not be triggered probably
              validator.validateWithCost(tx, costLimit) match {
                case Success(cost) => acceptIfNoDoubleSpend(tx, cost)
                case Failure(ex) => new ErgoMemPool(pool.invalidate(tx), stats, sortingOption) -> ProcessingOutcome.Invalidated(ex)
              }
            case _ =>
              // Accept transaction in case of "digest" state. Transactions are not downloaded in this mode from other
              // peers though, so such transactions can come from the local wallet only.
              //
              // We pass fake cost in this case, as there's no real competition between local transactions only anyway
              acceptIfNoDoubleSpend(tx, cost = FakeFeeFactor)
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
  }

  def weightedTransactionIds(limit: Int): Seq[WeightedTxId] = pool.orderedTransactions.keysIterator.take(limit).toSeq

  private def extractFee(tx: ErgoTransaction): Long = {
    tx.outputs
      .filter(_.ergoTree == settings.chainSettings.monetary.feeProposition)
      .map(_.value)
      .sum
  }

  /**
    * Get average fee for the specified wait time interval
    *
    * @param expectedWaitTimeMinutes - maximal amount of time for which transaction can be kept in mempool
    * @param txSize                  - size of transaction (in bytes)
    * @return recommended fee value for transaction to be proceeded in specified time
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

  /**
    * @return read-only copy of this history
    */
  def getReader: ErgoMemPoolReader = this
}

object ErgoMemPool extends ScorexLogging {

  /**
   * Hierarchy of sorting strategies for mempool transactions
   */
  sealed trait SortingOption

  object SortingOption {
    /**
      * Sort transactions by fee paid for transaction size, so fee/byte
      */
    case object FeePerByte extends SortingOption

    /**
      * Sort transactions by fee paid for transaction contracts validation cost, so fee/execution unit
      */
    case object FeePerCostUnit extends SortingOption

    /**
      * @return randomly chosen mempool sorting strategy
      */
    def random(): SortingOption = {
      if (Random.nextBoolean()) {
        log.info("Sorting mempool by fee-per-byte")
        FeePerByte
      } else {
        log.info("Sorting mempool by fee-per-cycle")
        FeePerCostUnit
      }
    }
  }


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

  /**
    * Create empty mempool
    * @param settings - node settings (to get mempool settings from)
    * @param sortingOption - how to sort transactions (by size or execution cost)
    * @return empty mempool
    */
  def empty(settings: ErgoSettings, sortingOption: SortingOption = SortingOption.random()): ErgoMemPool =
    new ErgoMemPool(OrderedTxPool.empty(settings),
      MemPoolStatistics(System.currentTimeMillis(), 0, System.currentTimeMillis()),
      sortingOption
    )(settings)

}
