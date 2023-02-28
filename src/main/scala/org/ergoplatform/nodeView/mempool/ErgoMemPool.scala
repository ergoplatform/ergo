package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool.SortingOption
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.settings.{ErgoSettings, MonetarySettings, NodeConfigurationSettings}
import scorex.core.transaction.state.TransactionValidation
import scorex.util.{ModifierId, ScorexLogging}
import spire.syntax.all.cfor

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Random, Success, Try}


/**
  * Immutable memory pool implementation.
  *
  * @param pool     - Ordered transaction pool. Acts as transactions storage,
  *                  used for implementing all transaction-related methods
  * @param stats    - Mempool statistics, that allows to track
  *                 information about mempool's state and transactions in it.
  * @param sortingOption - this input sets how transactions are sorted in the pool, by fee-per-byte or fee-per-cycle
  */
class ErgoMemPool private[mempool](private[mempool] val pool: OrderedTxPool,
                                   private[mempool] val stats: MemPoolStatistics,
                                   private[mempool] val sortingOption: SortingOption)
                                  (implicit settings: ErgoSettings)
  extends ErgoMemPoolReader with ScorexLogging {

  import EmissionRules.CoinsInOneErgo
  import ErgoMemPool._


  private val nodeSettings: NodeConfigurationSettings = settings.nodeSettings
  private implicit val monetarySettings: MonetarySettings = settings.chainSettings.monetary

  override def size: Int = pool.size

  override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = {
    pool.get(modifierId).map(unconfirmedTx => unconfirmedTx.transaction)
  }

   def contains(uTx: UnconfirmedTransaction): Boolean = {
    pool.contains(uTx)
  }

  override def take(limit: Int): Iterable[UnconfirmedTransaction] = {
    pool.orderedTransactions.take(limit)
  }

  def random(limit: Int): Iterable[UnconfirmedTransaction] = {
    val result = mutable.WrappedArray.newBuilder[UnconfirmedTransaction]
    val txSeq = pool.orderedTransactions.to[Vector]
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

  override def getAll: Seq[UnconfirmedTransaction] = pool.orderedTransactions.toSeq

  override def getAll(ids: Seq[ModifierId]): Seq[UnconfirmedTransaction] = ids.flatMap(pool.get)

  /**
    * Returns all transactions resided in pool sorted by weight in descending order
    */
  override def getAllPrioritized: Seq[UnconfirmedTransaction] = pool.orderedTransactions.toSeq

  /**
    * Method to put a transaction into the memory pool. Validation of the transactions against
    * the state is done in NodeViewHolder. This put() method can check whether a transaction is valid
    * @param unconfirmedTx
    * @return Success(updatedPool), if transaction successfully added to the pool, Failure(_) otherwise
    */
  def put(unconfirmedTx: UnconfirmedTransaction): ErgoMemPool = {
    val updatedPool = pool.put(unconfirmedTx)
    new ErgoMemPool(updatedPool, stats, sortingOption)
  }

  def put(txs: TraversableOnce[UnconfirmedTransaction]): ErgoMemPool = {
    txs.foldLeft(this) { case (acc, tx) => acc.put(tx) }
  }

  private def updateStatsOnRemoval(tx: ErgoTransaction): MemPoolStatistics = {
    pool.get(tx.id).map(tx => stats.add(System.currentTimeMillis(), tx)(sortingOption))
       .getOrElse(MemPoolStatistics(System.currentTimeMillis(), 0, System.currentTimeMillis()))
  }

  /**
    * Remove transaction from the pool
    */
  def remove(tx: ErgoTransaction): ErgoMemPool = {
    log.debug(s"Removing transaction ${tx.id} from the mempool")
    new ErgoMemPool(pool.remove(tx), updateStatsOnRemoval(tx), sortingOption)
  }

  def remove(txs: TraversableOnce[ErgoTransaction]): ErgoMemPool = {
    txs.foldLeft(this) { case (acc, tx) => acc.remove(tx) }
  }

  /**
    * Invalidate transaction and delete it from pool
    *
    * @param unconfirmedTx - Transaction to invalidate
    */
  def invalidate(unconfirmedTx: UnconfirmedTransaction): ErgoMemPool = {
    log.debug(s"Invalidating mempool transaction ${unconfirmedTx.id}")
    new ErgoMemPool(pool.invalidate(unconfirmedTx), updateStatsOnRemoval(unconfirmedTx.transaction), sortingOption)
  }

  def invalidate(unconfirmedTransactionId: ModifierId): ErgoMemPool = {
    pool.get(unconfirmedTransactionId) match {
      case Some(utx) => invalidate(utx)
      case None =>
        log.warn(s"Can't invalidate transaction $unconfirmedTransactionId as it is not in the pool")
        this
    }
  }

  /**
    * Check if transaction was invalidated earlier
    */
  def isInvalidated(id: ModifierId): Boolean = pool.isInvalidated(id)

  /**
    * @return inputs spent by the mempool transactions
    */
  override def spentInputs: Iterator[BoxId] = pool.inputs.keysIterator


  // Check if transaction is double-spending inputs spent in the mempool.
  // If so, the new transacting is replacing older ones if it has bigger weight (fee/byte) than them on average.
  // Otherwise, the new transaction being rejected.
  private def acceptIfNoDoubleSpend(unconfirmedTransaction: UnconfirmedTransaction,
                                    validationStartTime: Long): (ErgoMemPool, ProcessingOutcome) = {
    val tx = unconfirmedTransaction.transaction

    val doubleSpendingIds = tx.inputs.flatMap { inp =>
      pool.inputs.get(inp.boxId)
    }.toSet

    implicit val so: SortingOption = sortingOption

    if (doubleSpendingIds.nonEmpty) {
      val doubleSpendingTxs = doubleSpendingIds.map(pool.get(_).get).toSeq
      val doubleSpendingTotalWeight = doubleSpendingTxs.map(_.weight).sum / doubleSpendingTxs.size
      if (unconfirmedTransaction.weight > doubleSpendingTotalWeight) {
        val p = pool.remove(doubleSpendingTxs).put(unconfirmedTransaction)
        val updPool = new ErgoMemPool(p, stats, sortingOption)
        updPool -> new ProcessingOutcome.Accepted(unconfirmedTransaction, validationStartTime)
      } else {
        this -> new ProcessingOutcome.DoubleSpendingLoser(doubleSpendingIds, validationStartTime)
      }
    } else {
      val poolSizeLimit = nodeSettings.mempoolCapacity
      if (pool.size == poolSizeLimit &&
        unconfirmedTransaction.weight <= pool.orderedTransactions.lastKey.weight) {
        val exc = new Exception("Transaction pays less than any other in the pool being full")
        this -> new ProcessingOutcome.Declined(exc, validationStartTime)
      } else {
        val updPool = new ErgoMemPool(pool.put(unconfirmedTransaction), stats, sortingOption)
        updPool -> new ProcessingOutcome.Accepted(unconfirmedTransaction, validationStartTime)
      }
    }
  }

  def process(unconfirmedTx: UnconfirmedTransaction, state: ErgoState[_]): (ErgoMemPool, ProcessingOutcome) = {
    val tx = unconfirmedTx.transaction

    val invalidatedCnt = this.pool.invalidatedTxIds.approximateElementCount
    val poolSize = this.size

    log.info(s"Processing mempool transaction: $tx")
    log.debug(s"Mempool: invalidated transactions: $invalidatedCnt, pool size: $poolSize")

    val validationStartTime = System.currentTimeMillis()

    val blacklistedTransactions = nodeSettings.blacklistedTransactions
    if (blacklistedTransactions.nonEmpty && blacklistedTransactions.contains(tx.id)) {
      val exc = new Exception("blacklisted tx")
      this.invalidate(unconfirmedTx) -> new ProcessingOutcome.Invalidated(exc, validationStartTime)
    } else {
      val fee = extractFee(tx)
      val minFee = settings.nodeSettings.minimalFeeAmount
      val canAccept = pool.canAccept(unconfirmedTx)

      if (fee >= minFee) {
        if (canAccept) {
          val costLimit = nodeSettings.maxTransactionCost
          state match {
            case utxo: UtxoState =>
              // Allow proceeded transaction to spend outputs of pooled transactions.
              val utxoWithPool = utxo.withUnconfirmedTransactions(getAll)
              if (tx.inputIds.forall(inputBoxId => utxoWithPool.boxById(inputBoxId).isDefined)) {
                utxoWithPool.validateWithCost(tx, Some(utxo.stateContext), costLimit, None) match {
                  case Success(cost) =>
                    acceptIfNoDoubleSpend(unconfirmedTx.withCost(cost), validationStartTime)
                  case Failure(ex) =>
                    this.invalidate(unconfirmedTx) -> new ProcessingOutcome.Invalidated(ex, validationStartTime)
                }
              } else {
                val exc = new Exception("not all utxos in place yet")
                this -> new ProcessingOutcome.Declined(exc, validationStartTime)
              }
            case validator: TransactionValidation =>
              // transaction validation currently works only for UtxoState, so this branch currently
              // will not be triggered probably
              validator.validateWithCost(tx, costLimit) match {
                case Success(cost) =>
                  acceptIfNoDoubleSpend(unconfirmedTx.withCost(cost), validationStartTime)
                case Failure(ex) =>
                  this.invalidate(unconfirmedTx) -> new ProcessingOutcome.Invalidated(ex, validationStartTime)
              }
            case _ =>
              // Accept transaction in case of "digest" state. Transactions are not downloaded in this mode from other
              // peers though, so such transactions can come from the local wallet only.
              acceptIfNoDoubleSpend(unconfirmedTx, validationStartTime)
          }
        } else {
          val msg = if (contains(unconfirmedTx)) {
            s"Pool can not accept transaction ${tx.id}, it is already in the mempool"
          } else if (pool.size == settings.nodeSettings.mempoolCapacity) {
            s"Pool can not accept transaction ${tx.id}, the mempool is full"
          } else {
            s"Pool can not accept transaction ${tx.id}"
          }
          val exc = new Exception(msg)
          this -> new ProcessingOutcome.Declined(exc, validationStartTime)
        }
      } else {
        val exc = new Exception(s"Min fee not met: ${minFee.toDouble / CoinsInOneErgo} ergs required, " +
          s"${fee.toDouble / CoinsInOneErgo} ergs given")

        this -> new ProcessingOutcome.Declined(exc, validationStartTime)
      }
    }
  }

  def txTimesAndWeights: Seq[(Long,Long)] = pool.orderedTransactions.toSeq.map(uTx => uTx.createdTime -> uTx.weight(monetarySettings, sortingOption))

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
    *
    * @param txFee  - transaction fee
    * @param txSize - size of transaction (in bytes)
    * @return average time for this transaction to be placed in block
    */
  def getExpectedWaitTime(txFee : Long, txSize : Int): Long  = {
    val feePerKb = txFee * 1024 / txSize

    // Find position of entry in mempool
    val posInPool = pool.orderedTransactions.dropWhile(_.weight(monetarySettings, sortingOption) > feePerKb).size

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
    case object FeePerCycle extends SortingOption

    /**
      * @return randomly chosen mempool sorting strategy
      */
    def random(): SortingOption = {
      if (Random.nextBoolean()) {
        FeePerByte
      } else {
        FeePerCycle
      }
    }
  }

  /**
    * Root of possible mempool transaction validation result family
    */
  sealed trait ProcessingOutcome {
    /**
      * Time when transaction validation was started
      */
    protected val validationStartTime: Long

    /**
      * We assume that validation ends when this processing result class is constructed
      */
    private val validationEndTime: Long = System.currentTimeMillis()

    /**
      * 5.0 JIT costing was designed in a way that 1000 cost units are roughly corresponding to 1 ms of 1 CPU core
      * on commodity hardware (of 2021). So if we do not know the exact cost of transaction, we can estimate it by
      * tracking validation time and then getting estimated validation cost by multiplying the time (in ms) by 1000
      */
    val costPerMs = 1000

    /**
      * Estimated validation cost, see comment for `costPerMs`
      */
    def cost: Int = {
      val timeDiff = validationEndTime - validationStartTime
      if (timeDiff == 0) {
        costPerMs
      } else if (timeDiff > 1000000) {
        Int.MaxValue // shouldn't be here, so this branch is mostly to have safe .toInt below
      } else {
        (timeDiff * costPerMs).toInt
      }
    }
  }

  object ProcessingOutcome {

    /**
      * Object signalling that a transaction is accepted to the memory pool
      */
    class Accepted(val tx: UnconfirmedTransaction,
                   override protected val validationStartTime: Long) extends ProcessingOutcome {
      override val cost: Int = tx.lastCost.getOrElse(super.cost)
    }

    /**
      * Class signalling that a valid transaction was rejected as it is double-spending inputs of mempool transactions
      * and has no bigger weight (fee/byte) than them on average.
      *
      * @param winnerTxIds - identifiers of transactions won in replace-by-fee auction
      */
    class DoubleSpendingLoser(val winnerTxIds: Set[ModifierId],
                              override protected val validationStartTime: Long) extends ProcessingOutcome

    /**
      * Class signalling that a transaction declined from being accepted into the memory pool
      */
    class Declined(val e: Throwable,
                   override protected val validationStartTime: Long) extends ProcessingOutcome


    /**
      * Class signalling that a transaction turned out to be invalid when checked in the mempool
      */
    class Invalidated(val e: Throwable,
                      override protected val validationStartTime: Long) extends ProcessingOutcome

  }

  /**
    * Create empty mempool
    *
    * @param settings - node settings (to get mempool settings from)
    * @return empty mempool
    */
  def empty(settings: ErgoSettings): ErgoMemPool = {
    val sortingOption = settings.nodeSettings.mempoolSorting
    sortingOption match {
      case SortingOption.FeePerByte => log.info("Sorting mempool by fee-per-byte")
      case SortingOption.FeePerCycle => log.info("Sorting mempool by fee-per-cycle")
    }
    new ErgoMemPool(
      OrderedTxPool.empty(settings, sortingOption),
      MemPoolStatistics(System.currentTimeMillis(), 0, System.currentTimeMillis()),
      sortingOption
    )(settings)
  }

}
