package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.settings.{ErgoSettings, MonetarySettings, NodeConfigurationSettings}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import OrderedTxPool.weighted
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils._
import sigma.VersionContext
import spire.syntax.all.cfor

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}


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

  /**
    * When there's no reason to re-check transactions immediately, we assign fake cost to them
    */
  private val FakeCost = 1000

  private val nodeSettings: NodeConfigurationSettings = settings.nodeSettings
  private implicit val monetarySettings: MonetarySettings = settings.chainSettings.monetary

  override def size: Int = pool.size

  override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = {
    pool.get(modifierId).map(unconfirmedTx => unconfirmedTx.transaction)
  }

  override def contains(modifierId: ModifierId): Boolean = {
    pool.contains(modifierId)
  }

  override def take(limit: Int): Iterable[UnconfirmedTransaction] = {
    pool.orderedTransactions.values.take(limit)
  }

  def random(limit: Int): Iterable[UnconfirmedTransaction] = {
    val result = mutable.WrappedArray.newBuilder[UnconfirmedTransaction]
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

  override def getAll: Seq[UnconfirmedTransaction] = pool.orderedTransactions.values.toSeq

  override def getAll(ids: Seq[ModifierId]): Seq[UnconfirmedTransaction] = ids.flatMap(pool.get)

  /**
    * Returns all transactions resided in pool sorted by weight in descending order
    */
  override def getAllPrioritized: Seq[UnconfirmedTransaction] = pool.orderedTransactions.values.toSeq

  /**
    * Method to put a transaction into the memory pool. Validation of the transactions against
    * the state is done in NodeViewHolder. This put() method can check whether a transaction is valid
    * @param unconfirmedTx
    * @return Success(updatedPool), if transaction successfully added to the pool, Failure(_) otherwise
    */
  def put(unconfirmedTx: UnconfirmedTransaction): ErgoMemPool = {
    val updatedPool = pool.put(unconfirmedTx, feeFactor(unconfirmedTx))
    new ErgoMemPool(updatedPool, stats, sortingOption)
  }

  def put(txs: TraversableOnce[UnconfirmedTransaction]): ErgoMemPool = {
    txs.foldLeft(this) { case (acc, tx) => acc.put(tx) }
  }

  private def updateStatsOnRemoval(tx: ErgoTransaction): MemPoolStatistics = {
    val wtx = pool.transactionsRegistry.get(tx.id)
    wtx.map(wgtx => stats.add(System.currentTimeMillis(), wgtx))
       .getOrElse(MemPoolStatistics(System.currentTimeMillis(), 0, System.currentTimeMillis()))
  }

  /**
    * Remove transaction from the pool along with its double-spends
    */
  def removeTxAndDoubleSpends(tx: ErgoTransaction): ErgoMemPool = {
    def removeTx(mp: ErgoMemPool, tx: ErgoTransaction): ErgoMemPool = {
      log.debug(s"Removing transaction ${tx.id} from the mempool")
      new ErgoMemPool(mp.pool.remove(tx), mp.updateStatsOnRemoval(tx), sortingOption)
    }

    val poolWithoutTx = removeTx(this, tx)
    val doubleSpentTransactionIds = tx.inputs.flatMap(i =>
      poolWithoutTx.pool.inputs.get(i.boxId)
    ).toSet
    val doubleSpentTransactions = doubleSpentTransactionIds.flatMap { txId =>
      poolWithoutTx.pool.orderedTransactions.get(txId)
    }
    doubleSpentTransactions.foldLeft(poolWithoutTx) { case (pool, tx) =>
      removeTx(pool, tx.transaction)
    }
  }

  /**
    * Remove provided transactions and their doublespends from the pool
    */
  def removeWithDoubleSpends(txs: TraversableOnce[ErgoTransaction]): ErgoMemPool = {
    txs.foldLeft(this) { case (memPool, tx) =>
      if (memPool.contains(tx.id)) { // tx could be removed earlier in this loop as double-spend of another tx
        memPool.removeTxAndDoubleSpends(tx)
      } else {
        memPool
      }
    }
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
        log.warn(s"pool.get failed for $unconfirmedTransactionId")
        pool.orderedTransactions.valuesIterator.find(_.id == unconfirmedTransactionId) match {
          case Some(utx) =>
            invalidate(utx)
          case None =>
            log.warn(s"Can't invalidate transaction $unconfirmedTransactionId as it is not in the pool")
            this
        }
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

  private def feeFactor(unconfirmedTransaction: UnconfirmedTransaction): Int = {
    sortingOption match {
      case SortingOption.FeePerByte =>
        unconfirmedTransaction.transactionBytes.map(_.length).getOrElse(unconfirmedTransaction.transaction.size)
      case SortingOption.FeePerCycle =>
        unconfirmedTransaction.lastCost.getOrElse(FakeCost)
    }
  }

  // Check if transaction is double-spending inputs spent in the mempool.
  // If so, the new transacting is replacing older ones if it has bigger weight (fee/byte) than them on average.
  // Otherwise, the new transaction being rejected.
  private def acceptIfNoDoubleSpend(unconfirmedTransaction: UnconfirmedTransaction,
                                    validationStartTime: Long): (ErgoMemPool, ProcessingOutcome) = {
    val tx = unconfirmedTransaction.transaction

    val doubleSpendingWtxs = tx.inputs.flatMap { inp =>
      pool.inputs.get(inp.boxId)
    }.toSet

    val feeF = feeFactor(unconfirmedTransaction)

    if (doubleSpendingWtxs.nonEmpty) {
      val ownWtx = weighted(tx, feeF)
      val doubleSpendingTotalWeight = doubleSpendingWtxs.map(_.weight).sum / doubleSpendingWtxs.size
      if (ownWtx.weight > doubleSpendingTotalWeight) {
        val doubleSpendingTxs = doubleSpendingWtxs.map(wtx => pool.orderedTransactions(wtx)).toSeq
        val p = pool.put(unconfirmedTransaction, feeF).remove(doubleSpendingTxs)
        val updPool = new ErgoMemPool(p, stats, sortingOption)
        updPool -> new ProcessingOutcome.Accepted(unconfirmedTransaction, validationStartTime)
      } else {
        this -> new ProcessingOutcome.DoubleSpendingLoser(doubleSpendingWtxs.map(_.id), validationStartTime)
      }
    } else {
      val poolSizeLimit = nodeSettings.mempoolCapacity
      if (pool.size == poolSizeLimit &&
        weighted(tx, feeF).weight <= pool.orderedTransactions.lastKey.weight) {
        val exc = new Exception("Transaction pays less than any other in the pool being full")
        this -> new ProcessingOutcome.Declined(exc, validationStartTime)
      } else {
        val updPool = new ErgoMemPool(pool.put(unconfirmedTransaction, feeF), stats, sortingOption)
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

                // added in 6.0 to check now versioned serializers
                // as having unparseable outputs is okay per protocol rules, but in some cases in 6.0
                // tree deserialization fails with versioning issues (eg when tree version > activated version),
                // not parsing ones
                val scriptVersion = Header.scriptFromBlockVersion(state.stateContext.blockVersion)
                VersionContext.withVersions(scriptVersion, scriptVersion) {
                  ErgoTransactionSerializer.parseBytesTry(unconfirmedTx.transaction.bytes) match {
                    case Success(_) =>
                    case Failure(e) => return (this, new ProcessingOutcome.Invalidated(e, validationStartTime))
                  }
                }

                val validationContext = utxo.stateContext.simplifiedUpcoming()
                utxoWithPool.validateWithCost(tx, validationContext, costLimit, None) match {
                  case Success(cost) =>
                    acceptIfNoDoubleSpend(unconfirmedTx.withCost(cost), validationStartTime)
                  case Failure(ex) =>
                    this.invalidate(unconfirmedTx) -> new ProcessingOutcome.Invalidated(ex, validationStartTime)
                }
              } else {
                val exc = new Exception("not all utxos in place yet")
                this -> new ProcessingOutcome.Declined(exc, validationStartTime)
              }
            case _ =>
              // Accept transaction in case of "digest" state. Transactions are not downloaded in this mode from other
              // peers though, so such transactions can come from the local wallet only.
              acceptIfNoDoubleSpend(unconfirmedTx, validationStartTime)
          }
        } else {
          val msg = if (this.contains(tx.id)) {
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
    *
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
      OrderedTxPool.empty(settings),
      MemPoolStatistics(System.currentTimeMillis(), 0, System.currentTimeMillis()),
      sortingOption
    )(settings)
  }
}
