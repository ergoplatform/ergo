package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool.SortingOption
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedKey
import org.ergoplatform.settings.{Algos, ErgoSettings, MonetarySettings}
import scorex.util.{ModifierId, ScorexLogging}

import scala.collection.immutable.TreeMap

/**
  * An immutable pool of transactions of limited size with priority management and blacklisting support.
  *
  * @param orderedTransactions  - collection mapping `WeightedKey(id,weight)` -> `UnconfirmedTransaction`, ordered by weight
  * @param invalidatedTxIds     - invalidated transaction ids in bloom filters
  * @param outputs              - mapping `box.id` -> `ModifierId` required for getting a transaction by its output box
  * @param inputs               - mapping `box.id` -> `ModifierId` required for getting a transaction by its input box id
  */
class OrderedTxPool(val orderedTransactions: TreeMap[WeightedKey, UnconfirmedTransaction],
                    val invalidatedTxIds: ApproximateCacheLike[String],
                    val outputs: TreeMap[BoxId, ModifierId],
                    val inputs: TreeMap[BoxId, ModifierId])
                   (implicit settings: ErgoSettings, sortingOption: SortingOption) extends ScorexLogging {

  /**
    * When a transaction has a parent in the mempool, we update its weight, weight of parent's parents etc.
    * This parameter sets max update depth
    */
  private val MaxParentScanDepth = 500

  /**
    * See `MaxParentScanDepth`, but this parameter sets max update time
    */
  private val MaxParentScanTime = 500

  private implicit val ms: MonetarySettings = settings.chainSettings.monetary

  private val mempoolCapacity = settings.nodeSettings.mempoolCapacity

  def size: Int = orderedTransactions.size

  def get(id: ModifierId): Option[UnconfirmedTransaction] =
    orderedTransactions.valuesIterator.find(_.id == id)


  /**
    * Add new transaction to the pool and throw away from the pool transaction with the smallest weight
    * if pool is overflown. We should first add transaction and only after it find candidate for replacement
    * because new transaction may affect weights of existed transaction in mempool (see updateFamily).
    * So candidate for replacement (transaction with minimal weight) can be changed after adding new transaction.
    * put() is preceded by canAccept method which enforces that newly added transaction will not be immediately
    * thrown from the pool.
    *
    * @param unconfirmedTx - transaction to add
    * @param feeFactor - fee factor override, used in tests
    * @return - modified pool
    */
  def put(unconfirmedTx: UnconfirmedTransaction, feeFactor: Option[Int] = None): OrderedTxPool = {
    val tx = unconfirmedTx.transaction

    feeFactor match {
      case Some(factor) => unconfirmedTx._feeFactor = factor
      case _ =>
    }

    val newPool =
      if(contains(unconfirmedTx))
        this
      else {
        val key = WeightedKey(unconfirmedTx)
        new OrderedTxPool(
          orderedTransactions.updated(key, unconfirmedTx),
          invalidatedTxIds,
          outputs ++ tx.outputs.map(_.id -> tx.id),
          inputs ++ tx.inputs.map(_.boxId -> tx.id)
        ).updateFamily(tx, unconfirmedTx.weight, System.currentTimeMillis(), 0)
      }

    if (newPool.orderedTransactions.size > mempoolCapacity) {
      newPool.remove(newPool.orderedTransactions.last._2)
    } else {
      newPool
    }
  }

  def remove(txs: Seq[UnconfirmedTransaction]): OrderedTxPool = {
    txs.foldLeft(this) { case (pool, tx) => pool.remove(tx) }
  }

  /**
    * Removes transaction from the pool
    *
    * @param tx - Transaction to remove
    */
  def remove(tx: ErgoTransaction): OrderedTxPool = {
    get(tx.id) match {
      case Some(wtx) =>
        new OrderedTxPool(
          orderedTransactions - WeightedKey(wtx),
          invalidatedTxIds,
          outputs -- tx.outputs.map(_.id),
          inputs -- tx.inputs.map(_.boxId)
        ).updateFamily(tx, -wtx.weight, System.currentTimeMillis(), depth = 0)
      case None => this
    }
  }

  def remove(utx: UnconfirmedTransaction): OrderedTxPool = {
    val key = WeightedKey(utx.id, utx.weight)
    orderedTransactions.get(key) match {
      case Some(wtx) =>
        new OrderedTxPool(
          orderedTransactions - key,
          invalidatedTxIds,
          outputs -- utx.transaction.outputs.map(_.id),
          inputs -- utx.transaction.inputs.map(_.boxId)
        ).updateFamily(utx.transaction, -wtx.weight, System.currentTimeMillis(), depth = 0)
      case None => this
    }
  }

  /**
    * Remove transaction from the pool and add it to invalidated transaction ids cache
    */
  def invalidate(unconfirmedTx: UnconfirmedTransaction): OrderedTxPool = {
    val tx = unconfirmedTx.transaction
    orderedTransactions.get(WeightedKey(unconfirmedTx)) match {
      case Some(wtx) =>
        new OrderedTxPool(
          orderedTransactions - WeightedKey(wtx),
          invalidatedTxIds.put(tx.id),
          outputs -- tx.outputs.map(_.id),
          inputs -- tx.inputs.map(_.boxId)
        ).updateFamily(tx, -wtx.weight, System.currentTimeMillis(), depth = 0)
      case None =>
        new OrderedTxPool(orderedTransactions, invalidatedTxIds.put(tx.id), outputs, inputs)
    }
  }

  /**
    * Do not place transaction in the pool if the transaction known to be invalid, pool already has it, or the pool
    * is overfull.
    *
    * TODO: the latter should not happen likely as we clean pool immediately as it becomes overfull.
    *
    */
  def canAccept(unconfirmedTx: UnconfirmedTransaction): Boolean = {
    !contains(unconfirmedTx) && size <= mempoolCapacity
  }

  /**
    *
    * @param uTx - unconfirmed transaction
    * @return - true, if transaction is in the pool or invalidated earlier, false otherwise
    */
  def contains(uTx: UnconfirmedTransaction): Boolean =
    orderedTransactions.get(WeightedKey(uTx)).exists(_.lastCheckedTime == uTx.lastCheckedTime)

  def isInvalidated(id: ModifierId): Boolean = invalidatedTxIds.mightContain(id)

  /**
    *
    * Form families of transactions: take in account relations between transactions when performing ordering.
    * If transaction X is spending output of transaction Y, then X weight should be greater than of Y.
    * Y should be proceeded prior to X or swapped out of mempool after X.
    * To achieve this goal we recursively add weight of new transaction to all transactions which
    * outputs it directly or indirectly spending.
    *
    * @param tx
    * @param weight
    * @return
    */
  private def updateFamily(tx: ErgoTransaction,
                           weight: Long,
                           startTime: Long,
                           depth: Int): OrderedTxPool = {
    val now = System.currentTimeMillis()
    val timeDiff = now - startTime
    if (depth > MaxParentScanDepth || timeDiff > MaxParentScanTime) {
      log.warn(s"updateFamily takes too long, depth: $depth, time diff: $timeDiff, transaction: ${tx.id}")
      this
    } else {

      val uniqueTxIds: Set[ModifierId] = tx.inputs.flatMap(input => outputs.get(input.boxId)).toSet
      val parentTxs = uniqueTxIds.flatMap(get)

      parentTxs.foldLeft(this) { case (pool, uTx) =>
        val parent = uTx.transaction
        val newPool = new OrderedTxPool(
          (pool.orderedTransactions - WeightedKey(uTx)) + Tuple2(WeightedKey(uTx.id, uTx.weight + weight), uTx.addWeight(weight)),
          invalidatedTxIds,
          parent.outputs.foldLeft(pool.outputs)((newOutputs, box) => newOutputs.updated(box.id, parent.id)),
          parent.inputs.foldLeft(pool.inputs)((newInputs, inp) => newInputs.updated(inp.boxId, parent.id))
        )
        newPool.updateFamily(parent, weight, startTime, depth + 1)
      }
    }
  }
}

object OrderedTxPool {

  case class WeightedKey(_1: ModifierId, _2: Long)

  case object WeightedKey {
    def apply(utx: UnconfirmedTransaction)(implicit ms: MonetarySettings, sortingOption: SortingOption): WeightedKey =
      WeightedKey(utx.id, utx.weight)
  }

  private implicit val ordBoxId: Ordering[BoxId] = Ordering[String].on(b => Algos.encode(b))

  def empty(settings: ErgoSettings, sortingOption: SortingOption): OrderedTxPool = {
    val cacheSettings = settings.cacheSettings.mempool
    val frontCacheSize = cacheSettings.invalidModifiersCacheSize
    val frontCacheExpiration = cacheSettings.invalidModifiersCacheExpiration
    implicit val ordWeight: Ordering[WeightedKey] =
      Ordering[(Long, ModifierId)].on(x => (-x._2, x._1))
    new OrderedTxPool(
      TreeMap.empty[WeightedKey, UnconfirmedTransaction],
      ExpiringApproximateCache.empty(frontCacheSize, frontCacheExpiration),
      TreeMap.empty[BoxId, ModifierId],
      TreeMap.empty[BoxId, ModifierId]
    )(settings, sortingOption)
  }
}
