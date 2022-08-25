package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.ergoplatform.settings.{Algos, ErgoSettings, MonetarySettings}
import scorex.util.{ModifierId, ScorexLogging}

import scala.collection.immutable.TreeMap

/**
  * An immutable pool of transactions of limited size with priority management and blacklisting support.
  *
  * @param orderedTransactions  - collection containing transactions ordered by `tx.weight`
  * @param transactionsRegistry - mapping `tx.id` -> `WeightedTxId(tx.id,tx.weight)` required for getting transaction by its `id`
  * @param invalidatedTxIds     - invalidated transaction ids in bloom filters
  * @param outputs              - mapping `box.id` -> `WeightedTxId(tx.id,tx.weight)` required for getting a transaction by its output box
  * @param inputs               - mapping `box.id` -> `WeightedTxId(tx.id,tx.weight)` required for getting a transaction by its input box id
  */
case class OrderedTxPool(orderedTransactions: TreeMap[WeightedTxId, UnconfirmedTransaction],
                         transactionsRegistry: TreeMap[ModifierId, WeightedTxId],
                         invalidatedTxIds: ApproximateCacheLike[String],
                         outputs: TreeMap[BoxId, WeightedTxId],
                         inputs: TreeMap[BoxId, WeightedTxId])
                        (implicit settings: ErgoSettings) extends ScorexLogging {

  import OrderedTxPool.weighted

  private implicit val ms: MonetarySettings = settings.chainSettings.monetary

  private val mempoolCapacity = settings.nodeSettings.mempoolCapacity

  def size: Int = orderedTransactions.size

  def get(id: ModifierId): Option[UnconfirmedTransaction] = {
    transactionsRegistry.get(id).flatMap(orderedTransactions.get(_))
  }


  /**
    * Add new transaction to the pool and throw away from the pool transaction with the smallest weight
    * if pool is overflown. We should first add transaction and only after it find candidate for replacement
    * because new transaction may affect weights of existed transaction in mempool (see updateFamily).
    * So candidate for replacement (transaction with minimal weight) can be changed after adding new transaction.
    * put() is preceded by canAccept method which enforces that newly added transaction will not be immediately
    * thrown from the pool.
    *
    * @param unconfirmedTx - transaction to add
    * @return - modified pool
    */
  def put(unconfirmedTx: UnconfirmedTransaction, feeFactor: Int): OrderedTxPool = {
    val tx = unconfirmedTx.transaction
    val wtx = weighted(tx, feeFactor)
    val newPool = OrderedTxPool(
      orderedTransactions.updated(wtx, unconfirmedTx),
      transactionsRegistry.updated(wtx.id, wtx),
      invalidatedTxIds,
      outputs ++ tx.outputs.map(_.id -> wtx),
      inputs ++ tx.inputs.map(_.boxId -> wtx)
    ).updateFamily(unconfirmedTx, wtx.weight)
    if (newPool.orderedTransactions.size > mempoolCapacity) {
      val victim = newPool.orderedTransactions.last._2
      newPool.remove(victim)
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
    * @param unconfirmedTx - Transaction to remove
    */
  def remove(unconfirmedTx: UnconfirmedTransaction): OrderedTxPool = {
    val tx = unconfirmedTx.transaction
    transactionsRegistry.get(tx.id) match {
      case Some(wtx) =>
        OrderedTxPool(
          orderedTransactions - wtx,
          transactionsRegistry - tx.id,
          invalidatedTxIds,
          outputs -- tx.outputs.map(_.id),
          inputs -- tx.inputs.map(_.boxId)
        ).updateFamily(unconfirmedTx, -wtx.weight)
      case None => this
    }
  }

  def invalidate(unconfirmedTx: UnconfirmedTransaction): OrderedTxPool = {
    val tx = unconfirmedTx.transaction
    transactionsRegistry.get(tx.id) match {
      case Some(wtx) =>
        OrderedTxPool(
          orderedTransactions - wtx,
          transactionsRegistry - tx.id,
          invalidatedTxIds.put(tx.id),
          outputs -- tx.outputs.map(_.id),
          inputs -- tx.inputs.map(_.boxId)
        ).updateFamily(unconfirmedTx, -wtx.weight)
      case None =>
        OrderedTxPool(orderedTransactions, transactionsRegistry, invalidatedTxIds.put(tx.id), outputs, inputs)
    }
  }

  def filter(condition: UnconfirmedTransaction => Boolean): OrderedTxPool = {
    orderedTransactions.foldLeft(this)((pool, entry) => {
      val tx = entry._2
      if (condition(tx)) pool else pool.remove(tx)
    })
  }

  /**
    * Do not place transaction in the pool if the transaction known to be invalid, pool already has it, or the pool
    * is overfull.
    *
    * TODO: the latter should not happen likely as we clean pool immediately as it becomes overfull.
    *
    */
  def canAccept(unconfirmedTx: UnconfirmedTransaction): Boolean = {
    val tx = unconfirmedTx.transaction
    !isInvalidated(tx.id) && !contains(tx.id) && size <= mempoolCapacity
  }

  def contains(id: ModifierId): Boolean = transactionsRegistry.contains(id)

  def isInvalidated(id: ModifierId): Boolean = invalidatedTxIds.mightContain(id)


  /**
    *
    * Form families of transactions: take in account relations between transaction when perform ordering.
    * If transaction X is spending output of transaction Y, then X weight should be greater than of Y.
    * Y should be proceeded prior to X or swapped out of mempool after X.
    * To achieve this goal we recursively add weight of new transaction to all transactions which
    * outputs it directly or indirectly spending.
    *
    * @param unconfirmedTx
    * @param weight
    * @return
    */
  private def updateFamily(unconfirmedTx: UnconfirmedTransaction, weight: Long): OrderedTxPool = {
    val tx = unconfirmedTx.transaction
    tx.inputs.foldLeft(this)((pool, input) =>
      pool.outputs.get(input.boxId).fold(pool)(wtx => {
        pool.orderedTransactions.get(wtx) match {
          case Some(ut) =>
            val parent = ut.transaction
            val newWtx = WeightedTxId(wtx.id, wtx.weight + weight, wtx.feePerFactor, wtx.created)
            val newPool = OrderedTxPool(pool.orderedTransactions - wtx + (newWtx -> ut),
              pool.transactionsRegistry.updated(parent.id, newWtx),
              invalidatedTxIds,
              parent.outputs.foldLeft(pool.outputs)((newOutputs, box) => newOutputs.updated(box.id, newWtx)),
              parent.inputs.foldLeft(pool.inputs)((newInputs, inp) => newInputs.updated(inp.boxId, newWtx))
            )
            newPool.updateFamily(ut, weight)
          case None =>
            //shouldn't be the case, but better not to hide this possibility
            log.error("Could not find transaction in pool.orderedTransactions, please report to devs")
            pool
        }
      }))
  }
}

object OrderedTxPool {

  /**
    * Weighted transaction id
    *
    * @param id       - Transaction id
    * @param weight   - Weight of transaction
    * @param feePerFactor - Transaction's fee per factor (byte or execution cost)
    * @param created  - Transaction creation time
    */
  case class WeightedTxId(id: ModifierId, weight: Long, feePerFactor: Long, created: Long) {
    // `id` depends on `weight` so we can use only the former for comparison.
    override def equals(obj: Any): Boolean = obj match {
      case that: WeightedTxId => that.id == id
      case _ => false
    }

    override def hashCode(): Int = id.hashCode()
  }

  private implicit val ordWeight: Ordering[WeightedTxId] = Ordering[(Long, ModifierId)].on(x => (-x.weight, x.id))
  private implicit val ordBoxId: Ordering[BoxId] = Ordering[String].on(b => Algos.encode(b))

  def empty(settings: ErgoSettings): OrderedTxPool = {
    val cacheSettings = settings.cacheSettings.mempool
    val bloomFilterCapacity = cacheSettings.invalidModifiersBloomFilterCapacity
    val bloomFilterExpirationRate = cacheSettings.invalidModifiersBloomFilterExpirationRate
    val frontCacheSize = cacheSettings.invalidModifiersCacheSize
    val frontCacheExpiration = cacheSettings.invalidModifiersCacheExpiration
    OrderedTxPool(
      TreeMap.empty[WeightedTxId, UnconfirmedTransaction],
      TreeMap.empty[ModifierId, WeightedTxId],
      ExpiringApproximateCache.empty(bloomFilterCapacity, bloomFilterExpirationRate, frontCacheSize, frontCacheExpiration),
      TreeMap.empty[BoxId, WeightedTxId],
      TreeMap.empty[BoxId, WeightedTxId])(settings)
  }

  def weighted(unconfirmedTx: UnconfirmedTransaction, feeFactor: Int)(implicit ms: MonetarySettings): WeightedTxId = {
    weighted(unconfirmedTx.transaction, feeFactor)
  }

  /**
    * Wrap transaction into an entity which is storing its mempool sorting weight also
    *
    * @param tx - transaction
    * @param feeFactor - fee-related factor of the transaction `tx`, so size or cost
    * @param ms - monetary settings to extract fee proposition from
    * @return - transaction and its weight wrapped in `WeightedTxId`
    */
  def weighted(tx: ErgoTransaction, feeFactor: Int)(implicit ms: MonetarySettings): WeightedTxId = {
    val fee = tx.outputs
      .filter(b => java.util.Arrays.equals(b.propositionBytes, ms.feePropositionBytes))
      .map(_.value)
      .sum

    // We multiply by 1024 for better precision
    val feePerFactor = fee * 1024 / feeFactor
    // Weight is equal to feePerFactor here, however, it can be modified later when children transactions will arrive
    WeightedTxId(tx.id, feePerFactor, feePerFactor, System.currentTimeMillis())
  }
}
