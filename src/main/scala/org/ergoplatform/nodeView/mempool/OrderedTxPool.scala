package org.ergoplatform.nodeView.mempool

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.ergoplatform.settings.{Algos, ErgoSettings, MonetarySettings}
import scorex.util.{ModifierId, ScorexLogging}

import scala.collection.immutable.TreeMap

/**
  * An immutable pool of transactions of limited size with priority management and blacklisting support.
  *
  * @param orderedTransactions  - collection containing transactions ordered by `tx.weight`
  * @param transactionsRegistry - mapping `tx.id` -> `tx.weight` required for fast access to transaction by its `id`
  * @param invalidated          - collection containing invalidated transaction ids as keys
  *                             ordered by invalidation timestamp (values)
  * @param outputs              - mapping `box.id` -> `tx.weight` required locate transaction by its output box
  */
final case class OrderedTxPool(orderedTransactions: TreeMap[WeightedTxId, ErgoTransaction],
                               transactionsRegistry: TreeMap[ModifierId, WeightedTxId],
                               invalidated: TreeMap[ModifierId, Long],
                               outputs: TreeMap[BoxId, WeightedTxId]
                              )
                              (implicit settings: ErgoSettings) extends ScorexLogging {

  import OrderedTxPool.weighted

  private implicit val ms: MonetarySettings = settings.chainSettings.monetary

  private val mempoolCapacity = settings.nodeSettings.mempoolCapacity

  private val blacklistCapacity = settings.nodeSettings.blacklistCapacity

  def size: Int = orderedTransactions.size

  def get(id: ModifierId): Option[ErgoTransaction] = {
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
    * @param tx - transaction to add
    * @return - modified pool
    */
  def put(tx: ErgoTransaction): OrderedTxPool = {
    val wtx = weighted(tx)
    val newPool = OrderedTxPool(orderedTransactions.updated(wtx, tx),
      transactionsRegistry.updated(wtx.id, wtx), invalidated,
      outputs ++ tx.outputs.map(_.id -> wtx)).updateFamily(tx, wtx.weight)
    if (newPool.orderedTransactions.size > mempoolCapacity) {
      val victim = newPool.orderedTransactions.head._2
      newPool.remove(victim)
    } else {
      newPool
    }
  }

  def remove(tx: ErgoTransaction): OrderedTxPool = {
    transactionsRegistry.get(tx.id).fold(this)(wtx =>
      OrderedTxPool(orderedTransactions - wtx, transactionsRegistry - tx.id, invalidated, outputs -- tx.outputs.map(_.id)).updateFamily(tx, -wtx.weight))
  }

  def invalidate(tx: ErgoTransaction): OrderedTxPool = {
    val inv = if (invalidated.size >= blacklistCapacity) invalidated - invalidated.firstKey else invalidated
    val ts = System.currentTimeMillis()
    transactionsRegistry.get(tx.id).fold(OrderedTxPool(orderedTransactions, transactionsRegistry, inv.updated(tx.id, ts), outputs))(wtx =>
      OrderedTxPool(orderedTransactions - wtx, transactionsRegistry - tx.id, inv.updated(tx.id, ts),
        outputs -- tx.outputs.map(_.id)).updateFamily(tx, -wtx.weight))
  }

  def filter(condition: ErgoTransaction => Boolean): OrderedTxPool = {
    orderedTransactions.foldLeft(this)((pool,entry) => {
      val tx = entry._2
      if (condition(tx)) pool else pool.remove(tx)
    })
  }

  /**
    * Do not place transaction in the pool if its weight is smaller than smallest weight of transaction in the pool
    * and pool limit is reached. We need to take in account relationship between transactions: if candidate transaction
    * is pending output of one of transactions in mempool, we should adjust their weights by adding weight of this
    * transaction (see updateFamily). Otherwise we will do waste job of validating transaction which will be then
    * thrown from the pool.
    *
    * @param tx
    * @return
    */
  def canAccept(tx: ErgoTransaction): Boolean = {
    val weight = weighted(tx).weight
    !isInvalidated(tx.id) && !contains(tx.id) &&
      (size < settings.nodeSettings.mempoolCapacity ||
        weight > updateFamily(tx, weight).orderedTransactions.firstKey.weight)
  }

  def contains(id: ModifierId): Boolean = transactionsRegistry.contains(id)

  def isInvalidated(id: ModifierId): Boolean = invalidated.contains(id)


  /**
    *
    * Form families of transactions: take in account relations between transaction when perform ordering.
    * If transaction X is spending output of transaction Y, then X weight should be greater than of Y.
    * Y should be proceeded prior to X or swapped out of mempool after X.
    * To achieve this goal we recursively add weight of new transaction to all transactions which
    * outputs it directly or indirectly spending.
    *
    * @param tx
    * @param weight
    * @return
    */
  private def updateFamily(tx : ErgoTransaction, weight : Double) : OrderedTxPool = {
    tx.inputs.foldLeft(this)((pool,box) =>
      outputs.get(box.boxId).fold(pool)(wtx => {
        val parent = orderedTransactions(wtx)
        val newWtx = WeightedTxId(wtx.id, wtx.weight + weight)
        val newPool = OrderedTxPool(pool.orderedTransactions - wtx + (newWtx -> parent),
          pool.transactionsRegistry.updated(parent.id, newWtx),
          invalidated,
          outputs)
        newPool.updateFamily(parent, weight)
      }))
  }
}

object OrderedTxPool {

  case class WeightedTxId(id: ModifierId, weight: Double) {
    // `id` depends on `weight` so we can use only the former for comparison.
    override def equals(obj: Any): Boolean = obj match {
      case that: WeightedTxId => that.id == id
      case _ => false
    }

    override def hashCode(): Int = Ints.fromByteArray(Algos.decodeUnsafe(id.take(8)))
  }

  private implicit val ordWeight: Ordering[WeightedTxId] = Ordering[(Double, ModifierId)].on(x => (x.weight, x.id))
  private implicit val ordBoxId: Ordering[BoxId] = Ordering[String].on(b =>  Algos.encode(b))

  def empty(settings: ErgoSettings): OrderedTxPool = {
    OrderedTxPool(TreeMap.empty[WeightedTxId, ErgoTransaction], TreeMap.empty[ModifierId, WeightedTxId],
      TreeMap.empty[ModifierId, Long], TreeMap.empty[BoxId, WeightedTxId])(settings)
  }

  private def weighted(tx: ErgoTransaction)(implicit ms: MonetarySettings): WeightedTxId = {
    val fee = tx.outputs
      .filter(b => java.util.Arrays.equals(b.propositionBytes, ms.feePropositionBytes))
      .map(_.value)
      .sum
    WeightedTxId(tx.id, fee.toDouble / tx.size)
  }

}
