package org.ergoplatform.nodeView.mempool

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.util.ModifierId

import scala.collection.immutable.TreeMap

/**
  * Immutable transactions pool of limited size with priority management and blacklisting support.
  */
case class OrderedTxPool(orderedTransactions: TreeMap[WeightedTxId, ErgoTransaction],
                         transactionsRegistry: TreeMap[ModifierId, Long],
                         invalidated: TreeMap[ModifierId, Long],
                         settings: ErgoSettings) {

  import OrderedTxPool.weighted

  private val mempoolCapacity = settings.nodeSettings.mempoolCapacity

  private val blacklistCapacity = mempoolCapacity * 4

  def size: Int = orderedTransactions.size

  def get(id: ModifierId): Option[ErgoTransaction] = {
    transactionsRegistry.get(id).flatMap(w => orderedTransactions.get(WeightedTxId(id, w)))
  }

  def put(tx: ErgoTransaction): OrderedTxPool = {
    val (txs, ws) = if (orderedTransactions.size >= mempoolCapacity) {
      (orderedTransactions - orderedTransactions.firstKey) -> (transactionsRegistry - transactionsRegistry.firstKey)
    } else {
      orderedTransactions -> transactionsRegistry
    }
    val wtx = weighted(tx)(settings)
    OrderedTxPool(txs.updated(wtx, tx), ws.updated(wtx.id, wtx.weight), invalidated, settings)
  }

  def remove(tx: ErgoTransaction): OrderedTxPool = {
    OrderedTxPool(orderedTransactions - weighted(tx)(settings), transactionsRegistry - tx.id, invalidated, settings)
  }

  def invalidate(tx: ErgoTransaction): OrderedTxPool = {
    val inv = if (invalidated.size >= blacklistCapacity) invalidated - invalidated.firstKey else invalidated
    val ts = System.currentTimeMillis()
    OrderedTxPool(orderedTransactions - weighted(tx)(settings),
      transactionsRegistry - tx.id, inv.updated(tx.id, ts), settings)
  }

  def filter(condition: ErgoTransaction => Boolean): OrderedTxPool = {
    val txs = orderedTransactions.filter { case (_, v) => condition(v) }
    val ws = transactionsRegistry.filter(tx => orderedTransactions.get(WeightedTxId(tx._1, tx._2)).exists(condition))
    OrderedTxPool(txs, ws, invalidated, settings)
  }

  def canAccept(tx: ErgoTransaction): Boolean = {
    !isInvalidated(tx.id) && !contains(tx.id) &&
      (size < settings.nodeSettings.mempoolCapacity ||
        weighted(tx)(settings).weight > orderedTransactions.firstKey.weight)
  }

  def contains(id: ModifierId): Boolean = transactionsRegistry.contains(id)

  def isInvalidated(id: ModifierId): Boolean = invalidated.contains(id)

}

object OrderedTxPool {

  case class WeightedTxId(id: ModifierId, weight: Long) {
    // in order to avoid redundant allocations only `id` is used.
    override def equals(obj: Any): Boolean = obj match {
      case that: WeightedTxId => that.id == id
      case _ => false
    }
    override def hashCode(): Int = Ints.fromByteArray(Algos.decodeUnsafe(id.take(8)))
  }

  private implicit val ord: Ordering[WeightedTxId] = Ordering[(Long, ModifierId)].on(x => (x.weight, x.id))

  def empty(settings: ErgoSettings): OrderedTxPool = {
    OrderedTxPool(TreeMap.empty[WeightedTxId, ErgoTransaction], TreeMap.empty[ModifierId, Long],
      TreeMap.empty[ModifierId, Long], settings)
  }

  private def weighted(tx: ErgoTransaction)(settings: ErgoSettings): WeightedTxId = {
    val fee = tx.outputs
      .filter(b => java.util.Arrays.equals(b.propositionBytes, settings.feePropositionBytes))
      .map(_.value)
      .sum
    WeightedTxId(tx.id, fee)
  }

}
