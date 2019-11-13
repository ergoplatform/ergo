package org.ergoplatform.nodeView.mempool

import com.google.common.primitives.Ints
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
  */
final case class OrderedTxPool(orderedTransactions: TreeMap[WeightedTxId, ErgoTransaction],
                               transactionsRegistry: TreeMap[ModifierId, Double],
                               invalidated: TreeMap[ModifierId, Long])
                              (implicit settings: ErgoSettings) extends ScorexLogging {

  import OrderedTxPool.weighted

  private implicit val ms: MonetarySettings = settings.chainSettings.monetary

  private val mempoolCapacity = settings.nodeSettings.mempoolCapacity

  private val blacklistCapacity = settings.nodeSettings.blacklistCapacity

  def size: Int = orderedTransactions.size

  def get(id: ModifierId): Option[ErgoTransaction] = {
    transactionsRegistry.get(id).flatMap(w => orderedTransactions.get(WeightedTxId(id, w)))
  }

  def put(tx: ErgoTransaction): OrderedTxPool = {
    val (txs, ws) = if (orderedTransactions.size >= mempoolCapacity) {
      val keyToThrow = orderedTransactions.firstKey
      log.debug(s"Memory pool overflow - throwing out transaction with id = ${keyToThrow.id}")
      (orderedTransactions - keyToThrow) -> (transactionsRegistry - keyToThrow.id)
    } else {
      orderedTransactions -> transactionsRegistry
    }
    val wtx = weighted(tx)
    OrderedTxPool(txs.updated(wtx, tx), ws.updated(wtx.id, wtx.weight), invalidated)
  }

  def remove(tx: ErgoTransaction): OrderedTxPool = {
    OrderedTxPool(orderedTransactions - weighted(tx), transactionsRegistry - tx.id, invalidated)
  }

  def invalidate(tx: ErgoTransaction): OrderedTxPool = {
    val inv = if (invalidated.size >= blacklistCapacity) invalidated - invalidated.firstKey else invalidated
    val ts = System.currentTimeMillis()
    OrderedTxPool(orderedTransactions - weighted(tx),
      transactionsRegistry - tx.id, inv.updated(tx.id, ts))
  }

  def filter(condition: ErgoTransaction => Boolean): OrderedTxPool = {
    val txs = orderedTransactions.filter { case (_, v) => condition(v) }
    val ws = transactionsRegistry.filter(tx => orderedTransactions.get(WeightedTxId(tx._1, tx._2)).exists(condition))
    OrderedTxPool(txs, ws, invalidated)
  }

  def canAccept(tx: ErgoTransaction): Boolean = {
    !isInvalidated(tx.id) &&
      !contains(tx.id) &&
      (size < mempoolCapacity || weighted(tx).weight > orderedTransactions.firstKey.weight)
  }

  def contains(id: ModifierId): Boolean = transactionsRegistry.contains(id)

  def isInvalidated(id: ModifierId): Boolean = invalidated.contains(id)

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

  private implicit val ord: Ordering[WeightedTxId] = Ordering[(Double, ModifierId)].on(x => (x.weight, x.id))

  def empty(settings: ErgoSettings): OrderedTxPool = {
    OrderedTxPool(TreeMap.empty[WeightedTxId, ErgoTransaction], TreeMap.empty[ModifierId, Double],
      TreeMap.empty[ModifierId, Long])(settings)
  }

  private def weighted(tx: ErgoTransaction)(implicit ms: MonetarySettings): WeightedTxId = {
    val fee = tx.outputs
      .filter(b => java.util.Arrays.equals(b.propositionBytes, ms.feePropositionBytes))
      .map(_.value)
      .sum
    WeightedTxId(tx.id, fee.toDouble / tx.size)
  }

}
