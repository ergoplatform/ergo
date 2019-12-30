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

  def put(tx: ErgoTransaction): OrderedTxPool = {
    val (txs, ws, outs) = if (orderedTransactions.size >= mempoolCapacity) {
      val txnToThrow = orderedTransactions.head._2
      val keyToThrow = orderedTransactions.firstKey
      log.debug(s"Memory pool overflow - throwing out transaction with id = ${keyToThrow.id}")
      (orderedTransactions - keyToThrow, transactionsRegistry - keyToThrow.id, outputs -- txnToThrow.outputs.map(_.id))
    } else {
      (orderedTransactions, transactionsRegistry, outputs)
    }
    val wtx = weighted(tx)
    OrderedTxPool(txs.updated(wtx, tx), ws.updated(wtx.id, wtx), invalidated, outs ++ tx.outputs.map(_.id -> wtx)).updateFamily(tx, wtx.weight)
  }

  def remove(tx: ErgoTransaction): OrderedTxPool = {
    transactionsRegistry.get(tx.id).fold(this)(wtx =>
      OrderedTxPool(orderedTransactions - wtx, transactionsRegistry - tx.id, invalidated, outputs -- tx.outputs.map(_.id)).updateFamily(tx, -wtx.weight))
  }

  def invalidate(tx: ErgoTransaction): OrderedTxPool = {
    val inv = if (invalidated.size >= blacklistCapacity) invalidated - invalidated.firstKey else invalidated
    val ts = System.currentTimeMillis()
    transactionsRegistry.get(tx.id).fold(this)(wtx =>
      OrderedTxPool(orderedTransactions - wtx,
        transactionsRegistry - tx.id, inv.updated(tx.id, ts), outputs -- tx.outputs.map(_.id)).updateFamily(tx, -wtx.weight))
  }

  def filter(condition: ErgoTransaction => Boolean): OrderedTxPool = {
    orderedTransactions.foldLeft(this)((pool,entry) => {
      val tx = entry._2
      if (condition(tx)) pool else pool.remove(tx)
    })
  }

  def canAccept(tx: ErgoTransaction): Boolean = {
    !isInvalidated(tx.id) && !contains(tx.id) &&
      (size < settings.nodeSettings.mempoolCapacity ||
        weighted(tx).weight > orderedTransactions.firstKey.weight)
  }

  def contains(id: ModifierId): Boolean = transactionsRegistry.contains(id)

  def isInvalidated(id: ModifierId): Boolean = invalidated.contains(id)

  private def updateFamily(tx : ErgoTransaction, weight : Double) : OrderedTxPool = {
    tx.inputs.foldLeft(this)((pool,box) =>
      outputs.get(box.boxId).fold(pool)(wtx => {
        val parent = orderedTransactions(wtx)
        val new_wtx = WeightedTxId(wtx.id, wtx.weight + weight)
        val new_pool = OrderedTxPool(pool.orderedTransactions - wtx + (new_wtx -> parent),
          pool.transactionsRegistry.updated(parent.id, new_wtx),
          pool.invalidated,
          pool.outputs)
        new_pool.updateFamily(parent, weight)
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

  private implicit val ordWeigth: Ordering[WeightedTxId] = Ordering[(Double, ModifierId)].on(x => (x.weight, x.id))
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
