package org.ergoplatform.nodeView.mempool

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool._
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.transaction.MemoryPool
import scorex.core.transaction.state.TransactionValidation
import scorex.util.ModifierId

import scala.collection.immutable.TreeMap
import scala.util.Try

/**
  * Immutable transactions pool of limited size and blacklisting support.
  */
case class Pool(transactions: TreeMap[WeightedTxId, ErgoTransaction],
                weights: TreeMap[ModifierId, Long],
                invalidated: TreeMap[ModifierId, Long],
                settings: ErgoSettings) {

  private val mempoolCapacity = settings.nodeSettings.mempoolCapacity

  private val blacklistCapacity = mempoolCapacity * 4

  def size: Int = transactions.size

  def get(id: ModifierId): Option[ErgoTransaction] = weights.get(id).flatMap(w => transactions.get(WeightedTxId(id, w)))

  def put(tx: ErgoTransaction): Pool = {
    val (txs, ws) = if (transactions.size >= mempoolCapacity) {
      (transactions - transactions.firstKey) -> (weights - weights.firstKey)
    } else {
      transactions -> weights
    }
    val wtx = weighted(tx)(settings)
    Pool(txs.updated(wtx, tx), ws.updated(wtx.id, wtx.weight), invalidated, settings)
  }

  def remove(tx: ErgoTransaction): Pool = {
    Pool(transactions - weighted(tx)(settings), weights - tx.id, invalidated, settings)
  }

  def invalidate(tx: ErgoTransaction): Pool = {
    val inv = if (invalidated.size >= blacklistCapacity) invalidated - invalidated.firstKey else invalidated
    val ts = System.currentTimeMillis()
    Pool(transactions - weighted(tx)(settings), weights - tx.id, inv.updated(tx.id, ts), settings)
  }

  def filter(condition: ErgoTransaction => Boolean): Pool = {
    val txs = transactions.filter { case (_, v) => condition(v) }
    val ws = weights.filter(tx => transactions.get(WeightedTxId(tx._1, tx._2)).exists(condition))
    Pool(txs, ws, invalidated, settings)
  }

  def contains(id: ModifierId): Boolean = weights.contains(id)

  def isInvalidated(id: ModifierId): Boolean = invalidated.contains(id)

}

/**
  * Immutable memory pool implementation with transactions validation and priority management support.
  */
class ErgoMemPool private[mempool](pool: Pool, settings: ErgoSettings)
  extends MemoryPool[ErgoTransaction, ErgoMemPool] with ErgoMemPoolReader {

  import ErgoMemPool._

  override type NVCT = ErgoMemPool

  override def size: Int = pool.size

  override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = pool.get(modifierId)

  override def take(limit: Int): Iterable[ErgoTransaction] = pool.transactions.values.take(limit)

  override def getAll: Seq[ErgoTransaction] = pool.transactions.values.toSeq

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ids.flatMap(pool.get)

  override def getAllPrioritized: Seq[ErgoTransaction] = pool.transactions.values.toSeq.reverse

  override def put(tx: ErgoTransaction): Try[ErgoMemPool] = put(Seq(tx))

  override def put(txs: Iterable[ErgoTransaction]): Try[ErgoMemPool] = Try {
    putWithoutCheck(txs.filterNot(tx => pool.contains(tx.id)))
  }

  override def putWithoutCheck(txs: Iterable[ErgoTransaction]): ErgoMemPool = {
    val updatedPool = txs.toSeq.distinct.foldLeft(pool) { case (acc, tx) => acc.put(tx) }
    new ErgoMemPool(updatedPool, settings)
  }

  override def remove(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(pool.remove(tx), settings)
  }

  override def filter(condition: ErgoTransaction => Boolean): ErgoMemPool = {
    new ErgoMemPool(pool.filter(condition), settings)
  }

  def invalidate(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(pool.invalidate(tx), settings)
  }

  def putIfValid(tx: ErgoTransaction, state: ErgoState[_]): (ErgoMemPool, ProcessingOutcome) = {
    state match {
      case validator: TransactionValidation[ErgoTransaction@unchecked]
        if !pool.isInvalidated(tx.id) && !pool.contains(tx.id) &&
          (pool.size < settings.nodeSettings.mempoolCapacity ||
          weighted(tx)(settings).weight > pool.transactions.firstKey.weight) =>
        validator.validate(tx).fold(
          new ErgoMemPool(pool.invalidate(tx), settings) -> ProcessingOutcome.Invalidated(_),
          _ => new ErgoMemPool(pool.put(tx), settings) -> ProcessingOutcome.Accepted
        )
      case _ =>
        this -> ProcessingOutcome.Declined
    }
  }

}

object ErgoMemPool {

  sealed trait ProcessingOutcome

  object ProcessingOutcome {
    case object Accepted extends ProcessingOutcome
    case object Declined extends ProcessingOutcome
    case class Invalidated(e: Throwable) extends ProcessingOutcome
  }

  case class WeightedTxId(id: ModifierId, weight: Long) {
    // in order to avoid redundant allocations only `id` is used.
    override def equals(obj: Any): Boolean = obj match {
      case that: WeightedTxId => that.id == id
      case _ => false
    }
    override def hashCode(): Int = Ints.fromByteArray(Algos.decodeUnsafe(id.take(8)))
  }

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[ErgoTransaction]

  private implicit val ord: Ordering[WeightedTxId] = Ordering[(Long, ModifierId)].on(x => (x.weight, x.id))

  def empty(settings: ErgoSettings): ErgoMemPool = {
    val emptyPool = Pool(TreeMap.empty[WeightedTxId, ErgoTransaction], TreeMap.empty[ModifierId, Long],
      TreeMap.empty[ModifierId, Long], settings)
    new ErgoMemPool(emptyPool, settings)
  }

  def weighted(tx: ErgoTransaction)(settings: ErgoSettings): WeightedTxId = {
    val fee = tx.outputs
      .filter(b => java.util.Arrays.equals(b.propositionBytes, settings.feeProposition.bytes))
      .map(_.value)
      .sum
    WeightedTxId(tx.id, fee)
  }

}
