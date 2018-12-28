package org.ergoplatform.nodeView.mempool

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool._
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.transaction.MemoryPool
import scorex.core.transaction.state.TransactionValidation
import scorex.util.ModifierId

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.util.Try

/**
  * Immutable transactions pool of limited size and blacklisting support.
  */
case class Pool(transactions: TreeMap[ModifierId, ErgoTransaction],
                weightedTxs: TreeSet[WeightedTxId],
                invalidated: TreeMap[ModifierId, Long],
                settings: ErgoSettings) {

  private val mempoolCapacity = settings.nodeSettings.mempoolCapacity

  private val blacklistCapacity = mempoolCapacity * 4

  def size: Int = transactions.size

  def get(id: ModifierId): Option[ErgoTransaction] = transactions.get(id)

  def put(tx: ErgoTransaction): Pool = {
    val (txs, ws) = if (transactions.size >= mempoolCapacity) {
      (transactions - weightedTxs.firstKey.id) -> (weightedTxs - weightedTxs.firstKey)
    } else {
      transactions -> weightedTxs
    }
    Pool(txs.updated(tx.id, tx), ws.insert(weighted(tx)(settings)), invalidated, settings)
  }

  def remove(tx: ErgoTransaction): Pool = {
    Pool(transactions - tx.id, weightedTxs - weighted(tx)(settings), invalidated, settings)
  }

  def invalidate(tx: ErgoTransaction): Pool = {
    val inv = if (invalidated.size >= blacklistCapacity) invalidated - invalidated.firstKey else invalidated
    val ts = System.currentTimeMillis()
    Pool(transactions - tx.id, weightedTxs - weighted(tx)(settings), inv.updated(tx.id, ts), settings)
  }

  def filter(condition: ErgoTransaction => Boolean): Pool = {
    val txs = transactions.filter { case (_, v) => condition(v) }
    val ws = weightedTxs.filter(tx => transactions.get(tx.id).exists(condition))
    Pool(txs, ws, invalidated, settings)
  }

  def contains(id: ModifierId): Boolean = transactions.contains(id)

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

  override def getAllPrioritized: Seq[ErgoTransaction] = pool.weightedTxs.flatMap(tx => pool.get(tx.id)).toSeq.reverse

  override def put(tx: ErgoTransaction): Try[ErgoMemPool] = put(Seq(tx))

  override def put(txs: Iterable[ErgoTransaction]): Try[ErgoMemPool] = Try {
    putWithoutCheck(txs.filterNot(tx => pool.contains(tx.id)))
  }

  override def putWithoutCheck(txs: Iterable[ErgoTransaction]): ErgoMemPool = {
    val updatedPool = txs.foldLeft(pool) { case (acc, tx) =>
      if (!acc.contains(tx.id)) acc.put(tx) else acc
    }
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
          weighted(tx)(settings).weight > pool.weightedTxs.firstKey.weight) =>
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
    val emptyPool = Pool(
      TreeMap.empty[ModifierId, ErgoTransaction], TreeSet.empty, TreeMap.empty[ModifierId, Long], settings)
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
