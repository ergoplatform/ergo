package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool.WeightedTx
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.ErgoSettings
import scorex.core.transaction.MemoryPool
import scorex.core.transaction.state.TransactionValidation
import scorex.util.ModifierId

import scala.collection.immutable.TreeMap
import scala.util.Try

/**
  * Memory pool with limited size and transaction priority management.
  */
class ErgoMemPool private[mempool](val unconfirmed: TreeMap[WeightedTx, ErgoTransaction],
                                   invalidated: TreeMap[ModifierId, Long],
                                   settings: ErgoSettings)
  extends MemoryPool[ErgoTransaction, ErgoMemPool] with ErgoMemPoolReader {

  import ErgoMemPool._

  override type NVCT = ErgoMemPool

  private val blacklistCapacity = settings.nodeSettings.mempoolCapacity * 4

  private val propositionBytes = settings.feeProposition.bytes

  override def put(tx: ErgoTransaction): Try[ErgoMemPool] = put(Seq(tx))

  override def put(txs: Iterable[ErgoTransaction]): Try[ErgoMemPool] = Try {
    putWithoutCheck(txs.filterNot(tx => unconfirmed.contains(weighted(tx))))
  }

  override def putWithoutCheck(txs: Iterable[ErgoTransaction]): ErgoMemPool = {
    val newPool = txs.foldLeft(unconfirmed) { case (acc, tx) => updatePoolWith(tx, acc) }
    new ErgoMemPool(newPool, invalidated, settings)
  }

  override def remove(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(unconfirmed - weighted(tx), invalidated, settings)
  }

  override def filter(condition: ErgoTransaction => Boolean): ErgoMemPool = {
    val newPool = unconfirmed.filter { case (_, v) =>
      condition(v)
    }
    new ErgoMemPool(newPool, invalidated, settings)
  }

  def invalidate(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(unconfirmed - weighted(tx), updateInvalidatedWith(tx), settings)
  }

  def putIfValid(tx: ErgoTransaction, state: ErgoState[_]): (ErgoMemPool, ProcessingOutcome) = {
    state match {
      case validator: TransactionValidation[ErgoTransaction@unchecked]
        if !invalidated.contains(tx.id) && !unconfirmed.contains(weighted(tx)) &&
          (unconfirmed.size < settings.nodeSettings.mempoolCapacity ||
          weighted(tx).weight > unconfirmed.firstKey.weight) =>
        validator.validate(tx).fold(
          new ErgoMemPool(unconfirmed, updateInvalidatedWith(tx), settings) -> ProcessingOutcome.Invalidated(_),
          _ => new ErgoMemPool(updatePoolWith(tx, unconfirmed), invalidated, settings) -> ProcessingOutcome.Accepted
        )
      case _ =>
        this -> ProcessingOutcome.Declined
    }
  }

  private def weighted(tx: ErgoTransaction): WeightedTx = {
    val fee = tx.outputs
      .filter(b => java.util.Arrays.equals(b.propositionBytes, propositionBytes))
      .map(_.value)
      .sum
    WeightedTx(tx.id, fee)
  }

  /**
    * Updated memory pool with new transaction, if pool ran out of size limit then
    * transaction with the smallest weight is replaced by the new one
    */
  private def updatePoolWith(tx: ErgoTransaction,
                             pool: TreeMap[WeightedTx, ErgoTransaction]) = {
    (if (pool.size >= settings.nodeSettings.mempoolCapacity) pool - pool.firstKey else pool)
      .updated(weighted(tx), tx)
  }

  private def updateInvalidatedWith(tx: ErgoTransaction) = {
    (if (invalidated.size >= blacklistCapacity) invalidated - invalidated.toList.minBy(_._2)._1 else invalidated)
      .updated(tx.id, System.currentTimeMillis)
  }

}

object ErgoMemPool {

  sealed trait ProcessingOutcome

  object ProcessingOutcome {
    case object Accepted extends ProcessingOutcome
    case object Declined extends ProcessingOutcome
    case class Invalidated(e: Throwable) extends ProcessingOutcome
  }

  case class WeightedTx(id: ModifierId, weight: Long)

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[ErgoTransaction]

  private implicit val ord: Ordering[WeightedTx] = Ordering[(Long, ModifierId)].on(x => (x.weight, x.id))

  def empty(settings: ErgoSettings): ErgoMemPool = {
    new ErgoMemPool(TreeMap.empty[WeightedTx, ErgoTransaction], TreeMap.empty[ModifierId, Long], settings)
  }

}
