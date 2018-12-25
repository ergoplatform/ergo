package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.ErgoSettings
import scorex.core.transaction.MemoryPool
import scorex.core.transaction.state.TransactionValidation
import scorex.util.ModifierId

import scala.collection.immutable.TreeMap
import scala.util.{Failure, Success, Try}

/**
  * Memory pool with limited size implementation.
  */
class ErgoMemPool private[mempool](val unconfirmed: TreeMap[ModifierId, ErgoTransaction],
                                   val weights: TreeMap[Long, ModifierId],
                                   invalidated: TreeMap[ModifierId, Long],
                                   settings: ErgoSettings)
  extends MemoryPool[ErgoTransaction, ErgoMemPool] with ErgoMemPoolReader {

  import ErgoMemPool._

  override type NVCT = ErgoMemPool

  private val blacklistCapacity = settings.nodeSettings.mempoolCapacity * 4

  override def put(tx: ErgoTransaction): Try[ErgoMemPool] = put(Seq(tx))

  override def put(txs: Iterable[ErgoTransaction]): Try[ErgoMemPool] = Try {
    putWithoutCheck(txs.filterNot(tx => unconfirmed.contains(tx.id)))
  }

  override def putWithoutCheck(txs: Iterable[ErgoTransaction]): ErgoMemPool = {
    val (newPool, newWs) = txs.foldLeft(unconfirmed, weights) { case ((acc, ws), tx) => updatePoolWith(tx, acc, ws) }
    new ErgoMemPool(newPool, newWs, invalidated, settings)
  }

  override def remove(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(unconfirmed - tx.id, weights, invalidated, settings)
  }

  override def filter(condition: ErgoTransaction => Boolean): ErgoMemPool = {
    val newPool = unconfirmed.filter { case (_, v) =>
      condition(v)
    }
    new ErgoMemPool(newPool, weights, invalidated, settings)
  }

  def invalidate(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(unconfirmed - tx.id, weights, updateInvalidatedWith(tx), settings)
  }

  def putIfValid(tx: ErgoTransaction, state: ErgoState[_]): (ErgoMemPool, ProcessingOutcome) = {
    state match {
      case validator: TransactionValidation[ErgoTransaction@unchecked]
        if !invalidated.contains(tx.id) && !unconfirmed.contains(tx.id) &&
          (unconfirmed.size < settings.nodeSettings.mempoolCapacity ||
          weightOf(tx) > weights.headOption.map(_._1).getOrElse(0L)) =>
        validator.validate(tx) match {
          case Success(_) =>
            val (pool, ws) = updatePoolWith(tx)
            new ErgoMemPool(pool, ws, invalidated, settings) -> ProcessingOutcome.Accepted
          case Failure(e) =>
            new ErgoMemPool(unconfirmed, weights, updateInvalidatedWith(tx), settings) -> ProcessingOutcome.Invalidated(e)
        }
      case _ =>
        this -> ProcessingOutcome.Declined
    }
  }

  private def weightOf(tx: ErgoTransaction): Long = {
    val propositionBytes = ErgoState.feeProposition(settings.chainSettings.monetary.minerRewardDelay).bytes
    tx.outputs
      .filter(b => java.util.Arrays.equals(b.propositionBytes, propositionBytes))
      .map(_.value)
      .sum
  }

  /**
    * Updated memory pool with new transaction, if pool ran out of size limit then
    * transaction with the smallest weight is replaced by the new one
    */
  private def updatePoolWith(tx: ErgoTransaction,
                             pool: TreeMap[ModifierId, ErgoTransaction] = unconfirmed,
                             ws: TreeMap[Long, ModifierId] = weights) = {
    if (pool.size >= settings.nodeSettings.mempoolCapacity) {
      (pool - ws.head._2).updated(tx.id, tx) -> ws.tail.updated(weightOf(tx), tx.id)
    } else {
      pool.updated(tx.id, tx) -> ws.updated(weightOf(tx), tx.id)
    }
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

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[ErgoTransaction]

  def empty(settings: ErgoSettings): ErgoMemPool = {
    new ErgoMemPool(TreeMap.empty[ModifierId, ErgoTransaction], TreeMap.empty, TreeMap.empty, settings)
  }

}
