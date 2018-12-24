package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.ErgoSettings
import scorex.core.transaction.MemoryPool
import scorex.core.transaction.state.TransactionValidation
import scorex.util.ModifierId

import scala.collection.immutable.TreeMap
import scala.util.Try

/**
  * Memory pool with limited size implementation.
  */
class ErgoMemPool private[mempool](val unconfirmed: TreeMap[ModifierId, (ErgoTransaction, Long)],
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
    val newPool = txs.foldLeft(unconfirmed) { case (acc, tx) => acc.updated(tx.id, weighted(tx)) }
    new ErgoMemPool(newPool, invalidated, settings)
  }

  override def remove(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(unconfirmed - tx.id, invalidated, settings)
  }

  override def filter(condition: ErgoTransaction => Boolean): ErgoMemPool = {
    val newPool = unconfirmed.filter { case (_, v) =>
      condition(v._1)
    }
    new ErgoMemPool(newPool, invalidated, settings)
  }

  def invalidate(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(unconfirmed - tx.id, updateInvalidatedWith(tx), settings)
  }

  def putIfValid(tx: ErgoTransaction, state: ErgoState[_]): (ErgoMemPool, AppendingOutcome) = {
    state match {
      case validator: TransactionValidation[ErgoTransaction@unchecked]
        if !invalidated.contains(tx.id) && !unconfirmed.contains(tx.id) &&
          (unconfirmed.size < settings.nodeSettings.mempoolCapacity ||
          weighted(tx)._2 > unconfirmed.headOption.map(_._2._2).getOrElse(0L)) =>
        validator.validate(tx).fold(
          new ErgoMemPool(unconfirmed, updateInvalidatedWith(tx), settings) -> AppendingOutcome.Invalidated(_),
          _ => new ErgoMemPool(updatePoolWith(tx), invalidated, settings) -> AppendingOutcome.Appended
        )
      case _ =>
        this -> AppendingOutcome.Declined
    }
  }

  private def weighted(tx: ErgoTransaction): (ErgoTransaction, Long) = {
    val propositionBytes = ErgoState.feeProposition(settings.chainSettings.monetary.minerRewardDelay).bytes
    val fee = tx.outputs
      .filter(b => java.util.Arrays.equals(b.propositionBytes, propositionBytes))
      .map(_.value)
      .sum
    tx -> fee
  }

  private def updatePoolWith(tx: ErgoTransaction) = {
    if (unconfirmed.size >= settings.nodeSettings.mempoolCapacity) {
      unconfirmed - unconfirmed.firstKey
    } else {
      unconfirmed
    }
  }.updated(tx.id, weighted(tx))

  private def updateInvalidatedWith(tx: ErgoTransaction) = {
    if (invalidated.size >= blacklistCapacity) {
      invalidated - invalidated.firstKey
    } else {
      invalidated
    }
  }.updated(tx.id, System.currentTimeMillis())

}

object ErgoMemPool {

  sealed trait AppendingOutcome

  object AppendingOutcome {
    case object Appended extends AppendingOutcome
    case object Declined extends AppendingOutcome
    case class Invalidated(e: Throwable) extends AppendingOutcome
  }

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[ErgoTransaction]

  private implicit val blacklistOrd: Ordering[(ModifierId, Long)] = Ordering.by(_._1)
  private implicit val unconfirmedOrd: Ordering[(ModifierId, (ErgoTransaction, Long))] = Ordering.by(_._2._2)

  def empty(settings: ErgoSettings): ErgoMemPool = {
    new ErgoMemPool(TreeMap.empty[ModifierId, (ErgoTransaction, Long)], TreeMap.empty, settings)
  }

}
