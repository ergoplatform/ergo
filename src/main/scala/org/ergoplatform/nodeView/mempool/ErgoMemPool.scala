package org.ergoplatform.nodeView.mempool

import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.ErgoSettings
import scorex.core.transaction.MemoryPool
import scorex.core.transaction.state.TransactionValidation
import scorex.util.ModifierId

import scala.util.{Random, Try}

/**
  * Immutable memory pool implementation.
  */
class ErgoMemPool private[mempool](pool: OrderedTxPool)(implicit settings: ErgoSettings)
  extends MemoryPool[ErgoTransaction, ErgoMemPool] with ErgoMemPoolReader {

  import ErgoMemPool._

  override type NVCT = ErgoMemPool

  override def size: Int = pool.size

  override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = pool.get(modifierId)

  override def take(limit: Int): Iterable[ErgoTransaction] = pool.orderedTransactions.values.take(limit)

  override def getAll: Seq[ErgoTransaction] = pool.orderedTransactions.values.toSeq

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ids.flatMap(pool.get)

  override def getAllPrioritized: Seq[ErgoTransaction] = pool.orderedTransactions.values.toSeq.reverse

  override def put(tx: ErgoTransaction): Try[ErgoMemPool] = put(Seq(tx))

  override def put(txs: Iterable[ErgoTransaction]): Try[ErgoMemPool] = Try {
    putWithoutCheck(txs.filterNot(tx => pool.contains(tx.id)))
  }

  override def putWithoutCheck(txs: Iterable[ErgoTransaction]): ErgoMemPool = {
    val updatedPool = txs.toSeq.distinct.foldLeft(pool) { case (acc, tx) => acc.put(tx) }
    new ErgoMemPool(updatedPool)
  }

  override def remove(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(pool.remove(tx))
  }

  override def filter(condition: ErgoTransaction => Boolean): ErgoMemPool = {
    new ErgoMemPool(pool.filter(condition))
  }

  override def digest(size: Int): Seq[ErgoTransaction] = {
    val txs = pool.orderedTransactions.values.toArray
    val (digest, _, _) = (0 to size).foldLeft(Seq.empty[ErgoTransaction], txs, txs.length) {
      case ((acc, left, leftSize), _) =>
        val pick = Random.nextInt(leftSize)
        (acc :+ left(pick), left.take(pick) ++ left.drop(pick + 1), leftSize - 1)
    }
    digest
  }

  def invalidate(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(pool.invalidate(tx))
  }

  def process(tx: ErgoTransaction, state: ErgoState[_]): (ErgoMemPool, ProcessingOutcome) = {
    val fee = extractFee(tx)
    val minFee = settings.nodeSettings.minimalFeeAmount
    if (fee >= minFee) {
      state match {
        case validator: TransactionValidation[ErgoTransaction@unchecked] if pool.canAccept(tx) =>
          validator.validate(tx).fold(
            new ErgoMemPool(pool.invalidate(tx)) -> ProcessingOutcome.Invalidated(_),
            _ => new ErgoMemPool(pool.put(tx)) -> ProcessingOutcome.Accepted
          )
        case _ =>
          this -> ProcessingOutcome.Declined(
            new Exception("Transaction validation not supported"))
      }
    } else {
      val den = EmissionRules.CoinsInOneErgo
      this -> ProcessingOutcome.Declined(
        new Exception(
          s"Minimal fee amount not met. ${minFee.toDouble / den} Ergo required, ${fee.toDouble / den} Ergo given")
      )
    }
  }

  private def extractFee(tx: ErgoTransaction): Long = {
    val propositionBytes = settings.chainSettings.monetary.feePropositionBytes
    ErgoState.boxChanges(Seq(tx))._2
      .filter(_.ergoTree == settings.chainSettings.monetary.feeProposition)
      .map(_.value)
      .sum
  }

}

object ErgoMemPool {

  sealed trait ProcessingOutcome

  object ProcessingOutcome {
    case object Accepted extends ProcessingOutcome
    case class Declined(e: Throwable) extends ProcessingOutcome
    case class Invalidated(e: Throwable) extends ProcessingOutcome
  }

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[ErgoTransaction]

  def empty(settings: ErgoSettings): ErgoMemPool =
    new ErgoMemPool(OrderedTxPool.empty(settings))(settings)

}
