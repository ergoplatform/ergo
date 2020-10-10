package org.ergoplatform.nodeView.mempool

import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.settings.{ErgoSettings, MonetarySettings}
import scorex.core.transaction.MemoryPool
import scorex.core.transaction.state.TransactionValidation
import scorex.util.ModifierId

import scala.util.Try

/**
  * Immutable memory pool implementation.
  */
class ErgoMemPool private[mempool](pool: OrderedTxPool)(implicit settings: ErgoSettings)
  extends MemoryPool[ErgoTransaction, ErgoMemPool] with ErgoMemPoolReader {

  import ErgoMemPool._
  import EmissionRules.CoinsInOneErgo

  private implicit val monetarySettings: MonetarySettings = settings.chainSettings.monetary

  override type NVCT = ErgoMemPool

  override def size: Int = pool.size

  override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = pool.get(modifierId)

  override def take(limit: Int): Iterable[ErgoTransaction] = pool.orderedTransactions.values.take(limit)

  override def getAll: Seq[ErgoTransaction] = pool.orderedTransactions.values.toSeq

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ids.flatMap(pool.get)

  /**
    * Returns all transactions resided in pool sorted by weight in descending order
    */
  override def getAllPrioritized: Seq[ErgoTransaction] = pool.orderedTransactions.values.toSeq

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

  def invalidate(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(pool.invalidate(tx))
  }

  def acceptIfNoDoubleSpend(tx: ErgoTransaction): (ErgoMemPool, ProcessingOutcome) = {
    val doubleSpendingInputOpt = tx.inputs.find { inp =>
      pool.inputs.contains(inp.boxId)
    }
    doubleSpendingInputOpt match {
      case Some(doubleSpendingInput) =>
        val doubleWtx = pool.inputs.get(doubleSpendingInput.boxId).get //.get
        if (weighted(tx).weight > doubleWtx.weight) {
          new ErgoMemPool(pool.put(tx)) -> ProcessingOutcome.Accepted
        } else {
          this -> ProcessingOutcome.DoubleSpendingLoser(doubleWtx.id)
        }
      case None => new ErgoMemPool(pool.put(tx)) -> ProcessingOutcome.Accepted
    }
  }

  def process(tx: ErgoTransaction, state: ErgoState[_]): (ErgoMemPool, ProcessingOutcome) = {
    val fee = extractFee(tx)
    val minFee = settings.nodeSettings.minimalFeeAmount
    val canAccept = pool.canAccept(tx)

    if (fee >= minFee) {
      if (canAccept) {
        state match {
          case utxo: UtxoState =>
            // Allow proceeded transaction to spend outputs of pooled transactions.
            utxo.withTransactions(getAll).validate(tx).fold(
              new ErgoMemPool(pool.invalidate(tx)) -> ProcessingOutcome.Invalidated(_),
              _ => acceptIfNoDoubleSpend(tx)
            )
          case validator: TransactionValidation[ErgoTransaction@unchecked] =>
            // transaction validation currently works only for UtxoState, so this branch currently
            // will not be triggered probably
            validator.validate(tx).fold(
              new ErgoMemPool(pool.invalidate(tx)) -> ProcessingOutcome.Invalidated(_),
              _ => new ErgoMemPool(pool.put(tx)) -> ProcessingOutcome.Accepted
            )
          case _ =>
            new ErgoMemPool(pool.put(tx)) -> ProcessingOutcome.Accepted
        }
      } else {
        this -> ProcessingOutcome.Declined(
          new Exception(s"Pool can not accept transaction ${tx.id}, it is invalidated earlier or pool is full"))
      }
    } else {
      this -> ProcessingOutcome.Declined(
        new Exception(s"Min fee not met: ${minFee.toDouble / CoinsInOneErgo} ergs required, " +
          s"${fee.toDouble / CoinsInOneErgo} ergs given")
      )
    }
  }

  def weightedTransactionIds(limit: Int): Seq[WeightedTxId] = pool.orderedTransactions.keysIterator.take(limit).toSeq

}

object ErgoMemPool {

  sealed trait ProcessingOutcome

  object ProcessingOutcome {

    case object Accepted extends ProcessingOutcome

    case class DoubleSpendingLoser(winnerTxId: ModifierId) extends ProcessingOutcome

    case class Declined(e: Throwable) extends ProcessingOutcome

    case class Invalidated(e: Throwable) extends ProcessingOutcome

  }

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[ErgoTransaction]

  def empty(settings: ErgoSettings): ErgoMemPool =
    new ErgoMemPool(OrderedTxPool.empty(settings))(settings)

  private[mempool] def extractFee(tx: ErgoTransaction)(implicit ms: MonetarySettings): Long =
    tx.outputs
      .filter(_.ergoTree == ms.feeProposition)
      .map(_.value)
      .sum

  private[mempool] def weighted(tx: ErgoTransaction)(implicit ms: MonetarySettings): WeightedTxId = {
    val fee = extractFee(tx)
    // We multiply by 1024 for better precision
    WeightedTxId(tx.id, fee * 1024 / tx.size)
  }

}
