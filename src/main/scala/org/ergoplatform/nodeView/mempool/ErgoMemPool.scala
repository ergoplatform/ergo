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
  * TODO This is simplified implementation of Memory pool.
  * MemPool should:
  * - have limited size
  * - replace transactions with the lowest fee if size limit is reached
  * - validate transactions when put (is called)
  * - clean transactions, that become invalid
  */
class ErgoMemPool private[mempool](val unconfirmed: TreeMap[ModifierId, (ErgoTransaction, Long)],
                                   blacklisted: TreeMap[ModifierId, Long],
                                   settings: ErgoSettings)
  extends MemoryPool[ErgoTransaction, ErgoMemPool] with ErgoMemPoolReader {

  import ErgoMemPool._

  override type NVCT = ErgoMemPool

  override def put(tx: ErgoTransaction): Try[ErgoMemPool] = put(Seq(tx))

  override def put(txs: Iterable[ErgoTransaction]): Try[ErgoMemPool] = Try {
    putWithoutCheck(txs.filterNot(tx => unconfirmed.contains(tx.id)))
  }

  override def putWithoutCheck(txs: Iterable[ErgoTransaction]): ErgoMemPool = {
    val newPool = txs.foldLeft(unconfirmed) { case (acc, tx) => acc.updated(tx.id, weighted(tx)) }
    new ErgoMemPool(newPool, blacklisted, settings)
  }

  override def remove(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(unconfirmed - tx.id, blacklisted, settings)
  }

  override def filter(condition: ErgoTransaction => Boolean): ErgoMemPool = {
    val newPool = unconfirmed.filter { case (_, v) =>
      condition(v._1)
    }
    new ErgoMemPool(newPool, blacklisted, settings)
  }

  def putIfValid(tx: ErgoTransaction, state: ErgoState[_]): (ErgoMemPool, AppendingOutcome) = {
    state match {
      case validator: TransactionValidation[ErgoTransaction@unchecked] =>
        if (!blacklisted.contains(tx.id) && !unconfirmed.contains(tx.id)) {
          val ts = System.currentTimeMillis()
          validator.validate(tx).fold(
            new ErgoMemPool(unconfirmed.updated(tx.id, weighted(tx)), blacklisted, settings) -> AppendingOutcome.Blacklisted(_),
            _ => new ErgoMemPool(unconfirmed, blacklisted.updated(tx.id, ts), settings) -> AppendingOutcome.Appended
          )
        } else {
          this -> AppendingOutcome.Denied
        }
      case _ =>
        this -> AppendingOutcome.Denied
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

}

object ErgoMemPool {

  sealed trait AppendingOutcome

  object AppendingOutcome {
    case object Appended extends AppendingOutcome
    case object Denied extends AppendingOutcome
    case class Blacklisted(e: Throwable) extends AppendingOutcome
  }

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[ErgoTransaction]

  private implicit val blacklistOrd: Ordering[(ModifierId, Long)] = Ordering.by(_._1)
  private implicit val unconfirmedOrd: Ordering[(ModifierId, (ErgoTransaction, Long))] = Ordering.by(_._2._2)

  def empty(settings: ErgoSettings): ErgoMemPool = {
    new ErgoMemPool(TreeMap.empty[ModifierId, (ErgoTransaction, Long)], TreeMap.empty, settings)
  }

}
