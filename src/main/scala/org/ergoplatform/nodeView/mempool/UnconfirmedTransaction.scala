package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool.SortingOption
import org.ergoplatform.settings.MonetarySettings
import scorex.core.network.ConnectedPeer
import scorex.util.{ModifierId, ScorexLogging}

/**
  * Wrapper for unconfirmed transaction and corresponding data
  *
  * @param transaction - unconfirmed transaction
  * @param lastCost - validation cost during last check
  * @param createdTime - when transaction entered the pool
  * @param lastCheckedTime - when last validity check was done
  * @param transactionBytes - transaction bytes, to avoid serializations when we send it over the wire
  * @param source - peer which delivered the transaction (None if transaction submitted via API)
  */
class UnconfirmedTransaction(val transaction: ErgoTransaction,
                             val lastCost: Option[Int],
                             val createdTime: Long,
                             val lastCheckedTime: Long,
                             val transactionBytes: Option[Array[Byte]],
                             val source: Option[ConnectedPeer])
  extends ScorexLogging {

  def id: ModifierId = transaction.id

  /**
    * When there's no reason to re-check transactions immediately, we assign fake cost to them
    */
  private val FakeCost = 1000

  private[mempool] var _feeFactor: Int = -1
  def feeFactor(implicit sortingOption: SortingOption): Int = {
    if(_feeFactor == -1) {
      sortingOption match {
        case SortingOption.FeePerByte =>
          _feeFactor = transactionBytes.map(_.length).getOrElse(transaction.size)
        case SortingOption.FeePerCycle =>
          _feeFactor = lastCost.getOrElse(FakeCost)
      }
    }
    _feeFactor
  }

   private def feePerFactor(implicit ms: MonetarySettings, sortingOption: SortingOption): Long = {
     val fee = transaction.outputs
       .filter(b => java.util.Arrays.equals(b.propositionBytes, ms.feePropositionBytes))
       .map(_.value)
       .sum
     // We multiply by 1024 for better precision
     fee * 1024 / feeFactor
  }

  var _weight: Long = -1
  def weight(implicit ms: MonetarySettings, sortingOption: SortingOption): Long = {
    if(_weight == -1)
      _weight = feePerFactor
    _weight
  }

  def addWeight(weight: Long): UnconfirmedTransaction = {
    _weight = weight
    this
  }

  /**
    * Updates cost and last checked time of unconfirmed transaction
    */
  def withCost(cost: Int): UnconfirmedTransaction = {
    new UnconfirmedTransaction(
      transaction,
      lastCost = Some(cost),
      createdTime,
      lastCheckedTime = System.currentTimeMillis(),
      transactionBytes,
      source)
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: UnconfirmedTransaction => that.id == id
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()
}

object UnconfirmedTransaction {

  def apply(tx: ErgoTransaction, source: Option[ConnectedPeer]): UnconfirmedTransaction = {
    val now = System.currentTimeMillis()
    new UnconfirmedTransaction(tx, None, now, now, Some(tx.bytes), source)
  }

  def apply(tx: ErgoTransaction, txBytes: Array[Byte], source: Option[ConnectedPeer]): UnconfirmedTransaction = {
    val now = System.currentTimeMillis()
    new UnconfirmedTransaction(tx, None, now, now, Some(txBytes), source)
  }

}

