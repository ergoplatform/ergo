package org.ergoplatform.modifiers.mempool

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
  * @param isUsingBlockchainContext - whether any of the transaction inputs' ErgoTrees uses blockchain context
  * @param validationResult - validation result during last check
  */
class UnconfirmedTransaction(val transaction: ErgoTransaction,
                             val lastCost: Option[Int],
                             val createdTime: Long,
                             val lastCheckedTime: Long,
                             val transactionBytes: Option[Array[Byte]],
                             val source: Option[ConnectedPeer],
                             val isUsingBlockchainContext: Option[Boolean] = None,
                             val validationResult: Option[Boolean] = None)
  extends ScorexLogging {

  def id: ModifierId = transaction.id

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
      source,
      isUsingBlockchainContext,
      validationResult)
  }

  /**
    * Updates blockchain context usage flag, validation result, optional cost, and last checked time
    */
  def withValidationResult(usesBlockchainContext: Boolean,
                           isValid: Boolean,
                           costOpt: Option[Int]): UnconfirmedTransaction = {
    new UnconfirmedTransaction(
      transaction,
      lastCost = costOpt.orElse(lastCost),
      createdTime,
      lastCheckedTime = System.currentTimeMillis(),
      transactionBytes,
      source,
      isUsingBlockchainContext = Some(usesBlockchainContext),
      validationResult = Some(isValid))
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
