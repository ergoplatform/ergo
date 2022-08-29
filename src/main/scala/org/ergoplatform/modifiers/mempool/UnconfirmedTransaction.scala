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
  */
case class UnconfirmedTransaction(transaction: ErgoTransaction,
                                  lastCost: Option[Int],
                                  createdTime: Long,
                                  lastCheckedTime: Long,
                                  transactionBytes: Option[Array[Byte]],
                                  source: Option[ConnectedPeer])
  extends ScorexLogging {

  def id: ModifierId = transaction.id

  /**
    * Updates cost and last checked time of unconfirmed transaction
    */
  def withCost(cost: Int): UnconfirmedTransaction = {
    copy(lastCost = Some(cost), lastCheckedTime = System.currentTimeMillis())
  }

}

object UnconfirmedTransaction {

  def apply(tx: ErgoTransaction, source: Option[ConnectedPeer]): UnconfirmedTransaction = {
    val now = System.currentTimeMillis()
    UnconfirmedTransaction(tx, None, now, now, Some(tx.bytes), source)
  }

  def apply(tx: ErgoTransaction, txBytes: Array[Byte], source: Option[ConnectedPeer]): UnconfirmedTransaction = {
    val now = System.currentTimeMillis()
    UnconfirmedTransaction(tx, None, now, now, Some(txBytes), source)
  }

}

