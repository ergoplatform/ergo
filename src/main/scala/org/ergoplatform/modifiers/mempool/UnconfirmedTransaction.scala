package org.ergoplatform.modifiers.mempool

import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, ScorexLogging}
import scorex.util.serialization.{Reader, Writer}


case class UnconfirmedTransaction(transaction: ErgoTransaction,
                                  lastCost: Option[Int],
                                  createdTime: Long,
                                  lastCheckedTime: Long,
                                  transactionBytes: Option[Array[Byte]])
  extends ScorexLogging {

  def id: ModifierId = transaction.id

  /**
    * Updates cost and last checked time of unconfirmed transaction
    */
  def onRecheck(cost: Int): UnconfirmedTransaction = {
    copy(lastCost = Some(cost), lastCheckedTime = System.currentTimeMillis())
  }

}

object UnconfirmedTransaction {

  def apply(tx: ErgoTransaction): UnconfirmedTransaction = {
    val now = System.currentTimeMillis()
    UnconfirmedTransaction(tx, None, now, now, Some(tx.bytes))
  }

  def apply(tx: ErgoTransaction, txBytes: Array[Byte]): UnconfirmedTransaction = {
    val now = System.currentTimeMillis()
    UnconfirmedTransaction(tx, None, now, now, Some(txBytes))
  }

}

