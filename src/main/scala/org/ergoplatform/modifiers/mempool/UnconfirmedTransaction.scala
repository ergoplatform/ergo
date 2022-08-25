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

  def withCost(cost: Int): UnconfirmedTransaction = {
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

object UnconfirmedTransactionSerializer extends ScorexSerializer[UnconfirmedTransaction] {

  override def serialize(unconfirmedTx: UnconfirmedTransaction, w: Writer): Unit = {
    ErgoTransactionSerializer.serialize(unconfirmedTx.transaction, w)
    w.putOption(unconfirmedTx.lastCost)(_.putUInt(_))
    w.putULong(unconfirmedTx.createdTime)
    w.putULong(unconfirmedTx.lastCheckedTime)
    w.putOption(unconfirmedTx.transactionBytes){(_,d) =>
      w.putInt(d.length)
      w.putBytes(d)
    }

  }

  // this serializer used internally only, so we do not validate data here
  override def parse(r: Reader): UnconfirmedTransaction = {
    val ergoTransaction = ErgoTransactionSerializer.parse(r)
    val lastCostOpt = r.getOption(r.getUInt().toInt)
    val createdTime = r.getULong()
    val lastCheckedTime = r.getULong()
    val transactionBytesOpt = r.getOption{
      val size = r.getInt()
      r.getBytes(size)
    }

    UnconfirmedTransaction(ergoTransaction, lastCostOpt, createdTime, lastCheckedTime, transactionBytesOpt)
  }
}
