package org.ergoplatform.modifiers.mempool

import scorex.core.serialization.ScorexSerializer
import scorex.util.ScorexLogging
import scorex.util.serialization.{Reader, Writer}


case class UnconfirmedTransaction(
                                   transaction: ErgoTransaction,
                                   lastCost: Option[Int],
                                   createdTime: Long,
                                   lastCheckedTime: Long,
                                   transactionBytes: Option[Array[Byte]])
  extends ScorexLogging {
//    with ErgoNodeViewModifier {

  def updateCost(cost: Int): UnconfirmedTransaction = {
    copy(lastCost = Some(cost), lastCheckedTime = System.currentTimeMillis())
  }

//  override def serializedId: Array[Byte] = ???
//
//  override val sizeOpt: Option[Int] = _
}

object UnconfirmedTransaction {

  def apply(tx: ErgoTransaction): UnconfirmedTransaction = {
    UnconfirmedTransaction(
      tx,
      None,
      0,
      0,
      Some(ErgoTransactionSerializer.toBytes(tx))
    )
  }
}

object UnconfirmedTransactionSerializer extends ScorexSerializer[UnconfirmedTransaction] {

  override def serialize(unconfirmedTx: UnconfirmedTransaction, w: Writer): Unit = {
    ErgoTransactionSerializer.serialize(unconfirmedTx.transaction, w)
    w.putOption(unconfirmedTx.lastCost)(_.putUInt(_))
    w.putULong(unconfirmedTx.createdTime)
    w.putULong(unconfirmedTx.lastCheckedTime)
    w.putOption(unconfirmedTx.transactionBytes)((_,d) => w.putBytes(d.toArray))
  }

  override def parse(r: Reader): UnconfirmedTransaction = {

    val ergoTransaction = ErgoTransactionSerializer.parse(r)
    val lastCostOpt = r.getOption(r.getUInt().toInt)
    val createdTime = r.getULong()
    val lastCheckedTime = r.getULong()
    val transactionBytesOpt = r.getOption(r.getBytes(256)) //TODO transaction bytes size?

    UnconfirmedTransaction(ergoTransaction, lastCostOpt, createdTime, lastCheckedTime, transactionBytesOpt)
  }
}
