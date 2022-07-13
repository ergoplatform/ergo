package org.ergoplatform.modifiers.mempool

import org.ergoplatform.{ErgoLikeTransaction, ErgoLikeTransactionSerializer}
import scorex.core.consensus.ContainsModifiers
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.Transaction
import scorex.core.utils.{NetworkTime, NetworkTimeProvider, TimeProvider}
import scorex.util.ScorexLogging
import scorex.util.serialization.{Reader, Writer}
import sigmastate.serialization.ConstantStore
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}


case class UnconfirmedTransaction(
  transaction: ErgoTransaction,
  lastCost: Option[Int],
  createdTime: Long,
  lastCheckedTime: Long,
  transactionBytes: Option[Seq[Byte]]
) extends Transaction
  with ScorexLogging {
//  with ContainsModifiers[UnconfirmedTransaction] {

  def updateCost(cost: Int): UnconfirmedTransaction = {
    copy(lastCost = Some(cost), lastCheckedTime = System.currentTimeMillis())
  }

  override val messageToSign: Array[Byte] = _
  override type M = UnconfirmedTransaction

  override def serializer: ScorexSerializer[UnconfirmedTransaction] = UnconfirmedTransactionSerializer
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
    val transactionBytesOpt = r.getOption(r.getBytes(256).toSeq) //TODO transaction bytes size?

    UnconfirmedTransaction(ergoTransaction, lastCostOpt, createdTime, lastCheckedTime, transactionBytesOpt)
  }
}
