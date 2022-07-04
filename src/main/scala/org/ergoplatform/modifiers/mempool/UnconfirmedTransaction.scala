package org.ergoplatform.modifiers.mempool

import org.ergoplatform.{ErgoLikeTransaction, ErgoLikeTransactionSerializer}
import scorex.core.consensus.ContainsModifiers
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.Transaction
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
    copy(lastCost = Some(cost))
    //TODO set lastCheckedTime
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
    w.putOption(unconfirmedTx.lastCost)(_.putInt(_))
    w.putULong(unconfirmedTx.createdTime)
    w.putULong(unconfirmedTx.lastCheckedTime)
    w.putOption(unconfirmedTx.transactionBytes)((_,d) => w.putBytes(d.toArray))
  }

  override def parse(r: Reader): UnconfirmedTransaction = {

    val ergoTransaction = ErgoTransactionSerializer.parse(r)
    val lastCost = r.getOption(r.getUInt())
    val createdTime = r.getULong()
    val lastCheckedTime = r.getULong()


    val elt = ErgoLikeTransactionSerializer.parse(r)
    ErgoTransaction(elt.inputs, elt.dataInputs, elt.outputCandidates)
  }
}
