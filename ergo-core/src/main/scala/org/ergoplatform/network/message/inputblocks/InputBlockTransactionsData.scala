package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Constants
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._
import spire.syntax.all.cfor

/**
  * Data carrier for input block transactions in P2P messaging.
  */
case class InputBlockTransactionsData(inputBlockId: ModifierId,
                                      transactions: Seq[ErgoTransaction],
                                      sizeOpt: Option[Int] = None)

object InputBlockTransactionsDataSerializer extends ErgoSerializer[InputBlockTransactionsData] {

  override def serialize(obj: InputBlockTransactionsData, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.inputBlockId))
    w.putUInt(obj.transactions.size.toLong)
    cfor(0)(_ < obj.transactions.length, _ + 1) { i =>
      ErgoTransactionSerializer.serialize(obj.transactions(i), w)
    }
  }

  override def parse(r: Reader): InputBlockTransactionsData = {
    //todo: consider max message size
    val startPos = r.position

    val headerId: ModifierId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val txCount = r.getUInt().toIntExact

    val txs = new Array[ErgoTransaction](txCount)
    cfor(0)(_ < txCount, _ + 1) { i =>
      txs(i) = ErgoTransactionSerializer.parse(r)
    }
    InputBlockTransactionsData(headerId, txs, Some(r.position - startPos))
  }

}
