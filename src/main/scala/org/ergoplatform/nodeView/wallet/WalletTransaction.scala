package org.ergoplatform.nodeView.wallet

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import scorex.core.serialization.ScorexSerializer
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}

final case class WalletTransaction(tx: ErgoTransaction, inclusionHeight: Int) {

  def id: ModifierId = tx.id

}

object WalletTransactionSerializer extends ScorexSerializer[WalletTransaction] {

  override def serialize(wtx: WalletTransaction, w: Writer): Unit = {
    val txBytes = wtx.tx.bytes
    w.putInt(wtx.inclusionHeight)
    w.putInt(txBytes.length)
    w.putBytes(txBytes)
  }

  override def parse(r: Reader): WalletTransaction = {
    val inclusionHeight = r.getInt()
    val txBytesLen = r.getInt()
    val tx = ErgoTransactionSerializer.parseBytes(r.getBytes(txBytesLen))
    WalletTransaction(tx, inclusionHeight)
  }

}
