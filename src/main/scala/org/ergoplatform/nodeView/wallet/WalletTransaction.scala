package org.ergoplatform.nodeView.wallet

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.Constants.ScanId
import scorex.core.serialization.ScorexSerializer
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}

/**
  * Transaction stored in the wallet.
  * Includes transaction itself, as well as some metadata: inclusion height and scans the transaction
  * can be associated with.
  *
  * @param tx - transaction to be stored in the wallet
  * @param inclusionHeight - blockchain inclusion height for the transaction
  * @param scanIds - scans the transaction is associated with
  */
final case class WalletTransaction(tx: ErgoTransaction, inclusionHeight: Int, scanIds: Seq[ScanId]) {

  // Transaction identifier as Base16-encoded string
  def id: ModifierId = tx.id

  // Transaction identifier as a byte array
  def idBytes: Array[Byte] = tx.serializedId

}

object WalletTransactionSerializer extends ScorexSerializer[WalletTransaction] {

  override def serialize(wtx: WalletTransaction, w: Writer): Unit = {
    val txBytes = wtx.tx.bytes
    w.putInt(wtx.inclusionHeight)

    val scansCount = wtx.scanIds.size.toShort
    if (scansCount == 1 && wtx.scanIds.head == Constants.PaymentsScanId) {
      w.putShort(0)
    } else {
      w.putShort(scansCount)
      wtx.scanIds.foreach { scanId =>
        w.putShort(scanId)
      }
    }
    w.putInt(txBytes.length)
    w.putBytes(txBytes)
  }

  override def parse(r: Reader): WalletTransaction = {
    val inclusionHeight = r.getInt()

    val scansCount = r.getShort()
    val scanIds = if (scansCount == 0) {
      Seq(Constants.PaymentsScanId)
    } else {
      (0 until scansCount).map(_ => ScanId @@ r.getShort())
    }

    val txBytesLen = r.getInt()
    val tx = ErgoTransactionSerializer.parseBytes(r.getBytes(txBytesLen))
    WalletTransaction(tx, inclusionHeight, scanIds)
  }

}
