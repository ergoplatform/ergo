package org.ergoplatform.nodeView.wallet

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.wallet.Constants
import scorex.core.serialization.ScorexSerializer
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}

final case class WalletTransaction(tx: ErgoTransaction, inclusionHeight: Int, applicationIds: Seq[Short]) {

  def id: ModifierId = tx.id

}

object WalletTransactionSerializer extends ScorexSerializer[WalletTransaction] {

  override def serialize(wtx: WalletTransaction, w: Writer): Unit = {
    val txBytes = wtx.tx.bytes
    w.putInt(wtx.inclusionHeight)

    val appsCount = wtx.applicationIds.size.toShort
    if (appsCount == 1 && wtx.applicationIds.head == Constants.WalletAppId) {
      w.putShort(0)
    } else {
      w.putShort(appsCount)
      wtx.applicationIds.foreach { appId =>
        w.putShort(appId)
      }
    }
    w.putInt(txBytes.length)
    w.putBytes(txBytes)
  }

  override def parse(r: Reader): WalletTransaction = {
    val inclusionHeight = r.getInt()

    val appsCount = r.getShort()
    val appIds = if (appsCount == 0) {
      Seq(Constants.WalletAppId)
    } else {
      (0 until appsCount).map(_ => r.getShort())
    }

    val txBytesLen = r.getInt()
    val tx = ErgoTransactionSerializer.parseBytes(r.getBytes(txBytesLen))
    WalletTransaction(tx, inclusionHeight, appIds)
  }

}
