package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.settings.Constants
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

/**
  * Wallet-critical data extracted from full block to be processed later.
  * @param id - header id of postponed block
  * @param height - height of postponed block
  * @param txs - all wallet-related transactions from a postponed block
  */
final case class PostponedBlock(id: ModifierId,
                                height: Int,
                                txs: Seq[ErgoTransaction])

object PostponedBlockSerializer extends ScorexSerializer[PostponedBlock] {

  override def serialize(obj: PostponedBlock, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.id))
    w.putInt(obj.height)
    w.putInt(obj.txs.size)
    obj.txs.foreach { tx =>
      w.putInt(tx.bytes.length)
      w.putBytes(tx.bytes)
    }
  }

  override def parse(r: Reader): PostponedBlock = {
    val id = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val height = r.getInt()
    val txsQty = r.getInt()
    val txs = (0 until txsQty)
      .foldLeft(Seq.empty[ErgoTransaction]) { case (acc, _) =>
        val txLen = r.getInt()
        val tx = ErgoTransactionSerializer.parseBytes(r.getBytes(txLen))
        acc :+ tx
      }
    PostponedBlock(id, height, txs)
  }

}
