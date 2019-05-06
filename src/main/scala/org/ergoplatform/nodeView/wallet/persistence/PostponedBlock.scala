package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.ErgoBoxSerializer
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.settings.Constants
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.authds.ADKey
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}

/**
  * Wallet-critic data extracted from full block to be processed later.
  * @param id - header id of postponed block
  * @param height - height of postponed block
  * @param inputs - all inputs from postponed block
  * @param outputs - outputs containing tracked addresses from postponed block
  */
final case class PostponedBlock(id: ModifierId,
                                height: Int,
                                inputs: Seq[(ModifierId, EncodedBoxId)],
                                outputs: Seq[(ModifierId, ErgoBox)])

object PostponedBlockSerializer extends ScorexSerializer[PostponedBlock] {

  override def serialize(obj: PostponedBlock, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.id))
    w.putInt(obj.height)
    w.putInt(obj.inputs.size)
    obj.inputs.foreach { case (txId, id) =>
      w.putBytes(idToBytes(txId) ++ decodedBoxId(id))
    }
    w.putInt(obj.outputs.size)
    obj.outputs.foreach { case (txId, box) =>
      w.putBytes(idToBytes(txId))
      w.putInt(box.bytes.length)
      w.putBytes(box.bytes)
    }
  }

  override def parse(r: Reader): PostponedBlock = {
    val id = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val height = r.getInt()
    val inputsQty = r.getInt()
    val inputs = (0 until inputsQty)
      .foldLeft(Seq.empty[(ModifierId, EncodedBoxId)]) { case (acc, _) =>
        val txId = bytesToId(r.getBytes(Constants.ModifierIdSize))
        val boxId = encodedBoxId(ADKey @@ r.getBytes(Constants.ModifierIdSize))
        acc :+ (txId, boxId)
      }
    val outputsQty = r.getInt()
    val outputs = (0 until outputsQty)
      .foldLeft(Seq.empty[(ModifierId, ErgoBox)]) { case (acc, _) =>
        val txId = bytesToId(r.getBytes(Constants.ModifierIdSize))
        val boxLen = r.getInt()
        val box = ErgoBoxSerializer.parseBytes(r.getBytes(boxLen))
        acc :+ (txId, box)
      }
    PostponedBlock(id, height, inputs, outputs)
  }

}
