package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.NetworkObjectTypeId.Value
import org.ergoplatform.modifiers.{NetworkObjectTypeId, NonHeaderBlockSection, TransactionsCarryingBlockSection}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Constants
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._

case class InputBlockTransactionsData(inputBlockId: ModifierId,
                                      transactions: Seq[ErgoTransaction],
                                      override val sizeOpt: Option[Int] = None)
  extends NonHeaderBlockSection with TransactionsCarryingBlockSection {  // todo: inheritance needed ?

  override def headerId: ModifierId = inputBlockId

  override def digest: Digest32 = ??? // todo: include witnesses ?

  /**
    * Type of node view modifier (transaction, header etc)
    */
  override val modifierTypeId: Value = NetworkObjectTypeId.fromByte(40.toByte) // todo: check / improve
  override type M = InputBlockTransactionsData

  /**
    * Serializer which can convert self to bytes
    */
  override def serializer: ErgoSerializer[InputBlockTransactionsData] = InputBlockTransactionsDataSerializer

}

object InputBlockTransactionsDataSerializer extends ErgoSerializer[InputBlockTransactionsData] {

  override def serialize(obj: InputBlockTransactionsData, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.inputBlockId))
    w.putUInt(obj.transactions.size.toLong)
    obj.transactions.foreach { tx =>
      ErgoTransactionSerializer.serialize(tx, w)
    }
  }

  override def parse(r: Reader): InputBlockTransactionsData = {
    val startPos = r.position

    val headerId: ModifierId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val txCount = r.getUInt().toIntExact

    val txs = (1 to txCount).map { _ =>
      ErgoTransactionSerializer.parse(r)
    }
    InputBlockTransactionsData(headerId, txs, Some(r.position - startPos))
  }

}
