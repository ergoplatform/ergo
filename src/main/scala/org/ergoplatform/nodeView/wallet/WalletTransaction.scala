package org.ergoplatform.nodeView.wallet

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId}
import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import scorex.core.serialization.ScorexSerializer
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}
import sigmastate.SType
import sigmastate.Values.{ErgoTree, EvaluatedValue}
import sigmastate.eval.Extensions._
import sigmastate.eval._

final case class WalletTransaction(tx: ErgoTransaction, inclusionHeight: Int) {

  def id: ModifierId = tx.id

}

object WalletTransaction {

  import ErgoTransaction._

  def boxEncoder(e: ErgoAddressEncoder): Encoder[ErgoBox] = { box =>
    Json.obj(
      "boxId" -> box.id.asJson,
      "value" -> box.value.asJson,
      "ergoTree" -> ergoTreeEncoder(box.ergoTree),
      "address" -> e.fromProposition(box.ergoTree).toOption.map(_.toString).asJson,
      "assets" -> box.additionalTokens.toArray.toSeq.asJson,
      "creationHeight" -> box.creationHeight.asJson,
      "additionalRegisters" -> registersEncoder(box.additionalRegisters)
    )
  }

  def jsonEncoder(implicit e: ErgoAddressEncoder): Encoder[WalletTransaction] = { obj =>
    implicit val enc: Encoder[ErgoBox] = boxEncoder(e)
    Json.obj(
      "id" -> obj.tx.id.asJson,
      "inputs" -> obj.tx.inputs.asJson,
      "dataInputs" -> obj.tx.dataInputs.asJson,
      "outputs" -> obj.tx.outputs.asJson,
      "size" -> obj.tx.size.asJson,
      "inclusionHeight" -> obj.inclusionHeight.asJson
    )
  }

  implicit val jsonDecoder: Decoder[WalletTransaction] = { c =>
    for {
      inputs <- c.downField("inputs").as[IndexedSeq[Input]]
      dataInputs <- c.downField("dataInputs").as[IndexedSeq[DataInput]]
      outputsWithIndex <- c.downField("outputs").as[IndexedSeq[(ErgoBoxCandidate, Option[BoxId])]]
      inclusionHeight <- c.downField("inclusionHeight").as[Int]
    } yield WalletTransaction(new ErgoTransaction(inputs, dataInputs, outputsWithIndex.map(_._1)), inclusionHeight)
  }

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
