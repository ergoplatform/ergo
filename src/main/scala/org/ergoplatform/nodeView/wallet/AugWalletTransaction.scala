package org.ergoplatform.nodeView.wallet

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.ErgoTransaction

/**
  * A wallet transaction augmented with confirmations number.
  */
final case class AugWalletTransaction(wtx: WalletTransaction, numConfirmations: Int)

object AugWalletTransaction {

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

  def jsonEncoder(implicit e: ErgoAddressEncoder): Encoder[AugWalletTransaction] = { obj =>
    implicit val enc: Encoder[ErgoBox] = boxEncoder(e)
    Json.obj(
      "id" -> obj.wtx.tx.id.asJson,
      "inputs" -> obj.wtx.tx.inputs.asJson,
      "dataInputs" -> obj.wtx.tx.dataInputs.asJson,
      "outputs" -> obj.wtx.tx.outputs.asJson,
      "size" -> obj.wtx.tx.size.asJson,
      "inclusionHeight" -> obj.wtx.inclusionHeight.asJson,
      "numConfirmations" -> obj.numConfirmations.asJson
    )
  }

  implicit val jsonDecoder: Decoder[AugWalletTransaction] = { c =>
    for {
      inputs <- c.downField("inputs").as[IndexedSeq[Input]]
      dataInputs <- c.downField("dataInputs").as[IndexedSeq[DataInput]]
      outputsWithIndex <- c.downField("outputs").as[IndexedSeq[(ErgoBoxCandidate, Option[BoxId])]]
      inclusionHeight <- c.downField("inclusionHeight").as[Int]
      numConfirmations <- c.downField("numConfirmations").as[Int]
    } yield AugWalletTransaction(
      WalletTransaction(new ErgoTransaction(inputs, dataInputs, outputsWithIndex.map(_._1)), inclusionHeight),
      numConfirmations
    )
  }

}
