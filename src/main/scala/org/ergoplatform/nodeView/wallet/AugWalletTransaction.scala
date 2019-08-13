package org.ergoplatform.nodeView.wallet

import io.circe.syntax._
import io.circe._
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId}
import org.ergoplatform._
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import sigmastate.SType
import sigmastate.Values.EvaluatedValue

/**
  * A wallet transaction augmented with confirmations number.
  */
final case class AugWalletTransaction(wtx: WalletTransaction, numConfirmations: Int)

object AugWalletTransaction extends ApiCodecs {

  def boxEncoder(e: ErgoAddressEncoder): Encoder[ErgoBox] = { box =>
    Json.obj(
      "boxId" -> box.id.asJson,
      "value" -> box.value.asJson,
      "ergoTree" -> ergoTreeEncoder(box.ergoTree),
      "address" -> e.fromProposition(box.ergoTree).toOption.map(_.toString).asJson,
      "assets" -> box.additionalTokens.toArray.toSeq.asJson,
      "creationHeight" -> box.creationHeight.asJson,
      "additionalRegisters" -> box.additionalRegisters.asJson
    )
  }

  def jsonEncoder(implicit e: ErgoAddressEncoder): Encoder[AugWalletTransaction] = { obj =>
    implicit val enc: Encoder[ErgoBox] = boxEncoder(e)
    Json.obj(
      "id" -> obj.wtx.tx.id.asJson,
      "inputs" -> obj.wtx.tx.inputs.asJson,
      "dataInputs" -> obj.wtx.tx.dataInputs.asJson,
      "outputs" -> obj.wtx.tx.outputs.toSeq.asJson(Encoder.encodeSeq(enc)),
      "size" -> obj.wtx.tx.size.asJson,
      "inclusionHeight" -> obj.wtx.inclusionHeight.asJson,
      "applicationId" -> obj.wtx.applicationId.asJson,
      "numConfirmations" -> obj.numConfirmations.asJson
    )
  }

  implicit val jsonDecoder: Decoder[AugWalletTransaction] = { c =>
    for {
      ergoTx <- c.as[ErgoTransaction]
      inclusionHeight <- c.downField("inclusionHeight").as[Int]
      numConfirmations <- c.downField("numConfirmations").as[Int]
      appId <- c.downField("applicationId").as[Short]} yield AugWalletTransaction(WalletTransaction(ergoTx, inclusionHeight, appId), numConfirmations)
  }

}
