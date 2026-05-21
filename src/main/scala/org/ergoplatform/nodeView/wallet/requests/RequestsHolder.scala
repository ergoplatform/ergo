package org.ergoplatform.nodeView.wallet.requests

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, ErgoTreePredef, Pay2SAddress}
import sigma.interpreter.ContextExtension


case class RequestsHolder(requests: Seq[TransactionGenerationRequest],
                          feeOpt: Option[Long],
                          inputsRaw: Seq[String],
                          dataInputsRaw: Seq[String],
                          extensions: Seq[ContextExtension],
                          minerRewardDelay: Int)
                         (implicit val addressEncoder: ErgoAddressEncoder) {

  // Add separate payment request with fee.
  def withFee(): Seq[TransactionGenerationRequest] = {
    val address = Pay2SAddress(ErgoTreePredef.feeProposition(minerRewardDelay))
    val feeRequests = feeOpt
        .map(PaymentRequest(address, _, assets = Array.empty, registers = Map.empty))
        .toSeq
    requests ++ feeRequests
  }

}

class RequestsHolderEncoder(ergoSettings: ErgoSettings) extends Encoder[RequestsHolder] with ApiCodecs {

  implicit val transactionRequestEncoder: TransactionRequestEncoder = new TransactionRequestEncoder(ergoSettings)
  implicit val addressEncoder: Encoder[ErgoAddress] = ErgoAddressJsonEncoder(ergoSettings.chainSettings).encoder

  def apply(holder: RequestsHolder): Json = {
    val base = Json.obj(
      "requests" -> holder.requests.asJson,
      "fee" -> holder.feeOpt.asJson,
      "inputsRaw" -> holder.inputsRaw.asJson,
      "dataInputsRaw" -> holder.dataInputsRaw.asJson
    )
    if (holder.extensions.nonEmpty) {
      base.deepMerge(Json.obj(
        "context" -> Json.obj("extension" -> holder.extensions.asJson)
      ))
    } else {
      base
    }
  }

}

class RequestsHolderDecoder(settings: ErgoSettings) extends Decoder[RequestsHolder] with ApiCodecs {

  implicit val transactionRequestDecoder: TransactionRequestDecoder = new TransactionRequestDecoder(settings)
  implicit val addressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(settings.chainSettings.addressPrefix)

  private val minerRewardDelay: Int = settings.chainSettings.monetary.minerRewardDelay

  def apply(cursor: HCursor): Decoder.Result[RequestsHolder] = {
    for {
      requests <- cursor.downField("requests").as[Seq[TransactionGenerationRequest]]
      fee <- cursor.downField("fee").as[Option[Long]]
      inputs <- cursor.downField("inputsRaw").as[Option[Seq[String]]]
      dataInputs <- cursor.downField("dataInputsRaw").as[Option[Seq[String]]]
      extensions <- cursor.downField("context").downField("extension").as[Option[Seq[ContextExtension]]]
      inputsSeq = inputs.getOrElse(Seq.empty)
      extSeq = extensions.getOrElse(Seq.empty)
      _ <- if (extSeq.nonEmpty && extSeq.size != inputsSeq.size) {
        Left(DecodingFailure(
          s"context.extension length (${extSeq.size}) must match inputsRaw length (${inputsSeq.size})",
          cursor.history
        ))
      } else {
        Right(())
      }
    } yield RequestsHolder(requests, fee, inputsSeq, dataInputs.getOrElse(Seq.empty), extSeq, minerRewardDelay)
  }

}
