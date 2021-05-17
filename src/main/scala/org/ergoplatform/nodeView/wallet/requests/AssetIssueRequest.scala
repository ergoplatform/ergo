package org.ergoplatform.nodeView.wallet.requests

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.ErgoAddress
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.settings.ErgoSettings
import scorex.core.transaction.box.Box.Amount
import sigmastate.SType
import sigmastate.Values.EvaluatedValue

/**
  * Request for new asset issuing.
  *
  * Ergo token data is stored in registers in the following way:
  * R2 - ID and supply amount
  * R4 - verbose name
  * R5 - description
  * R6 - number of decimal places
  *
  * additional registers (R7, R8, R9) could be used for asset-specific information
  */
case class AssetIssueRequest(addressOpt: Option[ErgoAddress],
                             valueOpt: Option[Long],
                             amount: Amount,
                             name: String,
                             description: String,
                             decimals: Int,
                             registers: Option[Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]])
  extends TransactionGenerationRequest

object AssetIssueRequest {

  def apply(address: ErgoAddress,
            valueOpt: Option[Long],
            amount: Amount,
            name: String,
            description: String,
            decimals: Int,
            registers: Option[Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]] = None): AssetIssueRequest =
    new AssetIssueRequest(Some(address), valueOpt, amount, name, description, decimals, registers)
}

class AssetIssueRequestEncoder(settings: ErgoSettings) extends Encoder[AssetIssueRequest] with ApiCodecs {

  implicit val addressEncoder: Encoder[ErgoAddress] = ErgoAddressJsonEncoder(settings).encoder

  def apply(request: AssetIssueRequest): Json = {
    Json.obj(
      "address" -> request.addressOpt.asJson,
      "ergValue" -> request.valueOpt.asJson,
      "amount" -> request.amount.asJson,
      "name" -> request.name.asJson,
      "description" -> request.description.asJson,
      "decimals" -> request.decimals.asJson,
      "registers" -> request.registers.asJson
    )
  }

}

class AssetIssueRequestDecoder(settings: ErgoSettings) extends Decoder[AssetIssueRequest] with ApiCodecs {

  val addressEncoders: ErgoAddressJsonEncoder = ErgoAddressJsonEncoder(settings)

  implicit def addressDecoder: Decoder[ErgoAddress] = addressEncoders.decoder

  def apply(cursor: HCursor): Decoder.Result[AssetIssueRequest] = {
    for {
      address <- cursor.downField("address").as[Option[ErgoAddress]]
      value <- cursor.downField("ergValue").as[Option[Long]]
      amount <- cursor.downField("amount").as[Amount]
      name <- cursor.downField("name").as[String]
      description <- cursor.downField("description").as[String]
      decimals <- cursor.downField("decimals").as[Int]
      registers <- cursor.downField("registers").as[Option[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]]
    } yield AssetIssueRequest(address, value, amount, name, description, decimals, registers)
  }

}
