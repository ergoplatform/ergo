package org.ergoplatform.nodeView.wallet.requests

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoAddress, ErgoBox, ErgoBoxCandidate}
import scorex.core.transaction.box.Box.Amount

/**
  * Request for new asset issuing.
  *
  * Ergo token data is stored in registers in the following way:
  * R2 - ID and supply amount
  * R4 - verbose name
  * R5 - description
  * R6 - number of decimal places
  */
case class AssetIssueRequest(addressOpt: Option[ErgoAddress],
                             amount: Amount,
                             name: String,
                             description: String,
                             decimals: Int) extends TransactionRequest

class AssetIssueRequestEncoder(settings: ErgoSettings) extends Encoder[AssetIssueRequest] with ApiCodecs {

  implicit val addressEncoder: Encoder[ErgoAddress] = ErgoAddressJsonEncoder(settings).encoder

  def apply(request: AssetIssueRequest): Json = {
    Json.obj(
      "address" -> request.addressOpt.asJson,
      "amount" -> request.amount.asJson,
      "name" -> request.name.asJson,
      "description" -> request.description.asJson,
      "decimals" -> request.decimals.asJson
    )
  }

}

class AssetIssueRequestDecoder(settings: ErgoSettings) extends Decoder[AssetIssueRequest] with ApiCodecs {

  val addressEncoders: ErgoAddressJsonEncoder = ErgoAddressJsonEncoder(settings)

  implicit def addressDecoder: Decoder[ErgoAddress] = addressEncoders.decoder

  def apply(cursor: HCursor): Decoder.Result[AssetIssueRequest] = {
    for {
      address <- cursor.downField("address").as[Option[ErgoAddress]]
      amount <- cursor.downField("amount").as[Amount]
      name <- cursor.downField("name").as[String]
      description <- cursor.downField("description").as[String]
      decimals <- cursor.downField("decimals").as[Int]
    } yield AssetIssueRequest(address, amount, name, description, decimals)
  }

}
