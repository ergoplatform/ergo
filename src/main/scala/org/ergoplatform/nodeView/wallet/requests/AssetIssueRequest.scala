package org.ergoplatform.nodeView.wallet.requests

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.nodeView.wallet.{ErgoAddress, ErgoAddressEncoder}
import org.ergoplatform.settings.ErgoSettings
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
case class AssetIssueRequest(address: ErgoAddress,
                             amount: Amount,
                             name: String,
                             description: String,
                             decimals: Int) extends TransactionRequest

class AssetIssueRequestEncoder(settings: ErgoSettings) extends Encoder[AssetIssueRequest] with ApiCodecs {

  implicit val addressEncoder: Encoder[ErgoAddress] = new ErgoAddressEncoder(settings).encoder

  def apply(request: AssetIssueRequest): Json = Json.obj(
    "address" -> request.address.asJson,
    "amount" -> request.amount.asJson,
    "name" -> request.name.asJson,
    "description" -> request.name.asJson,
    "decimals" -> request.decimals.asJson
  )
}

class AssetIssueRequestDecoder(settings: ErgoSettings) extends Decoder[AssetIssueRequest] with ApiCodecs {

  val addressEncoders: ErgoAddressEncoder = new ErgoAddressEncoder(settings)

  implicit def addressDecoder: Decoder[ErgoAddress] = addressEncoders.decoder

  def apply(cursor: HCursor): Decoder.Result[AssetIssueRequest] = {
    for {
      address <- cursor.downField("address").as[ErgoAddress]
      amount <- cursor.downField("amount").as[Amount]
      name <- cursor.downField("name").as[String]
      description <- cursor.downField("description").as[String]
      decimals <- cursor.downField("decimals").as[Int]
    } yield AssetIssueRequest(address, amount, name, description, decimals)
  }
}
