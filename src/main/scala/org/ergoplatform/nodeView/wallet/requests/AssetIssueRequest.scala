package org.ergoplatform.nodeView.wallet.requests

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.nodeView.wallet.{ErgoAddress, ErgoAddressEncoder}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import scorex.core.transaction.box.Box.Amount
import sigmastate.Values.{IntConstant, StringConstant}

/**
  * Request for issuing new asset of Ergo Asset Standard 1.
  *
  * EAS1 token data is stored in registers in the following way:
  * R3 - ID and supply amount
  * R4 - verbose name
  * R5 - description
  * R6 - number of decimal places
  */
case class AssetIssueRequest(address: ErgoAddress,
                             assetId: ErgoBox.TokenId,
                             amount: Amount,
                             name: String,
                             description: String,
                             decimals: Int) extends TransactionRequest {

  override def toBoxCandidate: ErgoBoxCandidate = {
    val nonMandatoryRegisters = Map(
      R4 -> StringConstant(name),
      R5 -> StringConstant(description),
      R6 -> IntConstant(decimals)
    )
    new ErgoBoxCandidate(0L, address.script, Seq(assetId -> amount), nonMandatoryRegisters)
  }
}

class AssetIssueRequestEncoder(settings: ErgoSettings) extends Encoder[AssetIssueRequest] with ApiCodecs {

  implicit val addressEncoder: Encoder[ErgoAddress] = new ErgoAddressEncoder(settings).encoder

  def apply(request: AssetIssueRequest): Json = Json.obj(
    "address" -> request.address.asJson,
    "assetId" -> request.assetId.asJson,
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
      assetId <- cursor.downField("assetId").as[ErgoBox.TokenId]
      amount <- cursor.downField("amount").as[Amount]
      name <- cursor.downField("name").as[String]
      description <- cursor.downField("description").as[String]
      decimals <- cursor.downField("decimals").as[Int]
    } yield AssetIssueRequest(address, assetId, amount, name, description, decimals)
  }
}
