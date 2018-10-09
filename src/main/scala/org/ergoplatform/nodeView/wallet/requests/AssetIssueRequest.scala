package org.ergoplatform.nodeView.wallet.requests

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.nodeView.wallet.{ErgoAddress, ErgoAddressEncoder}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.transaction.box.Box.Amount

/**
  * Ergo Token Standard 1
  * R3 - Monetary value of asset
  */
case class AssetIssueRequest(address: ErgoAddress,
                             assetId: TokenId,
                             amount: Amount,
                             assetName: String,
                             decimals: Int) extends TransactionRequest {

  override def toBoxCandidate: ErgoBoxCandidate =
    new ErgoBoxCandidate(0L, address.script, Seq(assetId -> amount), Map.empty)
}

class AssetIssueRequestEncoder(settings: ErgoSettings) extends Encoder[AssetIssueRequest] {

  implicit val addressEncoder: Encoder[ErgoAddress] = new ErgoAddressEncoder(settings).encoder

  def apply(request: AssetIssueRequest): Json = Json.obj(
      "address" -> request.address.asJson,
      "assetId" -> request.assetId.asJson,
      "amount" -> request.amount.asJson,
      "assetName" -> request.assetName.asJson,
      "decimals" -> request.decimals.asJson
    )
}

class AssetIssueRequestDecoder(settings: ErgoSettings) extends Decoder[AssetIssueRequest] {

  val addressEncoders: ErgoAddressEncoder = new ErgoAddressEncoder(settings)

  implicit def addressDecoder: Decoder[ErgoAddress] = addressEncoders.decoder

  def apply(cursor: HCursor): Decoder.Result[AssetIssueRequest] = {
    for {
      address <- cursor.downField("address").as[ErgoAddress]
      assetId <- cursor.downField("assetId").as[TokenId]
      amount <- cursor.downField("amount").as[Amount]
      assetName <- cursor.downField("assetName").as[String] // String?
      decimals <- cursor.downField("decimals").as[Int]
    } yield AssetIssueRequest(address, assetId, amount, assetName, decimals)
  }
}
