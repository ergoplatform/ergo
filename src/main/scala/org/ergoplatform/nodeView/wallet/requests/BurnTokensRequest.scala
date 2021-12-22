package org.ergoplatform.nodeView.wallet.requests

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.modifiers.mempool.ErgoTransaction._
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoAddress, ErgoBox}


/**
  * Request for asset burning.
  *
  * @param assetsToBurn sequence of token id's and amount to burn
  *
  */
case class BurnTokensRequest(assetsToBurn: Seq[(ErgoBox.TokenId, Long)])
  extends TransactionGenerationRequest

class BurnTokensRequestEncoder(settings: ErgoSettings) extends Encoder[BurnTokensRequest] {

  implicit val addressEncoder: Encoder[ErgoAddress] = ErgoAddressJsonEncoder(settings).encoder

  def apply(request: BurnTokensRequest): Json = {
    Json.obj(
      "assetsToBurn" -> request.assetsToBurn.asJson,
    )
  }

}

class BurnTokensRequestDecoder(settings: ErgoSettings) extends Decoder[BurnTokensRequest] {

  val addressEncoders: ErgoAddressJsonEncoder = ErgoAddressJsonEncoder(settings)

  implicit def addressDecoder: Decoder[ErgoAddress] = addressEncoders.decoder

  def apply(cursor: HCursor): Decoder.Result[BurnTokensRequest] = {
    for {
      assetsToBurn <- cursor.downField("assetsToBurn").as[Option[Seq[(ErgoBox.TokenId, Long)]]]
    } yield BurnTokensRequest(assetsToBurn.toSeq.flatten)
  }

}
