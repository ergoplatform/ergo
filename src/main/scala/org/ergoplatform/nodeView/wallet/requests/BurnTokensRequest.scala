package org.ergoplatform.nodeView.wallet.requests

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.modifiers.mempool.ErgoTransaction._
import org.ergoplatform.ErgoBox

/**
  * Request for asset burning.
  *
  * @param assetsToBurn sequence of token id's and amount to burn
  *
  */
case class BurnTokensRequest(assetsToBurn: Array[(ErgoBox.TokenId, Long)])
  extends TransactionGenerationRequest

class BurnTokensRequestEncoder extends Encoder[BurnTokensRequest] {
  def apply(request: BurnTokensRequest): Json = {
    Json.obj(
      "assetsToBurn" -> request.assetsToBurn.asJson
    )
  }

}

class BurnTokensRequestDecoder extends Decoder[BurnTokensRequest] {
  def apply(cursor: HCursor): Decoder.Result[BurnTokensRequest] = {
    for {
      assetsToBurn <- cursor.downField("assetsToBurn").as[Option[Seq[(ErgoBox.TokenId, Long)]]]
    } yield BurnTokensRequest(assetsToBurn.toArray.flatten)
  }

}
