package org.ergoplatform.nodeView.wallet.requests

import io.circe.generic.decoding.DerivedDecoder.deriveDecoder
import io.circe.{Decoder, KeyDecoder}
import org.ergoplatform.ErgoBox
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16

/**
  * A request for boxes with given balance and assets
  */
case class BoxesRequest(targetBalance: Long, targetAssets: Map[ErgoBox.TokenId, Long])

object BoxesRequest {

  implicit val keyDecoder: KeyDecoder[ErgoBox.TokenId] =
    KeyDecoder.instance(s => Base16.decode(s).toOption.map(Digest32 @@ _))

  implicit val decoder: Decoder[BoxesRequest] =
    cursor =>
      for {
        targetBalance <- cursor.downField("targetBalance").as[Long]
        targetAssets <- cursor.downField("targetAssets").as[Map[ErgoBox.TokenId, Long]]
      } yield BoxesRequest(targetBalance, targetAssets)
}
