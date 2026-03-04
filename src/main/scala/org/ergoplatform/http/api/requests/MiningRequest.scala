package org.ergoplatform.http.api.requests

import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax._
import io.circe.Json
import org.ergoplatform.modifiers.mempool.ErgoTransaction

/**
  * Represents a request to generate a candidate with the given transactions and miner public key.
  *
  * @param txs      Transactions to include in the block candidate
  * @param pk       String Hexadecimal representation of public key to use as minerPk
  */
case class MiningRequest(txs: Seq[ErgoTransaction], pk: String)

object MiningRequest {
  implicit val miningRequestEncoder: Encoder[MiningRequest] = { request =>
    Json.obj(
      "txs" -> request.txs.asJson,
      "pk" -> Json.fromString(request.pk)
    )
  }

  implicit val miningRequestDecoder: Decoder[MiningRequest] = { cursor =>
    for {
      txs <- cursor.downField("txs").as[Seq[ErgoTransaction]]
      pk <- cursor.downField("pk").as[String]
    } yield MiningRequest(txs, pk)
  }
}
