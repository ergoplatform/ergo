package org.ergoplatform.http.api

import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoLikeContext
import org.ergoplatform.http.api.requests.{CryptoResult, ExecuteRequest, HintExtractionRequest}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import sigma.AnyValue
import sigmastate.Values.SigmaBoolean

trait ApiRequestsCodes extends ApiCodecs {
  implicit val hintExtractionRequestEncoder: Encoder[HintExtractionRequest] = { hr =>
    Map(
      "tx" -> hr.tx.asJson,
      "real" -> hr.real.asJson,
      "simulated" -> hr.simulated.asJson,
      "inputsRaw" -> hr.inputs.asJson,
      "dataInputsRaw" -> hr.dataInputs.asJson
    ).asJson
  }

  implicit val hintExtractionRequestDecoder: Decoder[HintExtractionRequest] = { cursor =>
    for {
      tx <- cursor.downField("tx").as[ErgoTransaction]
      real <- cursor.downField("real").as[Seq[SigmaBoolean]]
      simulated <- cursor.downField("simulated").as[Seq[SigmaBoolean]]
      inputs <- cursor.downField("inputsRaw").as[Option[Seq[String]]]
      dataInputs <- cursor.downField("dataInputsRaw").as[Option[Seq[String]]]
    } yield HintExtractionRequest(tx, real, simulated, inputs, dataInputs)
  }

  implicit val executeRequestDecoder = new Decoder[ExecuteRequest] {
    def apply(cursor: HCursor): Decoder.Result[ExecuteRequest] = {
      for {
        script <- cursor.downField("script").as[String]
        env <- cursor.downField("namedConstants").as[Map[String, AnyValue]]
        ctx <- cursor.downField("context").as[ErgoLikeContext]
      } yield ExecuteRequest(script, env.map({ case (k, v) => k -> v.value }), ctx)
    }
  }

  implicit val cryptResultEncoder: Encoder[CryptoResult] = {
    res =>
      val fields = Map(
        "value" -> res.value.asJson,
        "cost" -> res.cost.asJson
      )
      fields.asJson
  }
}
