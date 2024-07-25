package org.ergoplatform.http.api

import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoLikeContext
import org.ergoplatform.http.api.requests.{CryptoResult, ExecuteRequest, HintExtractionRequest}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.wallet.requests.{ExternalSecret, GenerateCommitmentsRequest, TransactionSigningRequest}
import org.ergoplatform.sdk.wallet.secrets.{DhtSecretKey, DlogSecretKey}
import org.ergoplatform.wallet.interpreter.TransactionHintsBag
import sigma.AnyValue
import sigmastate.Values.SigmaBoolean

/**
  * JSON codecs for HTTP API requests related entities
  */
trait ApiRequestsCodecs extends ApiCodecs {

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

  implicit val transactionSigningRequestEncoder: Encoder[TransactionSigningRequest] = { tsr =>
    Json.obj(
      "tx" -> tsr.unsignedTx.asJson,
      "secrets" -> Json.obj(
        "dlog" -> tsr.dlogs.asJson,
        "dht" -> tsr.dhts.asJson
      ),
      "hints" -> tsr.hints.asJson,
      "inputsRaw" -> tsr.inputs.asJson,
      "dataInputsRaw" -> tsr.dataInputs.asJson
    )
  }

  implicit val transactionSigningRequestDecoder: Decoder[TransactionSigningRequest] = { cursor =>
    for {
      tx <- cursor.downField("tx").as[UnsignedErgoTransaction]
      dlogs <- cursor.downField("secrets").downField("dlog").as[Option[Seq[DlogSecretKey]]]
      dhts <- cursor.downField("secrets").downField("dht").as[Option[Seq[DhtSecretKey]]]
      hints <- cursor.downField("hints").as[Option[TransactionHintsBag]]
      inputs <- cursor.downField("inputsRaw").as[Option[Seq[String]]]
      dataInputs <- cursor.downField("dataInputsRaw").as[Option[Seq[String]]]
      secrets = (dlogs.getOrElse(Seq.empty) ++ dhts.getOrElse(Seq.empty)).map(ExternalSecret.apply)
    } yield TransactionSigningRequest(tx, hints.getOrElse(TransactionHintsBag.empty), secrets, inputs, dataInputs)
  }

  implicit val generateCommitmentsRequestEncoder: Encoder[GenerateCommitmentsRequest] = { gcr =>
    Json.obj(
      "tx" -> gcr.unsignedTx.asJson,
      "secrets" -> Json.obj(
        "dlog" -> gcr.dlogs.asJson,
        "dht" -> gcr.dhts.asJson,
        "inputsRaw" -> gcr.inputs.asJson,
        "dataInputsRaw" -> gcr.dataInputs.asJson
      )
    )
  }

  implicit val generateCommitmentsRequestDecoder: Decoder[GenerateCommitmentsRequest] = { cursor =>
    for {
      tx <- cursor.downField("tx").as[UnsignedErgoTransaction]
      dlogs <- cursor.downField("secrets").downField("dlog").as[Option[Seq[DlogSecretKey]]]
      dhts <- cursor.downField("secrets").downField("dht").as[Option[Seq[DhtSecretKey]]]
      secrets = (dlogs.getOrElse(Seq.empty) ++ dhts.getOrElse(Seq.empty)).map(ExternalSecret.apply)
      secretsOpt = if (secrets.isEmpty) None else Some(secrets)
      inputs <- cursor.downField("inputsRaw").as[Option[Seq[String]]]
      dataInputs <- cursor.downField("dataInputsRaw").as[Option[Seq[String]]]
    } yield GenerateCommitmentsRequest(tx, secretsOpt, inputs, dataInputs)
  }

}
