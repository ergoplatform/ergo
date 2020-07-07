package org.ergoplatform.nodeView.wallet.scanning

import io.circe.Json
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import scorex.util.encode.Base16
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.syntax._
import org.ergoplatform.http.api.ApiCodecs
import sigmastate.SType
import sigmastate.Values.EvaluatedValue


object ScanningPredicateJsonCodecs extends ApiCodecs {

  implicit val scanningPredicateEncoder: Encoder[ScanningPredicate] = {
    case cp: ContainsScanningPredicate =>
      Json.obj("predicate" -> "contains".asJson, "register" -> cp.regId.asJson, "value" -> cp.value.asJson(evaluatedValueEncoder))
    case ep: EqualsScanningPredicate =>
      Json.obj("predicate" -> "equals".asJson, "register" -> ep.regId.asJson, "value" -> ep.value.asJson)
    case cap: ContainsAssetPredicate =>
      Json.obj("predicate" -> "containsAsset".asJson, "assetId" -> Base16.encode(cap.assetId).asJson)
    case and: AndScanningPredicate =>
      Json.obj("predicate" -> "and".asJson, "args" -> and.subPredicates.asJson)
    case or: OrScanningPredicate =>
      Json.obj("predicate" -> "or".asJson, "args" -> or.subPredicates.asJson)
  }

  implicit val scanningPredicateDecoder: Decoder[ScanningPredicate] = { implicit cursor =>
    cursor.downField("predicate").as[String].flatMap {
      case predicate@(_: String) if predicate == "containsAsset" =>
        for {
          asset <- cursor.downField("assetId").as[ErgoBox.TokenId]
        } yield ContainsAssetPredicate(asset)
      case predicate@(_: String) if predicate == "contains" =>
        for {
          value <- cursor.downField("value").as[EvaluatedValue[_ <: SType]]
          register <- cursor.downField("register").as[Option[RegisterId]]
        } yield ContainsScanningPredicate(register.getOrElse(ErgoBox.R1), value)
      case predicate@(_: String) if predicate == "equals" =>
        for {
          value <- cursor.downField("value").as[EvaluatedValue[_ <: SType]]
          register <- cursor.downField("register").as[Option[RegisterId]]
        } yield EqualsScanningPredicate(register.getOrElse(ErgoBox.R1), value)
      case predicate@(_: String) if predicate == "and" =>
        for {
          args <- cursor.downField("args").as[Seq[ScanningPredicate]]
        } yield AndScanningPredicate(args: _*)
      case predicate@(_: String) if predicate == "or" =>
        for {
          args <- cursor.downField("args").as[Seq[ScanningPredicate]]
        } yield OrScanningPredicate(args: _*)
    }
  }

}
