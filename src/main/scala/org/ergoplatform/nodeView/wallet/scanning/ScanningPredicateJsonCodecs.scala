package org.ergoplatform.nodeView.wallet.scanning

import io.circe.Json
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import org.ergoplatform.api.ApiCodecs
import scorex.util.encode.Base16
import io.circe.{Decoder, Encoder}, io.circe.generic.auto._
import io.circe.syntax._


object ScanningPredicateJsonCodecs extends ApiCodecs {

  implicit val scanningPredicateEncoder: Encoder[ScanningPredicate] = {
    case cp: ContainsScanningPredicate =>
      Json.obj("predicate" -> "contains".asJson, "register" -> cp.regId.asJson, "bytes" -> Base16.encode(cp.bytes).asJson)
    case ep: EqualsScanningPredicate =>
      Json.obj("predicate" -> "equals".asJson, "register" -> ep.regId.asJson, "bytes" -> Base16.encode(ep.bytes).asJson)
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
          bytes <- cursor.downField("bytes").as[Array[Byte]]
          register <- cursor.downField("register").as[Option[RegisterId]]
        } yield ContainsScanningPredicate(register.getOrElse(ErgoBox.R1), bytes)
      case predicate@(_: String) if predicate == "equals" =>
        for {
          bytes <- cursor.downField("bytes").as[Array[Byte]]
          register <- cursor.downField("register").as[Option[RegisterId]]
        } yield EqualsScanningPredicate(register.getOrElse(ErgoBox.R1), bytes)
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
