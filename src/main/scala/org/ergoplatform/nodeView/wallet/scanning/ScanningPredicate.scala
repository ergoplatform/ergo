package org.ergoplatform.nodeView.wallet.scanning

import io.circe.{HCursor, Json}
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import org.ergoplatform.api.ApiCodecs
import scorex.crypto.hash.Digest32
import sigmastate.Values
import scorex.util.encode.Base16

/**
  * Basic interface for box scanning functionality
  */
sealed trait ScanningPredicate {
  def filter(box: ErgoBox): Boolean
}

case class ContainsScanningPredicate(regId: ErgoBox.RegisterId, bytes: Array[Byte]) extends ScanningPredicate {

  override def filter(box: ErgoBox): Boolean = {
    box.get(regId).exists {
      _ match {
        case Values.ByteArrayConstant(arr) => arr.toArray.containsSlice(bytes)
        case _ => false
      }
    }
  }
}

case class EqualsScanningPredicate(regId: ErgoBox.RegisterId, bytes: Array[Byte]) extends ScanningPredicate {
  override def filter(box: ErgoBox): Boolean = {
    box.get(regId).exists {
      _ match {
        case Values.ByteArrayConstant(arr) => arr.toArray.sameElements(bytes)
        case _ => false
      }
    }
  }
}

case class ContainsAssetPredicate(assetId: ErgoBox.TokenId) extends ScanningPredicate {
  override def filter(box: ErgoBox): Boolean = {
    box.additionalTokens.exists(_._1.sameElements(assetId))
  }
}

case class AndScanningPredicate(subPredicates: ScanningPredicate*) extends ScanningPredicate {
  override def filter(box: ErgoBox): Boolean = subPredicates.forall(p => p.filter(box))
}

case class OrScanningPredicate(subPredicates: ScanningPredicate*) extends ScanningPredicate {
  override def filter(box: ErgoBox): Boolean = subPredicates.exists(p => p.filter(box))
}

import io.circe.{ Decoder, Encoder }, io.circe.generic.auto._
import io.circe.syntax._

object ScanningPredicateJsonEncoders extends App with ApiCodecs {

  implicit val encodeContainsAsset: Encoder[ContainsAssetPredicate] =
    Encoder.forProduct2("predicate", "asset")(c => ("containsAsset", Base16.encode(c.assetId)))


  implicit val encodeScanningPredicate: Encoder[ScanningPredicate] = {predicate =>
    predicate match {
      case cap: ContainsAssetPredicate => Json.obj("predicate" -> "containsAsset".asJson, "asset" -> Base16.encode(cap.assetId).asJson)
      case or: OrScanningPredicate => Json.obj("predicate" -> "or".asJson, "args" -> or.subPredicates.asJson)
      case _ => ???
    }
  }

  implicit val scanningPredicateDecode: Decoder[ScanningPredicate] = { implicit cursor =>

      cursor.downField("predicate").as[String].flatMap { predicate =>
        predicate match {
          case _: String if predicate == "containsAsset" =>
            for {
              asset <- cursor.downField("asset").as[ErgoBox.TokenId]
            } yield ContainsAssetPredicate(asset)
          case _: String if predicate == "contains" =>
            for {
              bytes <- cursor.downField("bytes").as[Array[Byte]]
              register <- cursor.downField("register").as[Option[RegisterId]]
            } yield ContainsScanningPredicate(register.getOrElse(ErgoBox.R1), bytes)
          case _: String if predicate == "equals" =>
            for {
              bytes <- cursor.downField("bytes").as[Array[Byte]]
              register <- cursor.downField("register").as[Option[RegisterId]]
            } yield EqualsScanningPredicate(register.getOrElse(ErgoBox.R1), bytes)
          case _: String if predicate == "and" =>
            for {
              args <- cursor.downField("args").as[Seq[ScanningPredicate]]
            } yield AndScanningPredicate(args :_*)
          case _: String if predicate == "or" =>
            for {
              args <- cursor.downField("args").as[Seq[ScanningPredicate]]
            } yield OrScanningPredicate(args :_*)
        }
      }

  }

  val cap = OrScanningPredicate(
    ContainsAssetPredicate(Digest32 @@ Array.fill(32)(0: Byte)),
    ContainsAssetPredicate(Digest32 @@ Array.fill(32)(0: Byte))
  )

  val j = encodeScanningPredicate(cap)
  println(j)
  println(scanningPredicateDecode.decodeJson(j))
}