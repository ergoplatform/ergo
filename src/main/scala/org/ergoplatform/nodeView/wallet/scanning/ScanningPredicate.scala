package org.ergoplatform.nodeView.wallet.scanning

import io.circe.{DecodingFailure, HCursor}
import org.ergoplatform.{ErgoAddress, ErgoBox}
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, RegisterId}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.settings.ErgoSettings
import scorex.crypto.hash.Digest32
import sigmastate.{SType, Values}
import scorex.util.encode.Base16
import sigmastate.Values.EvaluatedValue

import scala.util.{Failure, Success}

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

object GenericDerivation extends ApiCodecs {

  implicit val encodeContainsAsset: Encoder[ContainsAssetPredicate] =
    Encoder.forProduct2("predicate", "asset")(c => ("containsAsset", Base16.encode(c.assetId)))

  object ScanningPredicateDecoder extends Decoder[ScanningPredicate] {
    def apply(cursor: HCursor): Decoder.Result[ScanningPredicate] = {
      cursor.downField("predicate").as[String].flatMap { predicate =>
        predicate match {
          case _: String if predicate == "containsAsset" =>
            for {
              asset <- cursor.downField("asset").as[ErgoBox.TokenId]
              //  registers <- cursor.downField("registers").as[Option[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]]
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
            } yield AndScanningPredicate(args)
          case _: String if predicate == "or" =>
            for {
              args <- cursor.downField("args").as[Seq[ScanningPredicate]]
            } yield AndScanningPredicate(args)
        }
      }
    }
  }

}