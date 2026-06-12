package org.ergoplatform.nodeView.wallet.requests

import io.circe.{CursorOp, Decoder, DecodingFailure, HCursor, Json, KeyDecoder}
import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.Constants
import scorex.util.encode.Base16
import sigmastate.eval.Extensions.ArrayByteOps

import scala.util.{Failure, Success}

private[requests] object WalletRequestCodecs {

  private val TokenIdSize = Constants.ModifierIdSize

  implicit val tokenIdKeyDecoder: KeyDecoder[ErgoBox.TokenId] =
    KeyDecoder.instance { value =>
      Base16.decode(value).toOption
        .filter(_.lengthCompare(TokenIdSize) == 0)
        .map(_.toTokenId)
    }

  def decodeTokenAmounts(cursor: HCursor, field: String): Decoder.Result[Array[(ErgoBox.TokenId, Long)]] = {
    val fieldCursor = cursor.downField(field)

    fieldCursor.as[Option[Seq[Json]]].flatMap { valuesOpt =>
      valuesOpt.getOrElse(Seq.empty)
        .foldLeft(Right(Vector.empty): Decoder.Result[Vector[(ErgoBox.TokenId, Long)]]) {
          case (acc, tokenJson) =>
            val tokenCursor = tokenJson.hcursor
            for {
              values <- acc
              tokenId <- tokenCursor.downField("tokenId").as[String]
              amount <- tokenCursor.downField("amount").as[Long]
              decodedTokenId <- decodeTokenId(tokenId, tokenCursor.downField("tokenId").history)
            } yield values :+ (decodedTokenId -> amount)
        }
        .map(_.toArray)
      }
  }

  private def decodeTokenId(value: String, history: List[CursorOp]): Decoder.Result[ErgoBox.TokenId] = {
    Base16.decode(value) match {
      case Success(bytes) if bytes.lengthCompare(TokenIdSize) == 0 =>
        Right(bytes.toTokenId)
      case Success(_) =>
        Left(DecodingFailure(s"Token id should be $TokenIdSize bytes", history))
      case Failure(e) =>
        Left(DecodingFailure.fromThrowable(e, history))
    }
  }
}
