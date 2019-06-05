package org.ergoplatform.api

import cats.syntax.either._
import io.circe._
import io.circe.syntax._
import org.ergoplatform.{ErgoBox}
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, TokenId}
import org.ergoplatform.api.ApiEncoderOption.Detalization
import org.ergoplatform.mining.{groupElemToBytes, groupElemFromBytes}
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.nodeView.wallet.IdUtils.EncodedTokenId
import org.ergoplatform.nodeView.wallet.persistence.RegistryIndex
import org.ergoplatform.settings.Algos
import org.ergoplatform.wallet.boxes.TrackedBox
import scorex.core.validation.ValidationResult
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import sigmastate.Values.{Value, ErgoTree, EvaluatedValue}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.serialization.{ValueSerializer, ErgoTreeSerializer}
import sigmastate.{SBoolean, SType}
import special.collection.Coll
import sigmastate.eval._

import scala.util.Try

trait ApiCodecs {

  def fromTry[T](tryResult: Try[T])(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    tryResult.fold(e => Left(DecodingFailure(e.toString, cursor.history)), Right.apply)
  }

  def fromOption[T](maybeResult: Option[T])(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    maybeResult.fold[Either[DecodingFailure, T]](Left(DecodingFailure("No value found", cursor.history)))(Right.apply)
  }

  def fromThrows[T](throwsBlock: => T)(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    Either.catchNonFatal(throwsBlock).leftMap(e => DecodingFailure(e.toString, cursor.history))
  }

  def fromValidation[T](validationResult: ValidationResult[T])
                       (implicit cursor: ACursor): Either[DecodingFailure, T] = {
    fromTry(validationResult.toTry)
  }

  implicit val bigIntEncoder: Encoder[BigInt] = { bigInt =>
    JsonNumber.fromDecimalStringUnsafe(bigInt.toString).asJson
  }

  implicit val difficultyEncoder: Encoder[Difficulty] = bigIntEncoder

  implicit val bytesEncoder: Encoder[Array[Byte]] = Algos.encode(_).asJson

  implicit val bytesDecoder: Decoder[Array[Byte]] = bytesDecoder(x => x)

  implicit val ecPointDecoder: Decoder[EcPointType] = { implicit cursor =>
    for {
      str <- cursor.as[String]
      bytes <- fromTry(Algos.decode(str))
    } yield groupElemFromBytes(bytes)
  }

  implicit val ecPointEncoder: Encoder[EcPointType] = { point:EcPointType =>
      groupElemToBytes(point).asJson
  }

  implicit val byteSeqEncoder: Encoder[IndexedSeq[Byte]] = { in =>
    Algos.encode(in.toArray).asJson
  }

  implicit val modifierIdEncoder: Encoder[ModifierId] = Algos.encode(_).asJson

  implicit val modifierIdDecoder: Decoder[ModifierId] = _.as[String].map(ModifierId @@ _)

  implicit val digest32Encoder: Encoder[Digest32] = _.array.asJson

  implicit val digest32Decoder: Decoder[Digest32] = bytesDecoder(Digest32 @@ _)

  implicit val adKeyEncoder: Encoder[ADKey] = _.array.asJson

  implicit val adKeyDecoder: Decoder[ADKey] = bytesDecoder(ADKey @@ _)

  implicit val adDigestDecoder: Decoder[ADDigest] = bytesDecoder(ADDigest @@ _)

  def bytesDecoder[T](transform: Array[Byte] => T): Decoder[T] = { implicit cursor =>
    for {
      str <- cursor.as[String]
      bytes <- fromTry(Algos.decode(str))
    } yield transform(bytes)
  }

  implicit val ergoTreeEncoder: Encoder[ErgoTree] = { value =>
    ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(value).asJson
  }

  implicit val ergoTreeDecoder: Decoder[ErgoTree] = {
    decodeErgoTree(_.asInstanceOf[ErgoTree])
  }

  implicit val evaluatedValueEncoder: Encoder[EvaluatedValue[SType]] = { value =>
    ValueSerializer.serialize(value).asJson
  }

  implicit val evaluatedValueDecoder: Decoder[EvaluatedValue[SType]] = {
    decodeEvaluatedValue(_.asInstanceOf[EvaluatedValue[SType]])
  }

  def decodeEvaluatedValue[T](transform: EvaluatedValue[SType] => T): Decoder[T] = { implicit cursor: ACursor =>
    cursor.as[Array[Byte]] flatMap { bytes =>
      fromThrows(transform(ValueSerializer.deserialize(bytes).asInstanceOf[EvaluatedValue[SType]]))
    }
  }

  def decodeErgoTree[T](transform: ErgoTree => T): Decoder[T] = { implicit cursor: ACursor =>
    cursor.as[Array[Byte]] flatMap { bytes =>
      fromThrows(transform(ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)))
    }
  }

  implicit val registerIdEncoder: KeyEncoder[NonMandatoryRegisterId] = { regId =>
    s"R${regId.number}"
  }

  implicit val registerIdDecoder: KeyDecoder[NonMandatoryRegisterId] = { key =>
    ErgoBox.registerByName.get(key).collect {
      case nonMandatoryId: NonMandatoryRegisterId => nonMandatoryId
    }
  }

  implicit val proveDlogEncoder: Encoder[ProveDlog] = _.pkBytes.asJson

  def decodeRegisterId(key: String)(implicit cursor: ACursor): Decoder.Result[NonMandatoryRegisterId] = {
    registerIdDecoder
      .apply(key.toUpperCase)
      .toRight(DecodingFailure(s"Unknown register identifier: $key", cursor.history))
  }

  implicit val registersEncoder: Encoder[Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]] = {
    _.map { case (key, value) =>
      registerIdEncoder(key) -> evaluatedValueEncoder(value)
    }.asJson
  }

  implicit def assetEncoder[Id: Encoder]: Encoder[(Id, Long)] = { asset =>
    Json.obj(
      "tokenId" -> asset._1.asJson,
      "amount" -> asset._2.asJson
    )
  }

  implicit val boxEncoder: Encoder[ErgoBox] = { box =>
    Json.obj(
      "boxId" -> box.id.asJson,
      "value" -> box.value.asJson,
      "ergoTree" -> ergoTreeEncoder(box.ergoTree),
      "assets" -> box.additionalTokens.toArray.toSeq.asJson,
      "creationHeight" -> box.creationHeight.asJson,
      "additionalRegisters" -> registersEncoder(box.additionalRegisters)
    )
  }

  implicit val encodedTokenIdEncoder: Encoder[EncodedTokenId] = _.asJson

  implicit val balancesSnapshotEncoder: Encoder[RegistryIndex] = { v =>
    import v._
    Json.obj(
      "height" -> height.asJson,
      "balance" -> balance.asJson,
      "assets" -> assetBalances.map(x => (x._1: String, x._2)).asJson
    )
  }

  implicit def trackedBoxEncoder(implicit opts: Detalization): Encoder[TrackedBox] = { b =>
    val plainFields = Map(
      "spent" -> b.spendingStatus.spent.asJson,
      "onchain" -> b.chainStatus.onChain.asJson,
      "certain" -> b.certainty.certain.asJson,
      "creationOutIndex" -> b.creationOutIndex.asJson,
      "inclusionHeight" -> b.inclusionHeightOpt.asJson,
      "spendingHeight" -> b.spendingHeightOpt.asJson,
      "box" -> b.box.asJson
    )
    val fieldsWithTx = if (opts.showDetails) {
      plainFields +
        ("creationTransaction" -> b.creationTxId.asJson) +
        ("spendingTransaction" -> b.spendingTxIdOpt.asJson)
    } else {
      plainFields +
        ("creationTransactionId" -> b.creationTxId.asJson) +
        ("spendingTransactionId" -> b.spendingTxIdOpt.asJson)
    }
    fieldsWithTx.asJson
  }
}

trait ApiEncoderOption

object ApiEncoderOption {

  abstract class Detalization(val showDetails: Boolean) extends ApiEncoderOption {
    implicit def implicitValue: Detalization = this
  }

  case object ShowDetails extends Detalization(true)

  case object HideDetails extends Detalization(false)

}
