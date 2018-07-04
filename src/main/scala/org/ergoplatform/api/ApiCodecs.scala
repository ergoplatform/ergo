package org.ergoplatform.api

import cats.syntax.either._
import io.circe._
import io.circe.syntax._
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.ApiSettings.EstimateByteLength
import org.ergoplatform.settings.{Algos, ApiSettings, NodeConfigurationSettings}
import scorex.core.ModifierId
import scorex.core.utils.{ScorexEncoding, ScorexLogging}
import scorex.core.validation.ValidationResult
import scorex.crypto.authds.ADKey
import sigmastate.Values.{EvaluatedValue, Value}
import sigmastate.serialization.ValueSerializer
import sigmastate.{SBoolean, SType}

import scala.util.Try

trait ApiCodecs extends ScorexEncoding with ScorexLogging {

  def settings: ApiSettings

  def shouldEstimateLength: Boolean = settings(EstimateByteLength)

  def fromTry[T](tryResult: Try[T])(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    tryResult.fold(e => Left(DecodingFailure(e.toString, cursor.history)), Right.apply)
  }

  def fromOption[T](maybeResult: Option[T])(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    maybeResult.fold[Either[DecodingFailure, T]](Left(DecodingFailure("No value found", cursor.history)))(Right.apply)
  }

  def fromThrows[T](throwsBlock: => T)(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    Either.catchNonFatal(throwsBlock).leftMap(e => DecodingFailure(e.toString, cursor.history))
  }

  def fromValidation[T](value: T)(validationResult: ValidationResult)
                       (implicit cursor: ACursor): Either[DecodingFailure, T] = {
    fromTry(validationResult.toTry.map(_ => value))
  }

  implicit val bigIntEncoder: Encoder[BigInt] = { bigInt =>
    JsonNumber.fromDecimalStringUnsafe(bigInt.toString).asJson
  }

  implicit val difficultyEncoder: Encoder[Difficulty] = bigIntEncoder

  implicit val bytesEncoder: Encoder[Array[Byte]] =  Algos.encode(_).asJson

  implicit val bytesDecoder: Decoder[Array[Byte]] = bytesDecoder(x => x)

  implicit val byteSeqEncoder: Encoder[IndexedSeq[Byte]] = { in =>
    Algos.encode(in.toArray).asJson
  }

  implicit val modifierIdEncoder: Encoder[ModifierId] = _.array.asJson

  implicit val modifierIdDecoder: Decoder[ModifierId] = bytesDecoder(ModifierId @@ _)

  implicit val adKeyEncoder: Encoder[ADKey] = _.array.asJson

  implicit val adKeyDecoder: Decoder[ADKey] = bytesDecoder(ADKey @@  _)

  def bytesDecoder[T](transform: Array[Byte] => T): Decoder[T] = { implicit cursor =>
    for {
      str <- cursor.as[String]
      bytes <- fromTry(Algos.decode(str))
    } yield transform(bytes)
  }

  def serialize(value: Value[SType]): Array[Byte] = ValueSerializer.serialize(value)

  implicit val valueEncoder: Encoder[Value[SType]] =  serialize(_).asJson

  implicit val booleanValueEncoder: Encoder[Value[SBoolean.type]] =  valueEncoder(_)

  implicit val booleanValueDecoder: Decoder[Value[SBoolean.type]] = {
    valueDecoder(_.asInstanceOf[Value[SBoolean.type]])
  }

  implicit val evaluatedValueDecoder: Decoder[EvaluatedValue[SType]] = {
    valueDecoder(_.asInstanceOf[EvaluatedValue[SType]])
  }

  def valueDecoder[T](transform: Value[SType] => T): Decoder[T]  = { implicit cursor: ACursor =>
    cursor.as[Array[Byte]] flatMap { bytes =>
      fromThrows(transform(ValueSerializer.deserialize(bytes)))
    }
  }

  def byteLengthField: String = "estimatedByteLength"

  def extractByteLength(json: Json): Int = {
    def reportError(e: DecodingFailure): Int = {
      logger.debug(s"Failed to extract $byteLengthField: ${e.message}")
      0
    }
    json.hcursor.downField(byteLengthField).as[Int].fold(reportError, l => l)
  }

  def jsonWithLength(estimatedByteLength: => Int, fields: (String, Json)*): Json = {
    val objFields = if (shouldEstimateLength) (byteLengthField -> estimatedByteLength.asJson) +: fields else fields
    Json.obj(objFields: _*)
  }

  def jsonWithLength(estimatedByteLength: => Int, fields: Map[String, Json]): Json = {
    val objMap = if (shouldEstimateLength) fields + (byteLengthField -> estimatedByteLength.asJson) else fields
    objMap.asJson
  }

}
