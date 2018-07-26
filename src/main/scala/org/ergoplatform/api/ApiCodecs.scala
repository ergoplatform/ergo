package org.ergoplatform.api

import cats.syntax.either._
import io.circe._
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, TokenId}
import org.ergoplatform.api.ApiEncoderOption.Detalization
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.settings.Algos
import scorex.core.ModifierId
import scorex.core.validation.ValidationResult
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import sigmastate.Values.{EvaluatedValue, Value}
import sigmastate.serialization.ValueSerializer
import sigmastate.{SBoolean, SType}

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

  implicit val bytesEncoder: Encoder[Array[Byte]] =  Algos.encode(_).asJson

  implicit val bytesDecoder: Decoder[Array[Byte]] = bytesDecoder(x => x)

  implicit val bytesWrapperEncoder: Encoder[ByteArrayWrapper] =  _.data.asJson

  implicit val bytesWrapperDecoder: Decoder[ByteArrayWrapper] = bytesDecoder.map(bs => ByteArrayWrapper.apply(bs))

  implicit val byteSeqEncoder: Encoder[IndexedSeq[Byte]] = { in =>
    Algos.encode(in.toArray).asJson
  }

  implicit val modifierIdEncoder: Encoder[ModifierId] = _.array.asJson

  implicit val modifierIdDecoder: Decoder[ModifierId] = bytesDecoder(ModifierId @@ _)

  implicit val tokenIdEncoder: Encoder[TokenId] = _.array.asJson

  implicit val tokenIdDecoder: Decoder[TokenId] = bytesDecoder(Digest32 @@ _)

  implicit val adKeyEncoder: Encoder[ADKey] = _.array.asJson

  implicit val adKeyDecoder: Decoder[ADKey] = bytesDecoder(ADKey @@  _)

  def bytesDecoder[T](transform: Array[Byte] => T): Decoder[T] = { implicit cursor =>
    for {
      str <- cursor.as[String]
      bytes <- fromTry(Algos.decode(str))
    } yield transform(bytes)
  }

  implicit val valueEncoder: Encoder[Value[SType]] = { value =>
    ValueSerializer.serialize(value).asJson
  }

  implicit val booleanValueEncoder: Encoder[Value[SBoolean.type]] = { value =>
    valueEncoder(value)
  }

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

 implicit val registersEncoder: Encoder[Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]] = {
    _.map { case (key, value) =>
      s"R${key.number}" -> valueEncoder(value)
    }.asJson
  }

  implicit val assetEncoder: Encoder[Tuple2[ErgoBox.TokenId, Long]] = { asset =>
    Json.obj(
      "tokenId" -> asset._1.asJson,
      "amount" -> asset._2.asJson
    )
  }

  implicit val boxEncoder: Encoder[ErgoBox] = { box =>
    Json.obj(
      "boxId" -> box.id.asJson,
      "value" -> box.value.asJson,
      "proposition" -> valueEncoder(box.proposition),
      "assets" -> box.additionalTokens.asJson,
      "additionalRegisters" -> registersEncoder(box.additionalRegisters)
    )
  }

  implicit val balancesSnapshotEncoder: Encoder[BalancesSnapshot] = { v =>
    import v._
    Json.obj(
      "height" -> height.asJson,
      "balance" ->   balance.asJson,
      "assets" ->   assetBalances.toSeq.asJson
    )
  }

  implicit def unspentOffchainBoxEncoder(implicit opts: Detalization): Encoder[UnspentOffchainBox] = { b =>
    trackedBoxFields(b).asJson
  }

  implicit def uncertUnspentOffchainBoxEnc(implicit opts: Detalization): Encoder[UncertainUnspentOffchainBox] = { b =>
    trackedBoxFields(b).asJson
  }

  implicit def unspentOnchainBoxEncoder(implicit opts: Detalization): Encoder[UnspentOnchainBox] = { b =>
    unspentOnchainBoxFields(b).asJson
  }

  implicit def uncertUnspentOnchainBoxEnc(implicit opts: Detalization): Encoder[UncertainUnspentOnchainBox] = { b =>
    unspentOnchainBoxFields(b).asJson
  }

  implicit def spentOffchainBoxEncoder(implicit opts: Detalization): Encoder[SpentOffchainBox] = { b =>
    spentOffchainBoxFields(b).asJson
  }

  implicit def uncertainSpentOffchainBoxEnc(implicit opts: Detalization): Encoder[UncertainSpentOffchainBox] = { b =>
    spentOffchainBoxFields(b).asJson
  }

  implicit def spentOnchainBoxEncoder(implicit opts: Detalization): Encoder[SpentOnchainBox] = { b =>
    spentOnchainBoxFields(b).asJson
  }

  implicit def uncertainSpentOnchainBoxEncoder(implicit opts: Detalization): Encoder[UncertainSpentOnchainBox] = { b =>
    spentOnchainBoxFields(b).asJson
  }

  private def spentOnchainBoxFields(b: SpentOnchainTrackedBox)(implicit opts: Detalization): Map[String, Json] = {
    spentBoxFields(b) +
      ("creationHeight" -> b.creationHeight.asJson) +
      ("spendingHeight" -> b.spendingHeight.asJson)
  }

  private def spentOffchainBoxFields(b: SpentOffchainTrackedBox)(implicit opts: Detalization): Map[String, Json] = {
    spentBoxFields(b) + ("creationHeight" -> b.creationHeight.asJson)
  }

  private def spentBoxFields(b: SpentBox)(implicit opts: Detalization): Map[String, Json] = {
    import b._
    val txField = if (opts.showDetails) {
      "spendingTransaction" -> spendingTx.asJson
    } else {
      "spendingTransactionId" -> spendingTx.id.asJson
    }
    trackedBoxFields(b) + txField
  }

  private def unspentOnchainBoxFields(b: UnspentOnchainTrackedBox)(implicit opts: Detalization): Map[String, Json] = {
    trackedBoxFields(b) + ("creationHeight" -> b.creationHeight.asJson)
  }

  private def trackedBoxFields(b: TrackedBox)(implicit opts: Detalization): Map[String, Json] = {
    import b._
    val txField = if (opts.showDetails) {
      "creationTransaction" -> creationTx.asJson
    } else {
      "creationTransactionId" -> creationTx.id.asJson
    }
    Map(txField,
      "creationOutIndex" -> creationOutIndex.asJson,
      "box" -> box.asJson,
      "onchain" -> onchain.asJson
    )
  }

}

trait ApiEncoderOption

object ApiEncoderOption {

  case class Detalization(showDetails: Boolean) extends ApiEncoderOption {
    implicit def implicitValue: Detalization = this
  }

  object ShowDetails extends Detalization(true)
  object HideDetails extends Detalization(false)

}
