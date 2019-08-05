package org.ergoplatform.api

import io.circe._
import io.circe.syntax._
import org.ergoplatform.JsonCodecs
import org.ergoplatform.api.ApiEncoderOption.Detalization
import org.ergoplatform.mining.{groupElemFromBytes, groupElemToBytes}
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.nodeView.wallet.IdUtils.EncodedTokenId
import org.ergoplatform.nodeView.wallet.persistence.RegistryIndex
import org.ergoplatform.settings.Algos
import org.ergoplatform.wallet.boxes.TrackedBox
import scorex.core.validation.ValidationResult
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.interpreter.CryptoConstants.EcPointType

trait ApiCodecs extends JsonCodecs {

  def fromValidation[T](validationResult: ValidationResult[T])
                       (implicit cursor: ACursor): Either[DecodingFailure, T] = {
    fromTry(validationResult.toTry)
  }

  implicit val bigIntEncoder: Encoder[BigInt] = { bigInt =>
    JsonNumber.fromDecimalStringUnsafe(bigInt.toString).asJson
  }

  implicit val difficultyEncoder: Encoder[Difficulty] = bigIntEncoder

  implicit val ecPointDecoder: Decoder[EcPointType] = { implicit cursor =>
    for {
      str <- cursor.as[String]
      bytes <- fromTry(Algos.decode(str))
    } yield groupElemFromBytes(bytes)
  }

  implicit val ecPointEncoder: Encoder[EcPointType] = { point:EcPointType =>
      groupElemToBytes(point).asJson
  }

  implicit val proveDlogEncoder: Encoder[ProveDlog] = _.pkBytes.asJson

  implicit val encodedTokenIdEncoder: Encoder[EncodedTokenId] = _.asJson

  implicit val balancesSnapshotEncoder: Encoder[RegistryIndex] = { v =>
    import v._
    Json.obj(
      "height" -> height.asJson,
      "balance" -> balance.asJson,
      "assets" -> assetBalances.map(x => (x._1: String, x._2)).asJson
    )
  }

  implicit def trackedBoxEncoder(implicit opts: Detalization): Encoder[TrackedBox] = { box =>
    val plainFields = Map(
      "spent" -> box.spendingStatus.spent.asJson,
      "onchain" -> box.chainStatus.onChain.asJson,
      "certain" -> box.certainty.certain.asJson,
      "creationOutIndex" -> box.creationOutIndex.asJson,
      "inclusionHeight" -> box.inclusionHeightOpt.asJson,
      "spendingHeight" -> box.spendingHeightOpt.asJson,
      "applicationId" -> box.applicationId.asJson,
      "box" -> box.box.asJson
    )
    val fieldsWithTx = if (opts.showDetails) {
      plainFields +
        ("creationTransaction" -> box.creationTxId.asJson) +
        ("spendingTransaction" -> box.spendingTxIdOpt.asJson)
    } else {
      plainFields +
        ("creationTransactionId" -> box.creationTxId.asJson) +
        ("spendingTransactionId" -> box.spendingTxIdOpt.asJson)
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
