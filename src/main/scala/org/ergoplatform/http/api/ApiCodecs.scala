package org.ergoplatform.http.api

import java.math.BigInteger

import io.circe._
import io.circe.syntax._
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.{ErgoLikeTransaction, JsonCodecs, UnsignedErgoLikeTransaction}
import org.ergoplatform.http.api.ApiEncoderOption.Detalization
import org.ergoplatform.mining.{groupElemFromBytes, groupElemToBytes}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.nodeView.wallet.IdUtils.EncodedTokenId
import org.ergoplatform.nodeView.wallet.persistence.RegistryIndex
import org.ergoplatform.settings.{Algos, ErgoAlgos}
import org.ergoplatform.wallet.boxes.TrackedBox
import org.ergoplatform.wallet.secrets.{DhtSecretWrapper, DlogSecretWrapper}
import scorex.core.validation.ValidationResult
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.basics.{DiffieHellmanTupleProverInput, ProveDHTuple}
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


  implicit val secretBigIntEncoder: Encoder[BigInteger] = Encoder.instance{w =>
    ErgoAlgos.encode(BigIntegers.asUnsignedByteArray(w)).asJson
  }

  implicit val secretBigIntDecoder: Decoder[BigInteger] = arrayBytesDecoder.map{ bytes=>
    BigIntegers.fromUnsignedByteArray(bytes)
  }

  implicit val dlogSecretWrapperEncoder: Encoder[DlogSecretWrapper] = Encoder.instance{dl =>
    secretBigIntEncoder(dl.key.w)
  }

  implicit val dlogSecretWrapperDecoder: Decoder[DlogSecretWrapper] =
    secretBigIntDecoder
      .map(DLogProverInput.apply)
      .map(DlogSecretWrapper.apply)

  implicit val dhtSecretWrapperEncoder: Encoder[DhtSecretWrapper] = { dht =>
    Json.obj(
      "secret" -> dht.key.w.asJson,
      "g" -> dht.key.commonInput.g.asJson,
      "h" -> dht.key.commonInput.h.asJson,
      "u" -> dht.key.commonInput.u.asJson,
      "v" -> dht.key.commonInput.v.asJson
    )
  }

  implicit val dhtSecretWrapperDecoder: Decoder[DhtSecretWrapper] = {cursor =>
    for {
      secret <- cursor.downField("secret").as[BigInteger]
      g <- cursor.downField("g").as[EcPointType]
      h <- cursor.downField("h").as[EcPointType]
      u <- cursor.downField("u").as[EcPointType]
      v <- cursor.downField("v").as[EcPointType]
    } yield DhtSecretWrapper(DiffieHellmanTupleProverInput(secret, ProveDHTuple(g, h, u, v)))
  }

  implicit val unsignedTransactionEncoder: Encoder[UnsignedErgoTransaction] = { tx =>
    tx.asInstanceOf[UnsignedErgoLikeTransaction].asJson
  }

  implicit val unsignedTransactionDecoder: Decoder[UnsignedErgoTransaction] = { cursor =>
    for {
      ergoLikeTx <- cursor.as[UnsignedErgoLikeTransaction]
    } yield UnsignedErgoTransaction(ergoLikeTx)
  }

  implicit val transactionEncoder: Encoder[ErgoTransaction] = { tx =>
    tx.asInstanceOf[ErgoLikeTransaction].asJson
      .mapObject(_.add("size", tx.size.asJson))
  }

  implicit val transactionDecoder: Decoder[ErgoTransaction] = { cursor =>
    for {
      ergoLikeTx <- cursor.as[ErgoLikeTransaction]
    } yield ErgoTransaction(ergoLikeTx)
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
