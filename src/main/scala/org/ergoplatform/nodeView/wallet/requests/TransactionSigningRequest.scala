package org.ergoplatform.nodeView.wallet.requests

import java.math.BigInteger

import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import org.ergoplatform.wallet.secrets.{DhtSecretKey, DlogSecretKey, PrimitiveSecretKey}
import scorex.util.encode.Base16
import sigmastate.Values.SigmaBoolean
import sigmastate.{CAND, COR, CTHRESHOLD, SigSerializer, TrivialProp}
import sigmastate.basics.DLogProtocol.{FirstDLogProverMessage, ProveDlog}
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.basics.{FirstDiffieHellmanTupleProverMessage, FirstProverMessage, ProveDHTuple}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter._
import sigmastate.serialization.OpCodes


/**
  * Externally provided secret (to be used once for a transaction to sign)
  *
  * @param key - the secret
  */
case class ExternalSecret(key: PrimitiveSecretKey)

/**
  * A request to sign a transaction
  *
  * @param unsignedTx - unsigned transaction
  * @param hints      - hints for interpreter (such as additional one-time secrets)
  * @param externalSecrets - externally provided secrets
  * @param inputs     - hex-encoded input boxes bytes for the unsigned transaction (optional)
  * @param dataInputs - hex-encoded data-input boxes bytes for the unsigned transaction (optional)
  */
case class TransactionSigningRequest(unsignedTx: UnsignedErgoTransaction,
                                     hints: Seq[Hint],
                                     externalSecrets: Seq[ExternalSecret],
                                     inputs: Option[Seq[String]],
                                     dataInputs: Option[Seq[String]]) {

  lazy val dlogs: Seq[DlogSecretKey] = externalSecrets.collect { case ExternalSecret(d: DlogSecretKey) => d }

  lazy val dhts: Seq[DhtSecretKey] = externalSecrets.collect { case ExternalSecret(d: DhtSecretKey) => d }

  lazy val hintsBag = HintsBag(hints)

}

object SigmaBooleanCodecs extends ApiCodecs {

  import io.circe.syntax._

  implicit val sigmaBooleanEncoder: Encoder[SigmaBoolean] = {
    sigma =>
      val op = sigma.opCode.toByte.asJson
      sigma match {
        case dlog: ProveDlog => Map("op" -> op, "h" -> dlog.h.asJson).asJson
        case dht: ProveDHTuple => Map("op" -> op, "g" -> dht.g.asJson, "h" -> dht.h.asJson, "u" -> dht.u.asJson, "v" -> dht.v.asJson).asJson
        case tp: TrivialProp => Map("op" -> op, "condition" -> tp.condition.asJson).asJson
        case and: CAND =>
          Map("op" -> op, "args" -> and.sigmaBooleans.map(_.asJson).asJson).asJson
        case or: COR =>
          Map("op" -> op, "args" -> or.sigmaBooleans.map(_.asJson).asJson).asJson
        case th: CTHRESHOLD =>
          Map("op" -> op, "args" -> th.sigmaBooleans.map(_.asJson).asJson).asJson
      }
  }

  implicit val sigmaBooleanDecoder: Decoder[SigmaBoolean] = Decoder.instance { c =>
    c.downField("op").as[Byte].flatMap {
      case b: Byte if b == OpCodes.ProveDlogCode =>
        c.downField("h").as[EcPointType].map(h => ProveDlog(h))
      case _ =>
        //only dlog is supported for now
        Left(DecodingFailure("Unsupported value", List()))
    }
  }

}


object HintCodecs extends ApiCodecs {

  import SigmaBooleanCodecs._
  import io.circe.syntax._

  implicit val firstProverMessageEncoder: Encoder[FirstProverMessage] = {
    case cmtDlog: FirstDLogProverMessage =>
      Json.obj("type" -> "dlog".asJson, "a" -> cmtDlog.ecData.asJson)
    case cmtDht: FirstDiffieHellmanTupleProverMessage =>
      Json.obj("type" -> "dht".asJson, "a" -> cmtDht.a.asJson, "b" -> cmtDht.b.asJson)
    case _ => ???
  }

  implicit val firstProverMessageDecoder: Decoder[FirstProverMessage] = { c =>
    c.downField("type").as[String].flatMap {
      case h: String if h == "dlog" =>
        for {
          a <- c.downField("a").as[EcPointType]
        } yield FirstDLogProverMessage(a)
      case h: String if h == "dht" =>
        for {
          a <- c.downField("a").as[EcPointType]
          b <- c.downField("b").as[EcPointType]
        } yield FirstDiffieHellmanTupleProverMessage(a, b)
      case _ =>
        Left(DecodingFailure("Unsupported sigma-protocol type value", List()))
    }
  }

  implicit val commitmentHintEncoder: Encoder[CommitmentHint] = { ch =>
    val commonFields: Json = (ch match {
      case own: OwnCommitment => Json.obj("hint" -> "cmtWithSecret".asJson, "secret" -> own.secretRandomness.asJson)
      case _: RealCommitment => Json.obj("hint" -> "cmtReal".asJson)
      case _: SimulatedCommitment => Json.obj("hint" -> "cmtSimulated".asJson)
    }).deepMerge(Json.obj("pubkey" -> ch.image.asJson))

    val cmt = ch.commitment.asJson
    commonFields.deepMerge(cmt)
  }

  implicit val commitmentHintDecoder: Decoder[CommitmentHint] = Decoder.instance { c =>
    c.downField("hint").as[String].flatMap {
      case h: String if h == "cmtWithSecret" =>
        for {
          secret <- c.downField("secret").as[BigInteger]
          pubkey <- c.downField("pubkey").as[SigmaBoolean]
          firstMsg <- firstProverMessageDecoder.tryDecode(c)
        } yield OwnCommitment(pubkey, secret, firstMsg)
      case h: String if h == "cmtReal" =>
        for {
          pubkey <- c.downField("pubkey").as[SigmaBoolean]
          firstMsg <- firstProverMessageDecoder.tryDecode(c)
        } yield RealCommitment(pubkey, firstMsg)
      case h: String if h == "cmtSimulated" =>
        for {
          pubkey <- c.downField("pubkey").as[SigmaBoolean]
          firstMsg <- firstProverMessageDecoder.tryDecode(c)
        } yield SimulatedCommitment(pubkey, firstMsg)
      case _ =>
        //only dlog is supported for now
        Left(DecodingFailure("Unsupported hint value", List()))
    }
  }

  implicit val secretProofEncoder: Encoder[SecretProven] = { sp =>
    //todo: fix .isInstanceOf with adding a flag to SecretProven

    val proofType = if(sp.isInstanceOf[RealSecretProof]) "proofReal" else "proofSimulated"

    Json.obj(
      "hint" -> proofType.asJson,
      "challenge" -> Base16.encode(sp.challenge).asJson,
      "pubkey" -> sp.image.asJson,
      "proof" -> SigSerializer.toBytes(sp.uncheckedTree).asJson
    )
  }

  implicit val secretProofDecoder: Decoder[SecretProven] = { c =>
    c.downField("hint").as[String].flatMap {
      case h: String if h == "proofReal" =>
        for {
          challenge <- c.downField("challenge").as[String]
          pubkey <- c.downField("pubkey").as[SigmaBoolean]
          proof <- c.downField("proof").as[String]
        } yield RealSecretProof(pubkey,
          Challenge @@ Base16.decode(challenge).get,
          SigSerializer.parseAndComputeChallenges(pubkey, Base16.decode(proof).get)
        )
      case h: String if h == "proofSimulated" =>
        for {
          challenge <- c.downField("challenge").as[String]
          pubkey <- c.downField("pubkey").as[SigmaBoolean]
          proof <- c.downField("proof").as[String]
        } yield SimulatedSecretProof(pubkey,
          Challenge @@ Base16.decode(challenge).get,
          SigSerializer.parseAndComputeChallenges(pubkey, Base16.decode(proof).get)
        )
      case _ =>
        //only dlog is supported for now
        Left(DecodingFailure("Unsupported hint value", List()))
    }
  }

  implicit val hintEncoder: Encoder[Hint] = {
    case cmt: CommitmentHint => cmt.asJson
    case proof: SecretProven => proof.asJson
    case _ => ???
  }

  implicit val hintDecoder: Decoder[Hint] = { cursor =>
    Seq(commitmentHintDecoder, secretProofDecoder)
      .map(_.apply(cursor))
      .find(_.isRight)
      .getOrElse(Left(DecodingFailure("Can not find suitable decoder", cursor.history)))
  }

}

object TransactionSigningRequest extends ApiCodecs {

  import HintCodecs._

  import io.circe.syntax._

  implicit val encoder: Encoder[TransactionSigningRequest] = { tsr =>
    Json.obj(
      "tx" -> tsr.unsignedTx.asJson,
      "secrets" -> Json.obj(
        "dlog" -> tsr.dlogs.asJson,
        "dht" -> tsr.dhts.asJson
      ),
      "hints" -> tsr.hints.asJson,
      "inputsRaw" -> tsr.inputs.asJson,
      "dataInputsRaw" -> tsr.dataInputs.asJson
    )
  }

  implicit val decoder: Decoder[TransactionSigningRequest] = { cursor =>
    for {
      tx <- cursor.downField("tx").as[UnsignedErgoTransaction]
      dlogs <- cursor.downField("secrets").downField("dlog").as[Option[Seq[DlogSecretKey]]]
      dhts <- cursor.downField("secrets").downField("dht").as[Option[Seq[DhtSecretKey]]]
      hints <- cursor.downField("hints").as[Option[Seq[Hint]]]
      inputs <- cursor.downField("inputsRaw").as[Option[Seq[String]]]
      dataInputs <- cursor.downField("dataInputsRaw").as[Option[Seq[String]]]
      secrets = (dlogs.getOrElse(Seq.empty) ++ dhts.getOrElse(Seq.empty)).map(ExternalSecret.apply)
    } yield TransactionSigningRequest(tx, hints.getOrElse(Seq.empty), secrets, inputs, dataInputs)
  }

}
