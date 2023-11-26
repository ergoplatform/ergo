package org.ergoplatform.http.api

import io.circe._
import io.circe.syntax._
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.ErgoBox.RegisterId
import org.ergoplatform._
import org.ergoplatform.http.api.ApiEncoderOption.Detalization
import org.ergoplatform.http.api.requests.{CryptoResult, ExecuteRequest, HintExtractionRequest}
import org.ergoplatform.mining.{groupElemFromBytes, groupElemToBytes}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.getAddress
import org.ergoplatform.nodeView.history.extra.{BalanceInfo, IndexedErgoBox, IndexedErgoTransaction, IndexedToken}
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.nodeView.wallet.requests.{ExternalSecret, GenerateCommitmentsRequest, TransactionSigningRequest}
import org.ergoplatform.sdk.wallet.secrets.{DhtSecretKey, DlogSecretKey}
import org.ergoplatform.settings.{Algos, ErgoAlgos}
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.TrackedBox
import org.ergoplatform.wallet.interface4j.SecretString
import org.ergoplatform.wallet.interpreter.TransactionHintsBag
import scorex.core.validation.ValidationResult
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.hash.Digest
import scorex.util.encode.Base16
import sigmastate.Values.SigmaBoolean
import sigmastate._
import sigmastate.crypto.CryptoConstants.EcPointType
import sigmastate.crypto.DLogProtocol.{DLogProverInput, FirstDLogProverMessage, ProveDlog}
import sigmastate.crypto.VerifierMessage.Challenge
import sigmastate.crypto._
import sigmastate.interpreter._
import sigmastate.serialization.OpCodes
import sigma.AnyValue
import org.ergoplatform.nodeView.state.SnapshotsInfo
import org.ergoplatform.nodeView.state.UtxoState.ManifestId
import org.ergoplatform.sdk.JsonCodecs
import sigmastate.eval.Extensions.ArrayOps

import java.math.BigInteger
import scala.util.{Failure, Success, Try}


trait ApiCodecs extends JsonCodecs {

  def fromValidation[T](validationResult: ValidationResult[T])
                       (implicit cursor: ACursor): Either[DecodingFailure, T] = {
    fromTry(validationResult.toTry)
  }

  implicit val leafDataEncoder: Encoder[LeafData] = xs => Base16.encode(xs).asJson

  implicit val digestEncoder: Encoder[Digest] = x => Base16.encode(x).asJson

  implicit val sideEncoder: Encoder[Side] = _.toByte.asJson

  implicit val ergoAddressEncoder: ErgoAddressEncoder = null

  protected implicit def merkleProofEncoder[D <: Digest]: Encoder[MerkleProof[D]] = { proof =>
    Json.obj(
      "leafData" -> proof.leafData.asJson,
      "levels" -> proof.levels.asJson,
    )
  }

  implicit val secretStringEncoder: Encoder[SecretString] = { secret =>
    secret.toStringUnsecure.asJson
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

  implicit val ecPointEncoder: Encoder[EcPointType] = { point: EcPointType =>
    groupElemToBytes(point).asJson
  }

  implicit val proveDlogEncoder: Encoder[ProveDlog] = _.pkBytes.asJson

  implicit val balancesSnapshotEncoder: Encoder[WalletDigest] = { v =>
    import v._
    Json.obj(
      "height" -> height.asJson,
      "balance" -> walletBalance.asJson,
      "assets" -> walletAssetBalances.toMap.map(x => (x._1: String, x._2)).asJson //toMap to have assets as JSON map
    )
  }

  // this val is named "anyRegisterIdEncoder" because parent trait already contains
  // "registerIdEncoder" which is actually a KeyEncoder for NonMandatoryRegisterId
  // todo: rename "registerIdEncoder" into "nonMandatoryRegisterId" in parent trait in sigma repo
  implicit val anyRegisterIdEncoder: Encoder[RegisterId] = { regId =>
    s"R${regId.number}".asJson
  }

  // todo: see comment for "RegisterIdEncoder" above
  implicit val anyRegisterIdDecoder: Decoder[RegisterId] = { implicit cursor =>
    for {
      regId <- cursor.as[String]
      reg <- fromOption(ErgoBox.registerByName.get(regId))
    } yield reg
  }

  implicit val scanIdEncoder: Encoder[ScanId] = { scanId =>
    scanId.toShort.asJson
  }

  implicit val scanIdDecoder: Decoder[ScanId] = { c: HCursor =>
    ScanId @@ c.as[Short]
  }

  implicit def trackedBoxEncoder(implicit opts: Detalization): Encoder[TrackedBox] = { box =>
    val plainFields = Map(
      "spent" -> box.spendingStatus.spent.asJson,
      "onchain" -> box.chainStatus.onChain.asJson,
      "creationOutIndex" -> box.creationOutIndex.asJson,
      "inclusionHeight" -> box.inclusionHeightOpt.asJson,
      "spendingHeight" -> box.spendingHeightOpt.asJson,
      "scans" -> box.scans.asJson,
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

  implicit val secretBigIntEncoder: Encoder[BigInteger] = Encoder.instance { w =>
    ErgoAlgos.encode(BigIntegers.asUnsignedByteArray(CryptoConstants.groupSize, w)).asJson
  }

  implicit val secretBigIntDecoder: Decoder[BigInteger] = arrayBytesDecoder.map { bytes =>
    BigIntegers.fromUnsignedByteArray(bytes)
  }

  implicit val dlogSecretWrapperEncoder: Encoder[DlogSecretKey] = Encoder.instance { dl =>
    secretBigIntEncoder(dl.privateInput.w)
  }

  implicit val dlogSecretWrapperDecoder: Decoder[DlogSecretKey] =
    secretBigIntDecoder
      .map(DLogProverInput.apply)
      .map(DlogSecretKey.apply)

  implicit val dhtSecretWrapperEncoder: Encoder[DhtSecretKey] = { dht =>
    Json.obj(
      "secret" -> dht.privateInput.w.asJson,
      "g" -> dht.privateInput.commonInput.g.asJson,
      "h" -> dht.privateInput.commonInput.h.asJson,
      "u" -> dht.privateInput.commonInput.u.asJson,
      "v" -> dht.privateInput.commonInput.v.asJson
    )
  }

  implicit val dhtSecretWrapperDecoder: Decoder[DhtSecretKey] = { cursor =>
    for {
      secret <- cursor.downField("secret").as[BigInteger]
      g <- cursor.downField("g").as[EcPointType]
      h <- cursor.downField("h").as[EcPointType]
      u <- cursor.downField("u").as[EcPointType]
      v <- cursor.downField("v").as[EcPointType]
    } yield DhtSecretKey(DiffieHellmanTupleProverInput(secret, ProveDHTuple(g, h, u, v)))
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

  implicit val sigmaLeafEncoder: Encoder[SigmaLeaf] = {
    leaf =>
      val op = leaf.opCode.toByte.asJson
      leaf match {
        case dlog: ProveDlog => Map("op" -> op, "h" -> dlog.value.asJson).asJson
        case dht: ProveDHTuple => Map("op" -> op, "g" -> dht.g.asJson, "h" -> dht.h.asJson, "u" -> dht.u.asJson, "v" -> dht.v.asJson).asJson
      }
  }

  implicit val sigmaBooleanEncoder: Encoder[SigmaBoolean] = {
    sigma =>
      val op = sigma.opCode.toByte.asJson
      sigma match {
        case dlog: ProveDlog => Map("op" -> op, "h" -> dlog.value.asJson).asJson
        case dht: ProveDHTuple => Map("op" -> op, "g" -> dht.g.asJson, "h" -> dht.h.asJson, "u" -> dht.u.asJson, "v" -> dht.v.asJson).asJson
        case tp: TrivialProp => Map("op" -> op, "condition" -> tp.condition.asJson).asJson
        case and: CAND =>
          Map("op" -> op, "args" -> and.children.map(_.asJson).asJson).asJson
        case or: COR =>
          Map("op" -> op, "args" -> or.children.map(_.asJson).asJson).asJson
        case th: CTHRESHOLD =>
          Map("op" -> op, "args" -> th.children.map(_.asJson).asJson).asJson
      }
  }

  implicit val sigmaLeafDecoder: Decoder[SigmaLeaf] = Decoder.instance { c =>
    c.downField("op").as[Byte].flatMap {
      case b: Byte if b == OpCodes.ProveDlogCode =>
        c.downField("h").as[EcPointType].map(h => ProveDlog(h))
      case _ =>
        //only dlog is supported for now
        Left(DecodingFailure("Unsupported value", List()))
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

  implicit val hintExtractionRequestEncoder: Encoder[HintExtractionRequest] = {hr =>
    Map(
      "tx" -> hr.tx.asJson,
      "real" -> hr.real.asJson,
      "simulated" -> hr.simulated.asJson,
      "inputsRaw" -> hr.inputs.asJson,
      "dataInputsRaw" -> hr.dataInputs.asJson
    ).asJson
  }

  implicit val hintExtractionRequestDecoder: Decoder[HintExtractionRequest] = {cursor =>
    for {
      tx <- cursor.downField("tx").as[ErgoTransaction]
      real <- cursor.downField("real").as[Seq[SigmaBoolean]]
      simulated <- cursor.downField("simulated").as[Seq[SigmaBoolean]]
      inputs <- cursor.downField("inputsRaw").as[Option[Seq[String]]]
      dataInputs <- cursor.downField("dataInputsRaw").as[Option[Seq[String]]]
    } yield HintExtractionRequest(tx, real, simulated, inputs, dataInputs)
  }

  implicit val firstProverMessageEncoder: Encoder[FirstProverMessage] = {
    case cmtDlog: FirstDLogProverMessage =>
      Json.obj("type" -> "dlog".asJson, "a" -> cmtDlog.ecData.asJson)
    case cmtDht: FirstDHTupleProverMessage =>
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
        } yield FirstDHTupleProverMessage(a, b)
      case _ =>
        Left(DecodingFailure("Unsupported sigma-protocol type value", List()))
    }
  }

  implicit val positionEncoder: Encoder[NodePosition] = { np =>
    np.positions.mkString("-").asJson
  }

  implicit val positionDecoder: Decoder[NodePosition] = { c =>
    c.as[String].flatMap {s =>
      Try(s.split("-").map(_.toInt)) match {
        case Success(seq) => Right(NodePosition(seq))
        case Failure(e) => Left(DecodingFailure.fromThrowable(e, List()))
      }
    }
  }

  implicit val commitmentHintEncoder: Encoder[CommitmentHint] = { ch =>
    val commonFields: Json = (ch match {
      case own: OwnCommitment =>
        Json.obj("hint" -> "cmtWithSecret".asJson, "secret" -> own.secretRandomness.asJson)
      case _: RealCommitment =>
        Json.obj("hint" -> "cmtReal".asJson)
      case _: SimulatedCommitment =>
        Json.obj("hint" -> "cmtSimulated".asJson)
    }).deepMerge(Json.obj("pubkey" -> ch.image.asJson, "position" -> ch.position.asJson))

    val cmt = ch.commitment.asJson
    commonFields.deepMerge(cmt)
  }

  implicit val commitmentHintDecoder: Decoder[CommitmentHint] = Decoder.instance { c =>
    c.downField("hint").as[String].flatMap {
      case h: String if h == "cmtWithSecret" =>
        for {
          secret <- c.downField("secret").as[BigInteger]
          pubkey <- c.downField("pubkey").as[SigmaLeaf]
          position <- c.downField("position").as[NodePosition]
          firstMsg <- firstProverMessageDecoder.tryDecode(c)
        } yield OwnCommitment(pubkey, secret, firstMsg, position)
      case h: String if h == "cmtReal" =>
        for {
          pubkey <- c.downField("pubkey").as[SigmaLeaf]
          position <- c.downField("position").as[NodePosition]
          firstMsg <- firstProverMessageDecoder.tryDecode(c)
        } yield RealCommitment(pubkey, firstMsg, position)
      case h: String if h == "cmtSimulated" =>
        for {
          position <- c.downField("position").as[NodePosition]
          pubkey <- c.downField("pubkey").as[SigmaLeaf]
          firstMsg <- firstProverMessageDecoder.tryDecode(c)
        } yield SimulatedCommitment(pubkey, firstMsg, position)
      case _ =>
        //only dlog is supported for now
        Left(DecodingFailure("Unsupported hint value", List()))
    }
  }

  implicit val proofEncoder: Encoder[SecretProven] = { sp =>
    val proofType = sp match {
      case _: RealSecretProof => "proofReal"
      case _: SimulatedSecretProof => "proofSimulated"
    }

    Json.obj(
      "hint" -> proofType.asJson,
      "challenge" -> Base16.encode(sp.challenge.toArray).asJson,
      "pubkey" -> sp.image.asJson,
      "proof" -> SigSerializer.toProofBytes(sp.uncheckedTree).asJson,
      "position" -> sp.position.asJson
    )
  }

  implicit val secretProofDecoder: Decoder[SecretProven] = { c =>
    c.downField("hint").as[String].flatMap {
      case h: String if h == "proofReal" =>
        for {
          challenge <- c.downField("challenge").as[String]
          pubkey <- c.downField("pubkey").as[SigmaLeaf]
          proof <- c.downField("proof").as[String]
          position <- c.downField("position").as[NodePosition]
        } yield
          RealSecretProof(
            pubkey,
            Challenge @@ Base16.decode(challenge).get.toColl,
            SigSerializer.parseAndComputeChallenges(pubkey, Base16.decode(proof).get)(null),
            position
          )
      case h: String if h == "proofSimulated" =>
        for {
          challenge <- c.downField("challenge").as[String]
          pubkey <- c.downField("pubkey").as[SigmaLeaf]
          proof <- c.downField("proof").as[String]
          position <- c.downField("position").as[NodePosition]
        } yield
          SimulatedSecretProof(
            pubkey,
            Challenge @@ Base16.decode(challenge).get.toColl,
            SigSerializer.parseAndComputeChallenges(pubkey, Base16.decode(proof).get)(null),
            position
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

  implicit val txHintsEncoder: Encoder[TransactionHintsBag] = { bag =>
    Json.obj(
      "secretHints" ->
        bag.secretHints.map { case (inputIdx, inputHints) =>
          inputIdx -> inputHints.hints
        }.asJson,
      "publicHints" -> bag.publicHints.map { case (inputIdx, inputHints) =>
        inputIdx -> inputHints.hints
      }.asJson
    )
  }

  implicit val txHintsDecoder: Decoder[TransactionHintsBag] = { cursor =>
    for {
      secretHints <- Decoder.decodeMap[Int, Seq[Hint]].tryDecode(cursor.downField("secretHints"))
      publicHints <- Decoder.decodeMap[Int, Seq[Hint]].tryDecode(cursor.downField("publicHints"))
    } yield TransactionHintsBag(secretHints.mapValues(HintsBag.apply), publicHints.mapValues(HintsBag.apply))
  }

  implicit val SnapshotInfoEncoder: Encoder[SnapshotsInfo] = { si =>
    Json.obj(
      "availableManifests" -> si.availableManifests.map { case (height, manifest) =>
        height -> manifest
      }.asJson
    )
  }

  implicit val SnapshotInfoDecoder: Decoder[SnapshotsInfo] = { cursor =>
    for {
      availableManifests <- Decoder.decodeMap[Int, ManifestId].tryDecode(cursor.downField("availableManifests"))
    } yield new SnapshotsInfo(availableManifests)
  }

  implicit val transactionSigningRequestEncoder: Encoder[TransactionSigningRequest] = { tsr =>
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

  implicit val transactionSigningRequestDecoder: Decoder[TransactionSigningRequest] = { cursor =>
    for {
      tx <- cursor.downField("tx").as[UnsignedErgoTransaction]
      dlogs <- cursor.downField("secrets").downField("dlog").as[Option[Seq[DlogSecretKey]]]
      dhts <- cursor.downField("secrets").downField("dht").as[Option[Seq[DhtSecretKey]]]
      hints <- cursor.downField("hints").as[Option[TransactionHintsBag]]
      inputs <- cursor.downField("inputsRaw").as[Option[Seq[String]]]
      dataInputs <- cursor.downField("dataInputsRaw").as[Option[Seq[String]]]
      secrets = (dlogs.getOrElse(Seq.empty) ++ dhts.getOrElse(Seq.empty)).map(ExternalSecret.apply)
    } yield TransactionSigningRequest(tx, hints.getOrElse(TransactionHintsBag.empty), secrets, inputs, dataInputs)
  }

  implicit val generateCommitmentsRequestEncoder: Encoder[GenerateCommitmentsRequest] = { gcr =>
    Json.obj(
      "tx" -> gcr.unsignedTx.asJson,
      "secrets" -> Json.obj(
        "dlog" -> gcr.dlogs.asJson,
        "dht" -> gcr.dhts.asJson,
        "inputsRaw" -> gcr.inputs.asJson,
        "dataInputsRaw" -> gcr.dataInputs.asJson
      )
    )
  }

  implicit val generateCommitmentsRequestDecoder: Decoder[GenerateCommitmentsRequest] = { cursor =>
    for {
      tx <- cursor.downField("tx").as[UnsignedErgoTransaction]
      dlogs <- cursor.downField("secrets").downField("dlog").as[Option[Seq[DlogSecretKey]]]
      dhts <- cursor.downField("secrets").downField("dht").as[Option[Seq[DhtSecretKey]]]
      secrets = (dlogs.getOrElse(Seq.empty) ++ dhts.getOrElse(Seq.empty)).map(ExternalSecret.apply)
      secretsOpt = if(secrets.isEmpty) None else Some(secrets)
      inputs <- cursor.downField("inputsRaw").as[Option[Seq[String]]]
      dataInputs <- cursor.downField("dataInputsRaw").as[Option[Seq[String]]]
    } yield GenerateCommitmentsRequest(tx, secretsOpt, inputs, dataInputs)
  }

  implicit val executeRequestDecoder = new Decoder[ExecuteRequest] {
    def apply(cursor: HCursor): Decoder.Result[ExecuteRequest] = {
      for {
        script <- cursor.downField("script").as[String]
        env <- cursor.downField("namedConstants").as[Map[String,AnyValue]]
        ctx <- cursor.downField("context").as[ErgoLikeContext]
      } yield ExecuteRequest(script, env.map({ case (k,v) => k -> v.value }), ctx)
    }
  }

  implicit val cryptResultEncoder: Encoder[CryptoResult] = {
    res =>
      val fields = Map(
        "value" -> res.value.asJson,
        "cost" -> res.cost.asJson
      )
      fields.asJson
  }

  implicit val indexedBoxEncoder: Encoder[IndexedErgoBox] = { iEb =>
    iEb.box.asJson.deepMerge(Json.obj(
      "globalIndex" -> iEb.globalIndex.asJson,
      "inclusionHeight" -> iEb.inclusionHeight.asJson,
      "address" -> ergoAddressEncoder.toString(getAddress(iEb.box.ergoTree)).asJson,
      "spentTransactionId" -> iEb.spendingTxIdOpt.asJson
    ))
  }

  implicit val indexedBoxSeqEncoder: Encoder[(Seq[IndexedErgoBox],Long)] = { iEbSeq =>
    Json.obj(
      "items" -> iEbSeq._1.asJson,
      "total" -> iEbSeq._2.asJson
    )
  }

  implicit val indexedTxEncoder: Encoder[IndexedErgoTransaction] = { iEt =>
    Json.obj(
      "id" -> iEt.txid.asJson,
      "blockId" -> iEt.blockId.asJson,
      "inclusionHeight" -> iEt.inclusionHeight.asJson,
      "timestamp" -> iEt.timestamp.asJson,
      "index" -> iEt.index.asJson,
      "globalIndex" -> iEt.globalIndex.asJson,
      "numConfirmations" -> iEt.numConfirmations.asJson,
      "inputs" -> iEt.inputs.asJson,
      "dataInputs" -> iEt.dataInputs.asJson,
      "outputs" -> iEt.outputs.asJson,
      "size" -> iEt.size.asJson
    )
  }

  implicit val indexedTxSeqEncoder: Encoder[(Seq[IndexedErgoTransaction],Long)] = { iEtSeq =>
    Json.obj(
      "items" -> iEtSeq._1.asJson,
      "total" -> iEtSeq._2.asJson
    )
  }

  implicit val IndexedTokenEncoder: Encoder[IndexedToken] = { token =>
    Json.obj(
      "id" -> token.tokenId.asJson,
      "boxId" -> token.boxId.asJson,
      "emissionAmount" -> token.amount.asJson,
      "name" -> token.name.asJson,
      "description" -> token.description.asJson,
      "decimals" -> token.decimals.asJson
    )
  }

  implicit val BalanceInfoEncoder: Encoder[BalanceInfo] = { bal =>
    Json.obj(
      "nanoErgs" -> bal.nanoErgs.asJson,
      "tokens" -> bal.tokens.map(token => {
        Json.obj(
          "tokenId" -> token._1.asJson,
          "amount" -> token._2.asJson,
          "decimals" -> bal.additionalTokenInfo(token._1)._2.asJson,
          "name" -> bal.additionalTokenInfo(token._1)._1.asJson
        )
      }).asJson
    )
  }

  implicit val TotalBalanceInfoEncoder: Encoder[(BalanceInfo,BalanceInfo)] = { tBal =>
    Json.obj(
      "confirmed" -> tBal._1.asJson,
      "unconfirmed" -> tBal._2.asJson
    )
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
