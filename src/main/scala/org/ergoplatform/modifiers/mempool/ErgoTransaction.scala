package org.ergoplatform.modifiers.mempool

import java.util

import io.circe._
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId}
import org.ergoplatform.ErgoLikeTransaction.flattenedTxSerializer
import org.ergoplatform.ErgoTransactionValidator.verifier
import org.ergoplatform._
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.settings.Algos
import scorex.core.ModifierId
import scorex.core.serialization.Serializer
import sigmastate.serialization.{Serializer => SSerializer}
import scorex.core.transaction.Transaction
import scorex.core.utils.{ScorexEncoding, ScorexLogging}
import scorex.core.validation.{ModifierValidator, ValidationResult}
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{EvaluatedValue, Value}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.{AvlTreeData, SBoolean, SType}
import sigmastate.serialization.Serializer.{Consumed, Position}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}


case class ErgoTransaction(override val inputs: IndexedSeq[Input],
                           override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends Transaction with ErgoLikeTransactionTemplate[Input] with MempoolModifier
    with ModifierValidator with ScorexLogging {

  override type IdType = ModifierId

  override lazy val id: ModifierId = ModifierId @@ Blake2b256.hash(messageToSign)

  private def fillAssetsMap(boxes: IndexedSeq[ErgoBoxCandidate],
                            map: mutable.Map[ByteArrayWrapper, Long]) = Try {
    boxes.foreach { box =>
      require(box.additionalTokens.size <= ErgoBox.MaxTokens, "Output contains too many assets")
      box.additionalTokens.foreach { case (assetId, amount) =>
        require(amount >= 0, s"negative asset amount for ${Base16.encode(assetId)}")
        val aiWrapped = ByteArrayWrapper(assetId)
        val total = map.getOrElse(aiWrapped, 0L)
        map.put(aiWrapped, Math.addExact(total, amount))
        require(map.size <= ErgoTransaction.MaxTokens, "Transaction is operating with too many assets")
      }
    }
  }

  lazy val outAssetsOpt: Try[Map[ByteArrayWrapper, Long]] = {
    val mutableMap = mutable.Map[ByteArrayWrapper, Long]()
    fillAssetsMap(outputCandidates, mutableMap).map(_ => mutableMap.toMap)
  }

  /**
    * statelessValidity is checking whether aspects of a transaction is valid which do not require the state to check.
    *
    * @return Success(Unit) if transaction is valid, Failure(e) if transaction is invalid, with respect to
    *         an error encapsulated in the exception "e".
    */
  def statelessValidity: Try[Unit] = validateStateless.toTry

  /** Stateless transaction validation with result returned as [[ValidationResult]]
    * to accumulate further validation results
    */
  def validateStateless: ValidationResult = {
    failFast
      .demand(outputCandidates.nonEmpty, s"No outputs in transaction $toString")
      .demand(inputs.nonEmpty, s"No inputs in transaction $toString")
      .demand(inputs.size <= Short.MaxValue, s"Too many inputs in transaction $toString")
      .demand(outputCandidates.size <= Short.MaxValue, s"Too many outputCandidates in transaction $toString")
      .demand(outputCandidates.forall(_.value >= 0), s"Transaction has an output with negative amount $toString")
      .demand(Try(outputCandidates.map(_.value).reduce(Math.addExact(_, _))).isSuccess, s"Overflow in outputs in $toString")
      .demand(outAssetsOpt.isSuccess, s"Asset rules violated in $toString: ${outAssetsOpt.failed.get.getMessage}")
      .result
  }

  /**
    * @return total computation cost
    */
  def statefulValidity(boxesToSpend: IndexedSeq[ErgoBox], blockchainState: ErgoStateContext): Try[Long] = Try {
    require(boxesToSpend.size == inputs.size, s"boxesToSpend.size ${boxesToSpend.size} != inputs.size ${inputs.size}")

    lazy val lastUtxoDigest = AvlTreeData(blockchainState.digest, ErgoBox.BoxId.size)

    lazy val txCost = boxesToSpend.zipWithIndex.foldLeft(0L) { case (accCost, (box, idx)) =>
      val input = inputs(idx)
      require(util.Arrays.equals(box.id, input.boxId))

      val proof = input.spendingProof
      val proverExtension = inputs(idx).spendingProof.extension
      val context =
        ErgoLikeContext(blockchainState.height, lastUtxoDigest, boxesToSpend, this, box, proverExtension)

      val scriptCost: Long = verifier.verify(box.proposition, context, proof, messageToSign) match {
        case Success((res, cost)) =>
          if (!res) throw new Exception(s"Validation failed for input #$idx of tx $toString") else cost
        case Failure(e) =>
          log.warn(s"Invalid transaction $toString: ", e)
          throw e
      }
      accCost + scriptCost
    }

    lazy val inputSum = boxesToSpend.map(_.value).reduce(Math.addExact(_, _))
    lazy val outputSum = outputCandidates.map(_.value).reduce(Math.addExact(_, _))

    def checkAssetPreservationRules = {
      outAssetsOpt match {
        case Success(outAssets) =>
          val inAssets = mutable.Map[ByteArrayWrapper, Long]()
          fillAssetsMap(boxesToSpend, inAssets)
          lazy val newAssetId = ByteArrayWrapper(inputs.head.boxId)
          outAssets.keysIterator.forall { assetId =>
            val inAmount = inAssets.remove(assetId).getOrElse(-1)
            val outAmount = outAssets.getOrElse(assetId, 0L)
            inAmount == outAmount || (assetId == newAssetId && outAmount > 0)
          }
        case Failure(_) => false
      }
    }

    failFast
      .demand(inputSum == outputSum, s"Ergo token preservation is broken in $toString")
      .demand(checkAssetPreservationRules, s"Assets preservation rule is broken in $toString")
      .result
      .toTry
      .map(_ => txCost)
  }.flatten

  override type M = ErgoTransaction

  override def serializer: Serializer[ErgoTransaction] = ErgoTransactionSerializer

  override def toString: String = this.asJson.noSpaces
}


object ErgoTransaction extends ApiCodecs with ModifierValidator with ScorexLogging with ScorexEncoding {

  //how many tokens the transaction can contain in outputs
  val MaxTokens = 16

  implicit private val extensionEncoder: Encoder[ContextExtension] = { extension =>
    extension.values.map { case (key, value) =>
      key -> valueEncoder(value)
    }.asJson
  }

  implicit private val inputEncoder: Encoder[Input] = { input =>
    Json.obj(
      "boxId" -> input.boxId.asJson,
      "spendingProof" -> Json.obj(
        "proofBytes" -> byteSeqEncoder(input.spendingProof.proof),
        "extension" -> extensionEncoder(input.spendingProof.extension)
      )
    )
  }

  implicit val proofDecoder: Decoder[ProverResult] = { cursor =>
    for {
      proofBytes <- cursor.downField("proofBytes").as[Array[Byte]]
      extMap <- cursor.downField("extension").as[Map[Byte, EvaluatedValue[SType]]]
    } yield ProverResult(proofBytes, ContextExtension(extMap))
  }

  implicit private val inputDecoder: Decoder[Input] = { cursor =>
    for {
      boxId <- cursor.downField("boxId").as[ADKey]
      proof <- cursor.downField("spendingProof").as[ProverResult]
    } yield Input(boxId, proof)
  }

  implicit private val registersEncoder: Encoder[Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]] = {
    _.map { case (key, value) =>
      s"R${key.number}" -> valueEncoder(value)
    }.asJson
  }

  implicit private val outputEncoder: Encoder[ErgoBox] = { box =>
    Json.obj(
      "boxId" -> box.id.asJson,
      "value" -> box.value.asJson,
      "proposition" -> valueEncoder(box.proposition),
      "assets" -> box.additionalTokens.asJson,
      "additionalRegisters" -> registersEncoder(box.additionalRegisters)
    )
  }

  implicit private val identifierDecoder: KeyDecoder[NonMandatoryRegisterId] = { key =>
    ErgoBox.registerByName.get(key).collect {
      case nonMandatoryId: NonMandatoryRegisterId => nonMandatoryId
    }
  }

  implicit private val outputDecoder: Decoder[(ErgoBoxCandidate, Option[BoxId])] = { cursor =>
    for {
      maybeId <- cursor.downField("boxId").as[Option[BoxId]]
      value <- cursor.downField("value").as[Long]
      proposition <- cursor.downField("proposition").as[Value[SBoolean.type]]
      assets <- cursor.downField("assets").as[Seq[(ErgoBox.TokenId, Long)]]
      registers <- cursor.downField("additionalRegisters").as[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]
    } yield (new ErgoBoxCandidate(value, proposition, assets, registers), maybeId)
  }

  implicit val transactionEncoder: Encoder[ErgoTransaction] = { tx =>
    Json.obj(
      "id" -> tx.id.asJson,
      "inputs" -> tx.inputs.asJson,
      "outputs" -> tx.outputs.asJson
    )
  }

  implicit val transactionDecoder: Decoder[ErgoTransaction] = { implicit cursor =>
    for {
      maybeId <- cursor.downField("id").as[Option[ModifierId]]
      inputs <- cursor.downField("inputs").as[IndexedSeq[Input]]
      outputs <- cursor.downField("outputs").as[IndexedSeq[(ErgoBoxCandidate, Option[BoxId])]]
      result <- validateDecodedTransaction(inputs, outputs, maybeId)
    } yield result
  }

  def validateDecodedTransaction(inputs: IndexedSeq[Input], outputs: IndexedSeq[(ErgoBoxCandidate, Option[BoxId])],
                                 maybeId: Option[ModifierId])(implicit cursor: ACursor): Decoder.Result[ErgoTransaction] = {
    val tx = ErgoTransaction(inputs, outputs.map(_._1))
    val result = accumulateErrors
      .validate(maybeId.forall(id => util.Arrays.equals(id, tx.id))) {
        fatal(s"Bad identifier ${Algos.encode(maybeId.get)} for ergo transaction. " +
          s"Identifier could be skipped, or should be ${Algos.encode(tx.id)}")
      }
      .validate(tx.validateStateless)
      .validate(validateOutputs(outputs, tx.id))
      .result
    if (!result.isValid) log.info(s"Transaction from json validation failed: ${result.message}")
    result.toDecoderResult(tx)
  }

  def validateOutputs(outputs: IndexedSeq[(ErgoBoxCandidate, Option[BoxId])], txId: ModifierId): ValidationResult = {
    outputs.zipWithIndex.foldLeft(accumulateErrors) {
      case (validationState, ((candidate, maybeId), index)) =>
        maybeId.map { boxId =>
          val box = candidate.toBox(txId, index.toShort)
          validationState.validate(util.Arrays.equals(boxId, box.id)) {
            fatal(s"Bad identifier ${Algos.encode(boxId)} for ergo box." +
              s"Identifier could be skipped, or should be ${Algos.encode(box.id)}")
          }
        }.getOrElse(validationState)
    }.result
  }
}

object ErgoTransactionSerializer extends Serializer[ErgoTransaction] with SSerializer[ErgoTransaction, ErgoTransaction] {
  override def toBytes(tx: ErgoTransaction): Array[Byte] =
    flattenedTxSerializer.toBytes(tx.inputs, tx.outputCandidates)

  override def parseBody(bytes: Array[Byte], pos: Position): (ErgoTransaction, Consumed) = {
    val ((inputs, outputCandidates), consumed) = flattenedTxSerializer.parseBody(bytes, pos)
    ErgoTransaction(inputs, outputCandidates) -> consumed
  }
}