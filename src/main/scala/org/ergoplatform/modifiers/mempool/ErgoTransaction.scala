package org.ergoplatform.modifiers.mempool

import cats.Eval
import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryIdentifier}
import org.ergoplatform.ErgoLikeTransaction.flattenedTxSerializer
import org.ergoplatform.ErgoTransactionValidator.verifier
import org.ergoplatform._
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.settings.{Algos, ApiSettings, NodeConfigurationSettings}
import org.ergoplatform.utils.JsonEncoders
import scorex.core.ModifierId
import scorex.core.serialization.Serializer
import sigmastate.serialization.{Serializer => SSerializer}
import scorex.core.transaction.Transaction
import scorex.core.utils.ScorexLogging
import scorex.core.validation.{ModifierValidator, ValidationResult}
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{EvaluatedValue, Value}
import sigmastate.interpreter.{ContextExtension, SerializedProverResult}
import sigmastate.{AvlTreeData, SBoolean, SType}
import sigmastate.serialization.Serializer.{Consumed, Position}

import scala.util.{Failure, Success, Try}


case class ErgoTransaction(override val inputs: IndexedSeq[Input],
                           override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends Transaction with ErgoLikeTransactionTemplate[Input] with MempoolModifier
     with ModifierValidator with ScorexLogging {

  override type IdType = ModifierId

  override lazy val id: ModifierId = ModifierId @@ Blake2b256.hash(messageToSign)

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
    accumulateErrors
      .demand(inputs.nonEmpty, s"No inputs in transaction $toString")
      .demand(inputs.size <= Short.MaxValue, s"Too many inputs in transaction $toString")
      .demand(outputCandidates.size <= Short.MaxValue, s"Too many outputCandidates in transaction $toString")
      .result
  }

  /**
    * @return total coimputation cost
    */
  def statefulValidity(boxesToSpend: IndexedSeq[ErgoBox], blockchainState: ErgoStateContext): Try[Long] = Try {
    require(boxesToSpend.size == inputs.size, s"boxesToSpend.size ${boxesToSpend.size} != inputs.size ${inputs.size}")

    val lastUtxoDigest = AvlTreeData(blockchainState.digest, ErgoBox.BoxId.size)

    val txCost = boxesToSpend.zipWithIndex.foldLeft(0L) { case (accCost, (box, idx)) =>
      val input = inputs(idx)

      assert(box.id sameElements input.boxId)

      val proof = input.spendingProof

      val proverExtension = inputs(idx).spendingProof.extension

      val context =
        ErgoLikeContext(blockchainState.height, lastUtxoDigest, boxesToSpend, this, box, proverExtension)

      val scriptCost: Long = verifier.verify(box.proposition, context, proof, messageToSign) match {
        case Success((res, cost)) =>
          if (!res) {
            throw new Exception(s"Validation failed for input #$idx of tx ${toString}")
          } else {
            cost
          }
        case Failure(e) =>
          log.warn(s"Invalid transaction $toString: ", e)
          throw e
      }
      accCost + scriptCost
    }
    txCost
  }

  override type M = ErgoTransaction

  override def serializer: Serializer[ErgoTransaction] = ErgoTransactionSerializer

  override def toString: String = JsonEncoders.default.transactionEncoder(this).noSpaces
}


class ErgoTransactionEncoder(implicit val settings: ApiSettings) extends Encoder[ErgoTransaction] with ApiCodecs {

  implicit private val extensionEncoder: Encoder[ContextExtension] = { extension =>
    var byteLength = 0
    val jsonMap = extension.values.map { case (key, value) =>
      val serializedValue = serialize(value)
      byteLength += 1 /* key */ + serializedValue.length
      key.toString -> bytesEncoder(serializedValue)
    }
    jsonWithLength(byteLength, jsonMap)
  }

  def estimateInputBytes(input: Input, extensionJson: Json): Int = {
    ErgoBox.BoxId.size +
      2 /* extSize field */ +
      input.spendingProof.proofBytes.length +
      extractByteLength(extensionJson)
  }

  implicit private val inputEncoder: Encoder[Input] = { input =>
    val extJson = extensionEncoder(input.spendingProof.extension)
    jsonWithLength(estimateInputBytes(input, extJson),
      "boxId" -> input.boxId.asJson,
      "spendingProof" -> Json.obj(
        "proofBytes" -> byteSeqEncoder(input.spendingProof.proofBytes),
        "extension" -> extJson
      )
    )
  }

  implicit private val registersEncoder: Encoder[Map[NonMandatoryIdentifier, EvaluatedValue[_ <: SType]]] = { regsMap =>
    var byteLength = 0
    val jsonMap = regsMap.map {  case (key, value) =>
      val serializedValue = serialize(value)
      byteLength += 1 /* key */ + serializedValue.length
      s"R${key.number}" -> serializedValue.asJson
    }
    jsonWithLength(byteLength, jsonMap)
  }

  def estimateOutputBytes(output: ErgoBox, registersJson: Json): Int = {
    8 /* value */ +
    output.propositionBytes.length +
    extractByteLength(registersJson)
  }

  implicit private val outputEncoder: Encoder[ErgoBox] = { box =>
    val registersJson = registersEncoder(box.additionalRegisters)
    jsonWithLength(estimateOutputBytes(box, registersJson),
      "boxId" -> box.id.asJson,
      "value" -> box.value.asJson,
      "proposition" -> valueEncoder(box.proposition),
      "additionalRegisters" -> registersJson
    )
  }

  def apply(tx: ErgoTransaction): Json = {
    var byteLength: Seq[Eval[Int]] = Seq(Eval.now(32 /* id */ + 2 /* inputsCount field */ + 2 /* outputsCount field */))

    val inputsJson = tx.inputs.map { input =>
      val inputJson = input.asJson
      byteLength = byteLength :+ Eval.always(extractByteLength(inputJson))
      inputJson
    }.asJson

    val outputsJson = tx.outputs.map { output =>
      val outputJson = output.asJson
      byteLength = byteLength :+  Eval.always(extractByteLength(outputJson))
      outputJson
    }.asJson

    jsonWithLength(byteLength.map(_.value).sum,
      "id" -> tx.id.asJson,
      "inputs" -> inputsJson,
      "outputs" -> outputsJson
    )
  }

}

class ErgoTransactionDecoder(implicit val settings: ApiSettings)
  extends Decoder[ErgoTransaction] with ApiCodecs with ModifierValidator {

  implicit val proofDecoder: Decoder[SerializedProverResult] = { cursor =>
    for {
      proofBytes <- cursor.downField("proofBytes").as[Array[Byte]]
      extMap <- cursor.downField("extension").as[Map[Byte, EvaluatedValue[SType]]]
    } yield SerializedProverResult(proofBytes, ContextExtension(extMap))
  }

  implicit private val inputDecoder: Decoder[Input] = { cursor =>
    for {
      boxId <- cursor.downField("boxId").as[ADKey]
      proof <- cursor.downField("spendingProof").as[SerializedProverResult]
    } yield Input(boxId, proof)
  }

  implicit private val identifierDecoder: KeyDecoder[NonMandatoryIdentifier] = { key =>
    ErgoBox.registerByName.get(key).collect {
      case nonMandatoryId: NonMandatoryIdentifier => nonMandatoryId
    }
  }

  implicit private val outputDecoder: Decoder[(ErgoBoxCandidate, Option[BoxId])] = { cursor =>
    for {
      maybeId <- cursor.downField("boxId").as[Option[BoxId]]
      value <- cursor.downField("value").as[Long]
      proposition <- cursor.downField("proposition").as[Value[SBoolean.type]]
      registers <- cursor.downField("additionalRegisters").as[Map[NonMandatoryIdentifier, EvaluatedValue[SType]]]
    } yield (new ErgoBoxCandidate(value, proposition, registers), maybeId)
  }

  def apply(cursor: HCursor): Decoder.Result[ErgoTransaction] = {
    implicit val c = cursor
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
      .validate(maybeId.forall(_ sameElements tx.id)) {
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
          validationState.validate(boxId sameElements box.id) {
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
