package org.ergoplatform.modifiers.mempool

import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox.NonMandatoryIdentifier
import org.ergoplatform.ErgoLikeTransaction.flattenedTxSerializer
import org.ergoplatform.ErgoTransactionValidator.verifier
import org.ergoplatform._
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.settings.Algos
import scorex.core.ModifierId
import scorex.core.serialization.Serializer
import sigmastate.serialization.{Serializer => SSerializer}
import scorex.core.transaction.Transaction
import scorex.core.utils.ScorexLogging
import scorex.core.validation.ModifierValidator
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{EvaluatedValue, Value}
import sigmastate.interpreter.{ContextExtension, SerializedProverResult}
import sigmastate.{AvlTreeData, SBoolean, SType}
import sigmastate.serialization.Serializer.{Consumed, Position}

import scala.util.{Failure, Success, Try}


case class ErgoStateContext(height: Long, lastUtxoDigest: ADDigest)

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
  def statelessValidity: Try[Unit] = {
    accumulateErrors
      .validate(inputs.nonEmpty) {
        fatal( s"No inputs in transaction $toString")
      }
      .validate(inputs.size <= Short.MaxValue) {
        fatal(s"Too many inputs in transaction $toString")
      }
      .validate(outputCandidates.size <= Short.MaxValue) {
        fatal(s"Too many outputCandidates in transaction $toString")
      }
      .result.toTry
    }

  /**
    * @return total coimputation cost
    */
  def statefulValidity(boxesToSpend: IndexedSeq[ErgoBox], blockchainState: ErgoStateContext): Try[Long] = Try {
    require(boxesToSpend.size == inputs.size, s"boxesToSpend.size ${boxesToSpend.size} != inputs.size ${inputs.size}")

    val lastUtxoDigest = AvlTreeData(blockchainState.lastUtxoDigest, ErgoBox.BoxId.size)

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
            throw new Exception(s"Validation failed for input #$idx")
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
}


object ErgoTransaction extends ApiCodecs with ModifierValidator {

  implicit private val extensionEncoder: Encoder[ContextExtension] = { extension =>
    extension.values.map { case (key, value) =>
      key -> valueEncoder(value)
    }.asJson
  }

  implicit private val inputEncoder: Encoder[Input] = { input =>
    Json.obj(
      "boxId" -> input.boxId.asJson,
      "spendingProof" -> Json.obj(
        "proofBytes" -> byteSeqEncoder(input.spendingProof.proofBytes),
        "extension" -> extensionEncoder(input.spendingProof.extension)
      )
    )
  }

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

  implicit private val registersEncoder: Encoder[Map[NonMandatoryIdentifier, EvaluatedValue[_ <: SType]]] = {
    _.map {  case (key, value) =>
      key.number -> valueEncoder(value)
    }.asJson
  }

  implicit private val outputEncoder: Encoder[ErgoBoxCandidate] = { box =>
    Json.obj(
      "value" -> box.value.asJson,
      "proposition" -> valueEncoder(box.proposition),
      "additionalRegisters" -> registersEncoder(box.additionalRegisters)
    )
  }

  implicit private val identifierDecoder: KeyDecoder[NonMandatoryIdentifier] = { key =>
    Try(ErgoBox.findRegisterByIndex(key.toByte).map(_.asInstanceOf[NonMandatoryIdentifier])).toOption.flatten
  }

  implicit private val outputDecoder: Decoder[ErgoBoxCandidate] = { cursor =>
    for {
      value <- cursor.downField("value").as[Long]
      proposition <- cursor.downField("proposition").as[Value[SBoolean.type]]
      registers <- cursor.downField("additionalRegisters").as[Map[NonMandatoryIdentifier, EvaluatedValue[SType]]]
    } yield new ErgoBoxCandidate(value, proposition, registers)
  }

  implicit val transactionEncoder: Encoder[ErgoTransaction] = { tx =>
    Json.obj(
      "id" -> tx.id.asJson,
      "inputs" -> tx.inputs.asJson,
      "outputs" -> tx.outputCandidates.asJson
    )
  }

  implicit val transactionDecoder: Decoder[ErgoTransaction] = { implicit cursor =>
    for {
      id <- cursor.downField("id").as[Option[ModifierId]]
      inputs <- cursor.downField("inputs").as[IndexedSeq[Input]]
      outputs <- cursor.downField("outputs").as[IndexedSeq[ErgoBoxCandidate]]
      result <- validateId(ErgoTransaction(inputs, outputs), id)
    } yield result
  }

  def validateId(tx: ErgoTransaction, maybeId: Option[ModifierId])
                (implicit cursor: ACursor): Decoder.Result[ErgoTransaction] = {
    fromValidation(tx) {
      accumulateErrors.validate(maybeId.forall(_ sameElements tx.id)) {
        fatal(s"Bad identifier ${Algos.encode(maybeId.get)} for ergo transaction. " +
              s"Identifier could be skipped, or should be ${Algos.encode(tx.id)}")
      }.result
    }
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
