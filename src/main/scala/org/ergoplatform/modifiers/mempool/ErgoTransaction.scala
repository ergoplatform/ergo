package org.ergoplatform.modifiers.mempool

import io.circe._
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId}
import org.ergoplatform.ErgoLikeTransaction.flattenedTxSerializer
import org.ergoplatform.ErgoTransactionValidator.verifier
import org.ergoplatform._
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.settings.Algos
import scorex.core._
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.core.utils.{ScorexEncoding, ScorexLogging}
import scorex.core.validation.ValidationResult.fromValidationState
import scorex.core.validation.{ModifierValidator, ValidationResult}
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{EvaluatedValue, Value}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.serialization.Serializer.{Consumed, Position}
import sigmastate.serialization.{Serializer => SSerializer}
import sigmastate.{AvlTreeData, SBoolean, SType}

import scala.collection.mutable
import scala.util.Try


case class ErgoTransaction(override val inputs: IndexedSeq[Input],
                           override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends Transaction with ErgoLikeTransactionTemplate[Input] with MempoolModifier with ErgoNodeViewModifier
    with ModifierValidator with ScorexLogging {

  override val serializedId: Array[Byte] = Blake2b256.hash(messageToSign)

  override lazy val id: ModifierId = bytesToId(serializedId)

  /**
    * Fill a mutable map passed as a parameter with (assets -> total amount) data, based on boxes passed as
    * a parameter. That is, the method is checking amounts of assets in the boxes(i.e. that a box contains non-negative
    * amount for an asset) and then summarize and group their corresponding amounts.
    *
    * @param boxes - boxes to
    * @param map   - map to modify
    * @return
    */
  private def fillAssetsMap(boxes: IndexedSeq[ErgoBoxCandidate],
                            map: mutable.Map[ByteArrayWrapper, Long]) = Try {
    boxes.foreach { box =>
      require(box.additionalTokens.size <= ErgoBox.MaxTokens, "Output contains too many assets")
      box.additionalTokens.foreach { case (assetId, amount) =>
        require(amount >= 0, s"negative asset amount for ${Algos.encode(assetId)}")
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
  def validateStateless: ValidationResult[Unit] = {
    failFast
      .demand(outputCandidates.nonEmpty, s"No outputs in transaction $this")
      .demand(inputs.nonEmpty, s"No inputs in transaction $this")
      .demand(inputs.size <= Short.MaxValue, s"Too many inputs in transaction $this")
      .demand(outputCandidates.size <= Short.MaxValue, s"Too many outputCandidates in transaction $this")
      .demand(outputCandidates.forall(_.value >= 0), s"Transaction has an output with negative amount $this")
      .demandNoThrow(outputCandidates.map(_.value).reduce(Math.addExact(_, _)), s"Overflow in outputs in $this")
      .demandSuccess(outAssetsOpt, s"Asset rules violated in $this")
      .result
  }

  /**
    * @return total computation cost
    */
  def statefulValidity(boxesToSpend: IndexedSeq[ErgoBox], blockchainState: ErgoStateContext): Try[Long] = {
    lazy val lastUtxoDigest = AvlTreeData(blockchainState.digest, ErgoBox.BoxId.size)
    lazy val inputSum = Try(boxesToSpend.map(_.value).reduce(Math.addExact(_, _)))
    lazy val outputSum = Try(outputCandidates.map(_.value).reduce(Math.addExact(_, _)))

    failFast
      .payload(0L)
      .demand(boxesToSpend.size == inputs.size, s"boxesToSpend.size ${boxesToSpend.size} != inputs.size ${inputs.size}")
      .validateSeq(boxesToSpend.zipWithIndex) { case (validation, (box, idx)) =>
        val input = inputs(idx)
        val proof = input.spendingProof
        val proverExtension = proof.extension

        def ctx = ErgoLikeContext(blockchainState.height, lastUtxoDigest, boxesToSpend, this, box, proverExtension)

        lazy val costTry = verifier.verify(box.proposition, ctx, proof, messageToSign)
        lazy val (isCostValid, scriptCost) = costTry.getOrElse((false, 0L))
        validation
          .demandEqualArrays(box.id, input.boxId, "Box id doesn't match input")
          .demandSuccess(costTry, s"Invalid transaction $this")
          .demand(isCostValid, s"Validation failed for input #$idx of tx $this")
          .map(_ + scriptCost)
      }
      .demandSuccess(inputSum, s"Overflow in inputs in $this")
      .demandSuccess(outputSum, s"Overflow in outputs in $this")
      .demand(inputSum == outputSum, s"Ergo token preservation is broken in $this")
      .demandTry(outAssetsOpt, s"Assets preservation rule is broken in $this") { (validation, outAssets) =>
        val inAssets = mutable.Map[ByteArrayWrapper, Long]()
        fillAssetsMap(boxesToSpend, inAssets)
        lazy val newAssetId = ByteArrayWrapper(inputs.head.boxId)
        validation.validateSeq(outAssets) {
          case (validation, (outAssetId, outAmount)) =>
            val inAmount: Long = inAssets.remove(outAssetId).getOrElse(-1L)
            validation
              .validate(inAmount == outAmount || (outAssetId == newAssetId && outAmount > 0)) {
                fatal(s"Assets preservation rule is broken in $this. " +
                  s"Amount in: $inAmount, out: $outAmount, Asset in: $newAssetId out: $outAssetId")
              }
        }
      }
      .toTry
  }

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


  implicit val identifierDecoder: KeyDecoder[NonMandatoryRegisterId] = { key =>
    ErgoBox.registerByName.get(key).collect {
      case nonMandatoryId: NonMandatoryRegisterId => nonMandatoryId
    }
  }

  implicit val assetDecoder: Decoder[(ErgoBox.TokenId, Long)] = { cursor =>
    for {
      tokenId <- cursor.downField("tokenId").as[ErgoBox.TokenId]
      amount <- cursor.downField("amount").as[Long]
    } yield (tokenId, amount)
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
      outputsWithIndex <- cursor.downField("outputs").as[IndexedSeq[(ErgoBoxCandidate, Option[BoxId])]]
      outputs <- validateOutputs(outputsWithIndex, maybeId)
      result <- validateTransaction(ErgoTransaction(inputs, outputs), maybeId)
    } yield result
  }

  def validateTransaction(tx: ErgoTransaction, txId: Option[ModifierId])
                         (implicit cursor: ACursor): Decoder.Result[ErgoTransaction] = {
    accumulateErrors
      .validateOrSkip(txId) { (validation, id) =>
        validation.demandEqualIds(id, tx.id, "Bad identifier for Ergo transaction. It could also be skipped")
      }
      .validate(tx.validateStateless)
      .result(tx)
      .toDecoderResult
  }

  def validateOutputs(outputs: IndexedSeq[(ErgoBoxCandidate, Option[BoxId])], maybeTxId: Option[ModifierId])
                     (implicit cursor: ACursor): Decoder.Result[IndexedSeq[ErgoBoxCandidate]] = {
    accumulateErrors.validateOrSkip(maybeTxId) { (validation, txId) =>
      validation.validateSeq(outputs.zipWithIndex) {
        case (validation, ((candidate, maybeId), index)) =>
          validation.validateOrSkip(maybeId) { (validation, boxId) =>
            // todo move ErgoBoxCandidate from sigmastate to Ergo and use ModifierId as a type of txId
            val box = candidate.toBox(idToBytes(txId), index.toShort)
            validation.demandEqualArrays(boxId, box.id, "Bad identifier for Ergo box. It could also be skipped")
          }
      }
    }
      .result(outputs.map(_._1))
      .toDecoderResult
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
