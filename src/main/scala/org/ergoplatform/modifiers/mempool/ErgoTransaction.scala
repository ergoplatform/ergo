package org.ergoplatform.modifiers.mempool

import java.nio.ByteBuffer

import io.circe._
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId}
import org.ergoplatform.ErgoLikeContext.Metadata
import org.ergoplatform.ErgoLikeTransaction.{FlattenedTransaction, flattenedTxSerializer}
import org.ergoplatform._
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.nodeView.{ErgoContext, ErgoInterpreter, TransactionContext}
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.BoxUtils
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ValidationResult.fromValidationState
import scorex.core.validation.{ModifierValidator, ValidationResult}
import scorex.crypto.authds.ADKey
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigmastate.Values.{EvaluatedValue, Value}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.serialization.{Serializer => SSerializer}
import sigmastate.utils.{ByteBufferReader, ByteReader, ByteWriter}
import sigmastate.{SBoolean, SType}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class ErgoTransaction(override val inputs: IndexedSeq[Input],
                           override val outputCandidates: IndexedSeq[ErgoBoxCandidate],
                           override val sizeOpt: Option[Int] = None)
  extends Transaction
    with ErgoLikeTransactionTemplate[Input]
    with MempoolModifier
    with ErgoNodeViewModifier
    with ModifierValidator
    with ScorexLogging {

  override val serializedId: Array[Byte] = Algos.hash(messageToSign)

  override lazy val id: ModifierId = bytesToId(serializedId)

  /**
    * Fill a mutable map passed as a parameter with (assets -> total amount) data, based on boxes passed as
    * a parameter. That is, the method is checking amounts of assets in the boxes(i.e. that a box contains non-negative
    * amount for an asset) and then summarize and group their corresponding amounts.
    *
    * @param boxes - boxes to
    * @return map from asset id to to balance
    */
  private def getAssetsMap(boxes: IndexedSeq[ErgoBoxCandidate]): Try[scala.collection.Map[ByteArrayWrapper, Long]] = Try {
    val map: mutable.Map[ByteArrayWrapper, Long] = mutable.Map[ByteArrayWrapper, Long]()
    boxes.foreach { box =>
      require(box.additionalTokens.size <= ErgoBox.MaxTokens, "Output contains too many assets")
      box.additionalTokens.foreach { case (assetId, amount) =>
        require(amount >= 0, s"negative asset amount for ${Algos.encode(assetId)}")
        val aiWrapped = ByteArrayWrapper(assetId)
        val total = map.getOrElse(aiWrapped, 0L)
        map.put(aiWrapped, Math.addExact(total, amount))
        require(map.size <= ErgoTransaction.MaxTokens, s"Transaction is operating with too many(${map.size}) assets")
      }
    }
    map
  }

  lazy val outAssetsTry: Try[Map[ByteArrayWrapper, Long]] = getAssetsMap(outputCandidates).map(_.toMap)

  /**
    * statelessValidity is checking whether aspects of a transaction is valid which do not require the state to check.
    *
    * @return Success(Unit) if transaction is valid, Failure(e) if transaction is invalid, with respect to
    *         an error encapsulated in the exception "e".
    */
  def statelessValidity: Try[Unit] = validateStateless.toTry

  /** Stateless transaction validation with result returned as `ValidationResult`
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
      .demandSuccess(outAssetsTry, s"Asset rules violated $outAssetsTry in $this")
      .result
  }

  /** Return total computation cost
    */
  def statefulValidity(boxesToSpend: IndexedSeq[ErgoBox],
                       blockchainState: ErgoStateContext,
                       metadata: Metadata): Try[Long] = {
    lazy val inputSum = Try(boxesToSpend.map(_.value).reduce(Math.addExact(_, _)))
    lazy val outputSum = Try(outputCandidates.map(_.value).reduce(Math.addExact(_, _)))

    failFast
      .payload(0L)
      .demand(outputs.forall(o => o.value >= BoxUtils.minimalErgoAmount(o, blockchainState.currentParameters)), s"Transaction is trying to create dust: $this")
      .demand(outputCandidates.forall(_.creationHeight <= blockchainState.currentHeight), s"Box created in future:  ${outputCandidates.map(_.creationHeight)} vs ${blockchainState.currentHeight}")
      .demand(boxesToSpend.size == inputs.size, s"boxesToSpend.size ${boxesToSpend.size} != inputs.size ${inputs.size}")
      .validateSeq(boxesToSpend.zipWithIndex) { case (validation, (box, idx)) =>
        val input = inputs(idx)
        val proof = input.spendingProof
        val proverExtension = proof.extension
        val transactionContext = TransactionContext(boxesToSpend, this, idx.toShort)
        val ctx = new ErgoContext(blockchainState, transactionContext, metadata, proverExtension)

        //todo: reuse the interpreter?
        val verifier: ErgoInterpreter = new ErgoInterpreter(blockchainState.currentParameters)

        val costTry = verifier.verify(box.proposition, ctx, proof, messageToSign)
        costTry.recover { case t => t.printStackTrace() }

        lazy val (isCostValid, scriptCost) = costTry.getOrElse((false, 0L))
        validation
          .demandEqualArrays(box.id, input.boxId, "Box id doesn't match input")
          .demandSuccess(costTry, s"Invalid transaction $this")
          .demand(isCostValid, s"Input script verification failed for input #$idx ($box) of tx $this: $costTry")
          .map(_ + scriptCost)
      }
      .demandSuccess(inputSum, s"Overflow in inputs in $this")
      .demandSuccess(outputSum, s"Overflow in outputs in $this")
      .demand(inputSum == outputSum, s"Ergo token preservation is broken in $this")
      .demandTry(outAssetsTry, outAssetsTry.toString) { (validation, outAssets) =>
        getAssetsMap(boxesToSpend) match {
          case Success(inAssets) =>
            lazy val newAssetId = ByteArrayWrapper(inputs.head.boxId)
            validation.validateSeq(outAssets) {
              case (validation, (outAssetId, outAmount)) =>
                val inAmount: Long = inAssets.getOrElse(outAssetId, -1L)
                validation.validate(inAmount >= outAmount || (outAssetId == newAssetId && outAmount > 0)) {
                  fatal(s"Assets preservation rule is broken in $this. " +
                    s"Amount in: $inAmount, out: $outAmount, Allowed new asset: $newAssetId out: $outAssetId")
                }
            }
          case Failure(e) => fatal(e.getMessage)
        }
      }.toTry
  }

  override type M = ErgoTransaction

  override def serializer: Serializer[ErgoTransaction] = ErgoTransactionSerializer

  override def toString: String = {
    import ErgoTransaction._
    val displayMaxObjects = 3
    val inputsStr = if (inputs.size > displayMaxObjects) {
      inputs.take(displayMaxObjects).asJson.noSpaces + s" ... (${inputs.size})"
    } else {
      inputs.asJson.noSpaces
    }
    val outputsStr = if (outputs.size > displayMaxObjects) {
      outputs.take(displayMaxObjects).asJson.noSpaces + s" ... (${outputs.size})"
    } else {
      outputs.asJson.noSpaces
    }
    s"ErgoTransaction(id: $encodedId, inputs: $inputsStr, outputs: $outputsStr, size: $size)"
  }
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
      creationHeight <- cursor.downField("creationHeight").as[Long]
      proposition <- cursor.downField("proposition").as[Value[SBoolean.type]]
      assets <- cursor.downField("assets").as[Seq[(ErgoBox.TokenId, Long)]]
      registers <- cursor.downField("additionalRegisters").as[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]
    } yield (new ErgoBoxCandidate(value, proposition, creationHeight, assets, registers), maybeId)
  }

  implicit val transactionEncoder: Encoder[ErgoTransaction] = { tx =>
    Json.obj(
      "id" -> tx.id.asJson,
      "inputs" -> tx.inputs.asJson,
      "outputs" -> tx.outputs.asJson,
      "size" -> tx.size.asJson
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
            val box = candidate.toBox(txId, index.toShort)
            validation.demandEqualArrays(boxId, box.id, s"Bad identifier for Ergo box. It could also be skipped")
          }
      }
    }.result(outputs.map(_._1)).toDecoderResult
  }
}

object ErgoTransactionSerializer extends Serializer[ErgoTransaction] with SSerializer[ErgoTransaction, ErgoTransaction] {
  override def serializeBody(tx: ErgoTransaction, w: ByteWriter): Unit =
    flattenedTxSerializer.serializeBody(FlattenedTransaction(tx.inputs.toArray, tx.outputCandidates.toArray), w)

  override def parseBody(r: ByteReader): ErgoTransaction = {
    val ftx = flattenedTxSerializer.parseBody(r)
    ErgoTransaction(ftx.inputs, ftx.outputCandidates)
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoTransaction] = Try {
    val buf = ByteBuffer.wrap(bytes)
    buf.position(0)
    parseBody(new ByteBufferReader(buf))
  }
}
