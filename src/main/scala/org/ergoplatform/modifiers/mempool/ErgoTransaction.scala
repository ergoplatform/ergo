package org.ergoplatform.modifiers.mempool

import io.circe._
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId}
import org.ergoplatform._
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.settings.{Algos, ErgoValidationSettings, ValidationRules}
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.utils.BoxUtils
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.wallet.protocol.context.TransactionContext
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.Transaction
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ValidationResult.fromValidationState
import scorex.core.validation.{ModifierValidator, ValidationSettings, ValidationState}
import scorex.crypto.authds.ADKey
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigmastate.SType
import sigmastate.Values.{ErgoTree, EvaluatedValue}
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.serialization.ConstantStore
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * ErgoTransaction is an atomic state transition operation. It destroys Boxes from the state
  * and creates new ones. If transaction is spending boxes protected by some non-trivial scripts,
  * its inputs should also contain proof of spending correctness - context extension (user-defined
  * key-value map) and data inputs (links to existing boxes in the state) that may be used during
  * script reduction to crypto, signatures that satisfies the remaining cryptographic protection
  * of the script.
  * Transactions are not encrypted, so it is possible to browse and view every transaction ever
  * collected into a block.
  *
  * @param inputs           - inputs, that will be spent by this transaction.
  * @param dataInputs       - inputs, that are not going to be spent by transaction, but will be
  *                         reachable from inputs scripts. `dataInputs` scripts will not be executed,
  *                         thus their scripts costs are not included in transaction cost and
  *                         they do not contain spending proofs.
  * @param outputCandidates - box candidates to be created by this transaction.
  *                         Differ from ordinary ones in that they do not include transaction id and index
  */
case class ErgoTransaction(override val inputs: IndexedSeq[Input],
                           override val dataInputs: IndexedSeq[DataInput],
                           override val outputCandidates: IndexedSeq[ErgoBoxCandidate],
                           override val sizeOpt: Option[Int] = None)
  extends ErgoLikeTransaction(inputs, dataInputs, outputCandidates)
    with Transaction
    with MempoolModifier
    with ErgoNodeViewModifier
    with ScorexLogging {

  override val serializedId: Array[Byte] = Algos.hash(messageToSign)

  override lazy val id: ModifierId = bytesToId(serializedId)

  /**
    * Extracts a mapping (assets -> total amount) from a set of boxes passed as a parameter.
    * That is, the method is checking amounts of assets in the boxes(i.e. that a box contains positive
    * amount for an asset) and then summarize and group their corresponding amounts.
    *
    * @param boxes - boxes to check and extract assets from
    * @return a mapping from asset id to to balance and total assets number
    */
  private def extractAssets(boxes: IndexedSeq[ErgoBoxCandidate]): Try[(Map[ByteArrayWrapper, Long], Int)] = Try {
    val map: mutable.Map[ByteArrayWrapper, Long] = mutable.Map[ByteArrayWrapper, Long]()
    val assetsNum = boxes.foldLeft(0) { case (acc, box) =>
      require(box.additionalTokens.length <= ErgoTransaction.MaxAssetsPerBox, "too many assets in one box")
      box.additionalTokens.foreach { case (assetId, amount) =>
        val aiWrapped = ByteArrayWrapper(assetId)
        val total = map.getOrElse(aiWrapped, 0L)
        map.put(aiWrapped, Math.addExact(total, amount))
      }
      acc + box.additionalTokens.size
    }
    map.toMap -> assetsNum
  }

  lazy val outAssetsTry: Try[(Map[ByteArrayWrapper, Long], Int)] = extractAssets(outputCandidates)

  lazy val outputsSumTry: Try[Long] = Try(outputCandidates.map(_.value).reduce(Math.addExact(_, _)))

  /**
    * Same as `validateStateless`, but result is returned as Try[Long]
    **/
  def statelessValidity: Try[Unit] = validateStateless.result.toTry

  /**
    * Same as `validateStateful`, but result is returned as Try[Long]
    **/
  def statefulValidity(boxesToSpend: IndexedSeq[ErgoBox],
                       dataBoxes: IndexedSeq[ErgoBox],
                       stateContext: ErgoStateContext,
                       accumulatedCost: Long = 0L)
                      (implicit verifier: ErgoInterpreter, vs: ValidationSettings): Try[Long] = {
    validateStateful(boxesToSpend, dataBoxes, stateContext, accumulatedCost).result.toTry
  }

  /**
    * Stateless transaction validation with result returned as `ValidationResult`
    * to accumulate further validation results
    *
    * @note Consensus-critical!
    */
  def validateStateless: ValidationState[Unit] = {
    ModifierValidator(ErgoValidationSettings.initial)
      .validate(txNoInputs, inputs.nonEmpty, s"$id")
      .validate(txNoOutputs, outputCandidates.nonEmpty, s"$id")
      .validate(txManyInputs, inputs.size <= Short.MaxValue, s"$id: ${inputs.size}")
      .validate(txManyDataInputs, dataInputs.size <= Short.MaxValue, s"$id: ${dataInputs.size}")
      .validate(txManyOutputs, outputCandidates.size <= Short.MaxValue, s"$id: ${outputCandidates.size}")
      .validate(txNegativeOutput, outputCandidates.forall(_.value >= 0), s"$id: ${outputCandidates.map(_.value)}")
      .validateNoFailure(txOutputSum, outputsSumTry)
      .validate(txInputsUnique, inputs.distinct.size == inputs.size, s"$id: ${inputs.distinct.size} == ${inputs.size}")
  }

  /**
    * Checks whether transaction is valid against input boxes to spend, and
    * non-spendable data inputs.
    *
    * Note that this method make only checks which are possible when input boxes are available.
    *
    * To make full transaction validation, use (tx.statelessValidity && tx.statefulValidity(...))
    *
    * @note Consensus-critical!
    * @param boxesToSpend    - boxes the transaction spends (via inputs)
    * @param dataBoxes       - boxes the transaction only reads (via data inputs)
    * @param stateContext    - blockchain context at the moment of validation
    * @param accumulatedCost - computational cost before validation, validation starts with this value
    * @param verifier        - interpreter used to check spending correctness for transaction inputs
    * @return total computation cost
    */
  def validateStateful(boxesToSpend: IndexedSeq[ErgoBox],
                       dataBoxes: IndexedSeq[ErgoBox],
                       stateContext: ErgoStateContext,
                       accumulatedCost: Long)
                      (implicit verifier: ErgoInterpreter, vs: ValidationSettings): ValidationState[Long] = {
    verifier.IR.resetContext() // ensure there is no garbage in the IRContext
    lazy val inputSumTry = Try(boxesToSpend.map(_.value).reduce(Math.addExact(_, _)))

    val initialCost: Long =
      boxesToSpend.size * stateContext.currentParameters.inputCost +
        dataBoxes.size * stateContext.currentParameters.dataInputCost +
        outputCandidates.size * stateContext.currentParameters.outputCost

    // Maximum transaction cost the validation procedure could tolerate
    val maxCost = verifier.maxCost - accumulatedCost

    ModifierValidator(vs)
      // Check that the transaction is not too big
      .validate(txCost, initialCost < maxCost, s"$id: initial cost")
      // Starting validation
      .payload(initialCost)
      // Perform cheap checks first
      .validateNoFailure(txAssetsInOneBox, outAssetsTry)
      .validate(txPositiveAssets, outputCandidates.forall(_.additionalTokens.forall(_._2 > 0)), s"$id: ${outputCandidates.map(_.additionalTokens)}")
      // Check that outputs are not dust, and not created in future
      .validateSeq(outputs) { case (validationState, out) =>
      validationState
        .validate(txDust, out.value >= BoxUtils.minimalErgoAmount(out, stateContext.currentParameters), s"$id: output $out")
        .validate(txFuture, out.creationHeight <= stateContext.currentHeight, s"$id: output $out")
    }
      // Just to be sure, check that all the input boxes to spend (and to read) are presented.
      // Normally, this check should always pass, if the client is implemented properly
      // so it is not part of the protocol really.
      .validate(txBoxesToSpend, boxesToSpend.size == inputs.size, s"$id: ${boxesToSpend.size} == ${inputs.size}")
      .validate(txDataBoxes, dataBoxes.size == dataInputs.size, s"$id: ${dataBoxes.size} == ${dataInputs.size}")
      // Check that there are no overflow in input and output values
      .validate(txInputsSum, inputSumTry.isSuccess, s"$id")
      // Check that transaction is not creating money out of thin air.
      .validate(txErgPreservation, inputSumTry == outputsSumTry, s"$id: $inputSumTry == $outputsSumTry")
      .validateTry(outAssetsTry, e => ModifierValidator.fatal("Incorrect assets", e)) { case (validation, (outAssets, outAssetsNum)) =>
      extractAssets(boxesToSpend) match {
        case Success((inAssets, inAssetsNum)) =>
          lazy val newAssetId = ByteArrayWrapper(inputs.head.boxId)
          val tokenAccessCost = stateContext.currentParameters.tokenAccessCost
          val totalAssetsAccessCost = (outAssetsNum + inAssetsNum) * tokenAccessCost +
            (inAssets.size + outAssets.size) * tokenAccessCost

          validation
            // Check that transaction is not too costly considering all the assets
            .validate(txCost, initialCost + totalAssetsAccessCost < maxCost, s"$id: assets cost")
            .validateSeq(outAssets) {
              case (validationState, (outAssetId, outAmount)) =>
                val inAmount: Long = inAssets.getOrElse(outAssetId, -1L)

                // Check that for each asset output amount is no more than input amount,
                // with a possible exception for a new asset created by the transaction
                validationState.validate(txAssetsPreservation,
                  inAmount >= outAmount || (outAssetId == newAssetId && outAmount > 0),
                  s"$id: Amount in = $inAmount, out = $outAmount. Allowed new asset = $newAssetId, out = $outAssetId")
            }
            .map(_ + totalAssetsAccessCost)
        case Failure(e) =>
          // should never be here as far as we've already checked this when we've created the box
          ModifierValidator.fatal(e.getMessage)
      }
    }
      // Check inputs, the most expensive check usually, so done last.
      .validateSeq(boxesToSpend.zipWithIndex) { case (validation, (box, idx)) =>

      val input = inputs(idx)
      val proof = input.spendingProof
      val proverExtension = proof.extension
      val transactionContext = TransactionContext(boxesToSpend, dataBoxes, this, idx.toShort)
      val ctx = new ErgoContext(stateContext, transactionContext, proverExtension)

      val costTry = verifier.verify(box.ergoTree, ctx, proof, messageToSign)
      costTry.recover { case t => t.printStackTrace() }

      lazy val (isCostValid, scriptCost) = costTry.getOrElse((false, 0L))

      val currentTxCost = validation.result.payload.get

      validation
        // Just in case, should always be true if client implementation is correct.
        .validateEquals(txBoxToSpend, box.id, input.boxId)
        // Check whether input box script interpreter raised exception
        .validate(txScriptValidation, costTry.isSuccess && isCostValid, s"$id: #$idx => $costTry")
        // Check that cost of the transaction after checking the input becomes too big
        .validate(txCost, currentTxCost + scriptCost <= maxCost, s"$id: cost exceeds limit after input #$idx")
        .map(_ + scriptCost)
    }
  }

  override type M = ErgoTransaction

  override def serializer: ScorexSerializer[ErgoTransaction] = ErgoTransactionSerializer

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

object ErgoTransaction extends ApiCodecs with ScorexLogging with ScorexEncoding {

  def apply(inputs: IndexedSeq[Input], outputCandidates: IndexedSeq[ErgoBoxCandidate]): ErgoTransaction = {
    ErgoTransaction(inputs, IndexedSeq(), outputCandidates, None)
  }

  def apply(tx: ErgoLikeTransaction): ErgoTransaction =
    ErgoTransaction(tx.inputs, tx.dataInputs, tx.outputCandidates)

  val MaxAssetsPerBox = 255

  implicit private val extensionEncoder: Encoder[ContextExtension] = { extension =>
    extension.values.map { case (key, value) =>
      key -> evaluatedValueEncoder(value)
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

  implicit private val dataInputEncoder: Encoder[DataInput] = { input =>
    Json.obj(
      "boxId" -> input.boxId.asJson,
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

  implicit private val dataInputDecoder: Decoder[DataInput] = { cursor =>
    for {
      boxId <- cursor.downField("boxId").as[ADKey]
    } yield DataInput(boxId)
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
      creationHeight <- cursor.downField("creationHeight").as[Int]
      ergoTree <- cursor.downField("ergoTree").as[ErgoTree]
      assets <- cursor.downField("assets").as[Seq[(ErgoBox.TokenId, Long)]] // TODO optimize: encode directly into Coll avoiding allocation of Tuple2 for each element
      registers <- cursor.downField("additionalRegisters").as[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]
    } yield (new ErgoBoxCandidate(value, ergoTree, creationHeight, assets.toColl, registers), maybeId)
  }

  implicit val transactionEncoder: Encoder[ErgoTransaction] = { tx =>
    Json.obj(
      "id" -> tx.id.asJson,
      "inputs" -> tx.inputs.asJson,
      "dataInputs" -> tx.dataInputs.asJson,
      "outputs" -> tx.outputs.asJson,
      "size" -> tx.size.asJson
    )
  }

  implicit val transactionDecoder: Decoder[ErgoTransaction] = { implicit cursor =>
    for {
      inputs <- cursor.downField("inputs").as[IndexedSeq[Input]]
      dataInputs <- cursor.downField("dataInputs").as[IndexedSeq[DataInput]]
      outputsWithIndex <- cursor.downField("outputs").as[IndexedSeq[(ErgoBoxCandidate, Option[BoxId])]]
    } yield new ErgoTransaction(inputs, dataInputs, outputsWithIndex.map(_._1))
  }

}

object ErgoTransactionSerializer extends ScorexSerializer[ErgoTransaction] {

  override def serialize(tx: ErgoTransaction, w: Writer): Unit = {
    val elt = new ErgoLikeTransaction(tx.inputs, tx.dataInputs, tx.outputCandidates)
    ErgoLikeTransactionSerializer.serialize(elt, new SigmaByteWriter(w, None))
  }

  override def parse(r: Reader): ErgoTransaction = {
    val reader = new SigmaByteReader(r, new ConstantStore(), resolvePlaceholdersToConstants = false)
    val elt = ErgoLikeTransactionSerializer.parse(reader)
    ErgoTransaction(elt.inputs, elt.dataInputs, elt.outputCandidates)
  }
}
