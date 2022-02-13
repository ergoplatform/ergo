package org.ergoplatform.modifiers.mempool

import io.circe.syntax._
import org.ergoplatform.SigmaConstants.{MaxBoxSize, MaxPropositionBytes}
import org.ergoplatform._
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.utils.ArithUtils._
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings.{Algos, ErgoValidationSettings}
import org.ergoplatform.utils.BoxUtils
import org.ergoplatform.wallet.boxes.ErgoBoxAssetExtractor
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import scorex.core.EphemerealNodeViewModifier
import org.ergoplatform.wallet.protocol.context.{InputContext, TransactionContext}
import org.ergoplatform.wallet.serialization.JsonCodecsWrapper
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.Transaction
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ValidationResult.fromValidationState
import scorex.core.validation.{ModifierValidator, ValidationResult, ValidationState}
import scorex.db.ByteArrayUtils
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigmastate.serialization.ConstantStore
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.CostTable

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
    with EphemerealNodeViewModifier
    with ErgoNodeViewModifier
    with ScorexLogging {

  override val serializedId: Array[Byte] = Algos.hash(messageToSign)

  override lazy val id: ModifierId = bytesToId(serializedId)

  /**
    * Id of transaction "witness" (taken from Bitcoin jargon, means commitment to signatures of a transaction).
    * Id is 248-bit long, to distinguish transaction ids from witness ids in Merkle tree of transactions,
    * where both kinds of ids are written into leafs of the tree.
    */
  lazy val witnessSerializedId: Array[Byte] =
    Algos.hash(ByteArrayUtils.mergeByteArrays(inputs.map(_.spendingProof.proof))).tail


  lazy val outAssetsTry: Try[(Map[Seq[Byte], Long], Int)] = ErgoBoxAssetExtractor.extractAssets(outputCandidates)

  lazy val outputsSumTry: Try[Long] = Try(outputCandidates.map(_.value).reduce(Math.addExact(_, _)))

  /**
    * Stateless transaction validation with result returned as `ValidationResult`
    * to accumulate further validation results
    *
    * @note Consensus-critical!
    */
  def validateStateless(): ValidationState[Unit] = {
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
    * Same as `validateStateless`, but result is returned as Try[Unit]
    **/
  def statelessValidity(): Try[Unit] = {
    validateStateless().result.toTry
  }

  private def verifyInput(validationBefore: ValidationState[Long],
                          boxesToSpend: IndexedSeq[ErgoBox],
                          dataBoxes: IndexedSeq[ErgoBox],
                          box: ErgoBox,
                          inputIndex: Short,
                          stateContext: ErgoStateContext,
                          currentTxCost: Long)
                         (implicit verifier: ErgoInterpreter): ValidationResult[Long] = {

    // Cost limit per block
    val maxCost = stateContext.currentParameters.maxBlockCost

    val input = inputs(inputIndex)

    // Just in case, should always be true if client implementation is correct.
    if(!box.id.sameElements(input.boxId)) {
      log.error("Critical client error: box is not inputs(inputIndex)")
    }

    val proof = input.spendingProof
    val transactionContext = TransactionContext(boxesToSpend, dataBoxes, this)
    val inputContext = InputContext(inputIndex, proof.extension)

    val ctx = new ErgoContext(
      stateContext, transactionContext, inputContext,
      costLimit = maxCost - currentTxCost, // remaining cost so far
      initCost = 0)

    val costTry = verifier.verify(box.ergoTree, ctx, proof, messageToSign)
    val (isCostValid, scriptCost) =
      costTry match {
        case Failure(t) =>
          log.warn(s"Tx verification failed: ${t.getMessage}")
          log.warn(s"Tx $id verification context: " +
            s"${JsonCodecsWrapper.ergoLikeContextEncoder.apply(ctx)} " +
            s"input context: $inputContext " +
            s"proof: $proof" +
            s"messageToSign: $messageToSign")
          (false, maxCost + 1)
        case Success(result) =>
          result
      }

    val currCost = addExact(currentTxCost, scriptCost)

    validationBefore
      // Check whether input box script interpreter raised exception
      .validate(txScriptValidation, costTry.isSuccess && isCostValid, s"$id: #$inputIndex => $costTry")
      // Check that cost of the transaction after checking the input becomes too big
      .validate(bsBlockTransactionsCost, currCost <= maxCost, s"$id: cost exceeds limit after input #$inputIndex")
      .map(c => addExact(c, scriptCost))
  }

  private def verifyOutput(validationBefore: ValidationState[Long],
                           out: ErgoBox,
                           stateContext: ErgoStateContext): ValidationResult[Long] = {

    val blockVersion = stateContext.blockVersion

    validationBefore
      .validate(txDust, out.value >= BoxUtils.minimalErgoAmount(out, stateContext.currentParameters), s"$id, output ${Algos.encode(out.id)}, ${out.value} >= ${BoxUtils.minimalErgoAmount(out, stateContext.currentParameters)}")
      .validate(txFuture, out.creationHeight <= stateContext.currentHeight, s" ${out.creationHeight} <= ${stateContext.currentHeight} is not true, output id: $id: output $out")
      .validate(txNegHeight, (blockVersion == 1) || out.creationHeight >= 0, s" ${out.creationHeight} >= 0 is not true, output id: $id: output $out")
      .validate(txBoxSize, out.bytes.length <= MaxBoxSize.value, s"$id: output $out")
      .validate(txBoxPropositionSize, out.propositionBytes.length <= MaxPropositionBytes.value, s"$id: output $out")
  }

  private def verifyAssets(validationBefore: ValidationState[Long],
                           outAssets: Map[Seq[Byte], Long],
                           outAssetsNum: Int,
                           boxesToSpend: IndexedSeq[ErgoBox],
                           stateContext: ErgoStateContext): ValidationResult[Long] = {
    // Cost limit per block
    val maxCost = stateContext.currentParameters.maxBlockCost

    ErgoBoxAssetExtractor.extractAssets(boxesToSpend) match {
      case Success((inAssets, inAssetsNum)) =>
        lazy val newAssetId = mutable.WrappedArray.make(inputs.head.boxId)
        val tokenAccessCost = stateContext.currentParameters.tokenAccessCost
        val currentTxCost = validationBefore.result.payload.get

        val totalAssetsAccessCost =
          ErgoBoxAssetExtractor.totalAssetsAccessCost(inAssetsNum, inAssets.size, outAssetsNum, outAssets.size, tokenAccessCost)
        val newCost = addExact(currentTxCost, totalAssetsAccessCost)

        validationBefore
          // Check that transaction is not too costly considering all the assets
          .validate(bsBlockTransactionsCost, maxCost >= newCost, s"$id: assets cost")
          .validateSeq(outAssets) {
            case (validationState, (outAssetId, outAmount)) =>
              val inAmount: Long = inAssets.getOrElse(outAssetId, -1L)

              // Check that for each asset output amount is no more than input amount,
              // with a possible exception for a new asset created by the transaction
              validationState.validate(txAssetsPreservation,
                inAmount >= outAmount || (outAssetId == newAssetId && outAmount > 0),
                s"$id: Amount in = $inAmount, out = $outAmount. Allowed new asset = $newAssetId, out = $outAssetId")
          }
          .payload(newCost)
      case Failure(e) =>
        // should never be here as far as we've already checked this when we've created the box
        ModifierValidator.fatal(e.getMessage)
    }
  }

  /**
    * Helper method to validate reemission rules according to EIP-27
    */
  def verifyReemissionSpending(boxesToSpend: IndexedSeq[ErgoBox],
                               outputCandidates: Seq[ErgoBoxCandidate],
                               stateContext: ErgoStateContext): Try[Unit] = {
    Try {

      lazy val reemissionSettings = stateContext.ergoSettings.chainSettings.reemission
      lazy val reemissionRules = reemissionSettings.reemissionRules

      lazy val ReemissionTokenId = ModifierId @@ reemissionSettings.reemissionTokenId
      lazy val ReemissionTokenIdBytes = reemissionSettings.reemissionTokenIdBytes

      lazy val EmissionNftId = ModifierId @@ reemissionSettings.emissionNftId
      lazy val EmissionNftIdBytes = reemissionSettings.emissionNftIdBytes

      lazy val reemissionNftIdBytes = reemissionSettings.reemissionNftIdBytes
      lazy val emissionRules = stateContext.ergoSettings.chainSettings.emissionRules

      lazy val height = stateContext.currentHeight
      lazy val activationHeight = reemissionSettings.activationHeight

      // reemission logic below
      var reemissionSpending = false
      boxesToSpend.foreach { box =>
        // checking EIP-27 rules for emission box
        if (box.value > 100000 * EmissionRules.CoinsInOneErgo) { // for efficiency, skip boxes with less than 100,000 ERG
          // on activation height, emissionNft is not in emission box yet, but in injection box
          if (box.tokens.contains(EmissionNftId) ||
                height == activationHeight && boxesToSpend(1).tokens.contains(EmissionNftId)) {

            // if emission contract NFT is in the input, remission tokens should be there also
            val reemissionTokensIn = box.tokens.getOrElse(ReemissionTokenId, 0L)
            require(reemissionTokensIn > 0)

            // output positions guaranteed by emission contract
            val emissionOut = outputCandidates(0)
            val rewardsOut = outputCandidates(1)

            // check positions of emission NFT and reemission token
            require(emissionOut.additionalTokens.apply(0)._1 == EmissionNftIdBytes)
            require(emissionOut.additionalTokens.apply(1)._1 == ReemissionTokenIdBytes)

            //we're checking how emission box is paying reemission tokens below
            val emissionTokensOut = emissionOut.tokens.getOrElse(ReemissionTokenId, 0L)
            val rewardsTokensOut = rewardsOut.tokens.getOrElse(ReemissionTokenId, 0L)
            require(reemissionTokensIn == emissionTokensOut + rewardsTokensOut, "Reemission token not preserved")

            val properReemissionRewardPart = reemissionRules.reemissionForHeight(height, emissionRules)
            require(rewardsTokensOut == properReemissionRewardPart, "Rewards out condition violated")
          }
        } else if (box.tokens.contains(ReemissionTokenId) && height > reemissionSettings.activationHeight) {
          // reemission tokens spent after EIP-27 activation
          reemissionSpending = true
        }
      }

      // if box with reemission tokens spent
      if (reemissionSpending) {
        val toBurn = boxesToSpend.map { box =>
          box.tokens.getOrElse(ReemissionTokenId, 0L)
        }.sum
        val reemissionOutputs = outputCandidates.filter { out =>
          require(!out.tokens.contains(ReemissionTokenId), "outputs contain reemission token")
          out.ergoTree == reemissionRules.payToReemission(reemissionNftIdBytes)
        }
        require(reemissionOutputs.map(_.value).sum == toBurn, "Burning condition violated")
      }
    }
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
    * @return total computation cost (accumulatedCost + transaction verification cost), or error details
    */
  def validateStateful(boxesToSpend: IndexedSeq[ErgoBox],
                       dataBoxes: IndexedSeq[ErgoBox],
                       stateContext: ErgoStateContext,
                       accumulatedCost: Long)
                      (implicit verifier: ErgoInterpreter): ValidationState[Long] = {

    verifier.IR.resetContext() // ensure there is no garbage in the IRContext
    lazy val inputSumTry = Try(boxesToSpend.map(_.value).reduce(Math.addExact(_, _)))

    // Cost of transaction initialization: we should read and parse all inputs and data inputs,
    // and also iterate through all outputs to check rules
    val initialCost: Long = addExact(
      CostTable.interpreterInitCost,
      multiplyExact(boxesToSpend.size, stateContext.currentParameters.inputCost),
      multiplyExact(dataBoxes.size, stateContext.currentParameters.dataInputCost),
      multiplyExact(outputCandidates.size, stateContext.currentParameters.outputCost),
    )

    // Cost limit per block
    val maxCost = stateContext.currentParameters.maxBlockCost

    // We sum up previously accumulated cost and transaction initialization cost
    val startCost = addExact(initialCost, accumulatedCost)
    ModifierValidator(stateContext.validationSettings)
      // Check that the initial transaction cost is not too exceeding block limit
      .validate(bsBlockTransactionsCost, maxCost >= startCost, s"$id: initial cost")
      // Starting validation
      .payload(startCost)
      // Perform cheap checks first
      .validateNoFailure(txAssetsInOneBox, outAssetsTry)
      .validate(txPositiveAssets,
        outputCandidates.forall(_.additionalTokens.forall(_._2 > 0)),
        s"$id: ${outputCandidates.map(_.additionalTokens)}")
      // Check that outputs are not dust, and not created in future
      .validateSeq(outputs) { case (validationState, out) => verifyOutput(validationState, out, stateContext) }
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
        verifyAssets(validation, outAssets, outAssetsNum, boxesToSpend, stateContext)
      }
      // Check inputs, the most expensive check usually, so done last.
      .validateSeq(boxesToSpend.zipWithIndex) { case (validation, (box, idx)) =>
        val currentTxCost = validation.result.payload.get
        verifyInput(validation, boxesToSpend, dataBoxes, box, idx.toShort, stateContext, currentTxCost)
       }
      .validate(txReemission, !stateContext.ergoSettings.chainSettings.reemission.checkReemissionRules ||
                                verifyReemissionSpending(boxesToSpend, outputCandidates, stateContext).isSuccess)
  }

  /**
    * Same as `validateStateful`, but result is returned as Try[Long]
    **/
  def statefulValidity(boxesToSpend: IndexedSeq[ErgoBox],
                       dataBoxes: IndexedSeq[ErgoBox],
                       stateContext: ErgoStateContext,
                       accumulatedCost: Long = 0L)
                      (implicit verifier: ErgoInterpreter): Try[Long] = {
    validateStateful(boxesToSpend, dataBoxes, stateContext, accumulatedCost).result.toTry
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

  def apply(inputs: IndexedSeq[Input], outputCandidates: IndexedSeq[ErgoBoxCandidate]): ErgoTransaction =
    ErgoTransaction(inputs, IndexedSeq.empty, outputCandidates, None)

  def apply(tx: ErgoLikeTransaction): ErgoTransaction =
    ErgoTransaction(tx.inputs, tx.dataInputs, tx.outputCandidates)

}

object ErgoTransactionSerializer extends ScorexSerializer[ErgoTransaction] {

  override def serialize(tx: ErgoTransaction, w: Writer): Unit = {
    val elt = new ErgoLikeTransaction(tx.inputs, tx.dataInputs, tx.outputCandidates)
    ErgoLikeTransactionSerializer.serialize(elt, new SigmaByteWriter(w, None))
  }

  override def parse(r: Reader): ErgoTransaction = {
    val reader = new SigmaByteReader(r,
      new ConstantStore(),
      resolvePlaceholdersToConstants = false)
    val elt = ErgoLikeTransactionSerializer.parse(reader)
    ErgoTransaction(elt.inputs, elt.dataInputs, elt.outputCandidates)
  }
}
