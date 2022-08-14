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
import scorex.core.validation.{InvalidModifier, ModifierValidator, ValidationResult, ValidationState}
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
      .validate(txNoInputs, inputs.nonEmpty, InvalidModifier(s"Tx $id has no inputs", id, modifierTypeId))
      .validate(txNoOutputs, outputCandidates.nonEmpty, InvalidModifier(s"Tx $id has no outputs", id, modifierTypeId))
      .validate(txManyInputs, inputs.size <= Short.MaxValue, InvalidModifier(s"$id: ${inputs.size}", id, modifierTypeId))
      .validate(txManyDataInputs, dataInputs.size <= Short.MaxValue, InvalidModifier(s"$id: ${dataInputs.size}", id, modifierTypeId))
      .validate(txManyOutputs, outputCandidates.size <= Short.MaxValue, InvalidModifier(s"$id: ${outputCandidates.size}", id, modifierTypeId))
      .validate(txNegativeOutput, outputCandidates.forall(_.value >= 0), InvalidModifier(s"$id: ${outputCandidates.map(_.value)}", id, modifierTypeId))
      .validateNoFailure(txOutputSum, outputsSumTry, id, modifierTypeId)
      .validate(txInputsUnique, inputs.distinct.size == inputs.size, InvalidModifier(s"$id: ${inputs.distinct.size} == ${inputs.size}", id, modifierTypeId))
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
    val maxCost = stateContext.currentParameters.maxBlockCost.toLong

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
    val (isCostValid, scriptCost: Long) =
      costTry match {
        case Failure(t) =>
          log.warn(s"Tx verification failed: ${t.getMessage}", t)
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
      .validate(txScriptValidation, costTry.isSuccess && isCostValid, InvalidModifier(s"$id: #$inputIndex => $costTry", id, modifierTypeId))
      // Check that cost of the transaction after checking the input becomes too big
      .validate(bsBlockTransactionsCost, currCost <= maxCost, InvalidModifier(s"$id: cost exceeds limit after input #$inputIndex", id, modifierTypeId))
      .map(c => addExact(c, scriptCost))
  }

  private def verifyOutput(validationBefore: ValidationState[Long],
                           out: ErgoBox,
                           stateContext: ErgoStateContext): ValidationResult[Long] = {

    val blockVersion = stateContext.blockVersion

    validationBefore
      .validate(txDust, out.value >= BoxUtils.minimalErgoAmount(out, stateContext.currentParameters), InvalidModifier(s"$id, output ${Algos.encode(out.id)}, ${out.value} >= ${BoxUtils.minimalErgoAmount(out, stateContext.currentParameters)}", id, modifierTypeId))
      .validate(txFuture, out.creationHeight <= stateContext.currentHeight, InvalidModifier(s" ${out.creationHeight} <= ${stateContext.currentHeight} is not true, output id: $id: output $out", id, modifierTypeId))
      .validate(txNegHeight, (blockVersion == 1) || out.creationHeight >= 0, InvalidModifier(s" ${out.creationHeight} >= 0 is not true, output id: $id: output $out", id, modifierTypeId))
      .validate(txBoxSize, out.bytes.length <= MaxBoxSize.value, InvalidModifier(s"$id: output $out", id, modifierTypeId))
      .validate(txBoxPropositionSize, out.propositionBytes.length <= MaxPropositionBytes.value, InvalidModifier(s"$id: output $out", id, modifierTypeId))
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
          .validate(bsBlockTransactionsCost, maxCost >= newCost, InvalidModifier(s"$id: assets cost", id, modifierTypeId))
          .validateSeq(outAssets) {
            case (validationState, (outAssetId, outAmount)) =>
              val inAmount: Long = inAssets.getOrElse(outAssetId, -1L)

              // Check that for each asset output amount is no more than input amount,
              // with a possible exception for a new asset created by the transaction
              validationState.validate(txAssetsPreservation,
                inAmount >= outAmount || (outAssetId == newAssetId && outAmount > 0),
                InvalidModifier(s"$id: Amount in = $inAmount, out = $outAmount. Allowed new asset = $newAssetId, out = $outAssetId", id, modifierTypeId))
          }
          .payload(newCost)
      case Failure(e) =>
        // should never be here as far as we've already checked this when we've created the box
        ModifierValidator.fatal(e.getMessage, id, modifierTypeId)
    }
  }

  /**
    * Helper method to validate reemission rules according to EIP-27
    *
    * @param boxesToSpend     - inputs of transaction
    * @param outputCandidates - outputs of the transaction
    * @param stateContext     - validation context
    */
  private def verifyReemissionSpending(boxesToSpend: IndexedSeq[ErgoBox],
                                       outputCandidates: Seq[ErgoBoxCandidate],
                                       stateContext: ErgoStateContext): Try[Unit] = {
    val res: Try[Unit] = Try {

      lazy val reemissionSettings = stateContext.ergoSettings.chainSettings.reemission
      lazy val reemissionRules = reemissionSettings.reemissionRules

      lazy val reemissionTokenId = ModifierId @@ reemissionSettings.reemissionTokenId
      lazy val reemissionTokenIdBytes = reemissionSettings.reemissionTokenIdBytes

      lazy val emissionNftId = ModifierId @@ reemissionSettings.emissionNftId
      lazy val emissionNftIdBytes = reemissionSettings.emissionNftIdBytes

      lazy val chainSettings = stateContext.ergoSettings.chainSettings
      lazy val emissionRules = chainSettings.emissionRules

      lazy val height = stateContext.currentHeight

      val activationHeight = reemissionSettings.activationHeight

      if (height >= activationHeight) { // we check EIP-27 rules only since activation height
        // reemission check logic below
        var reemissionSpending = false // flag indicating that inputs have re-emission tokens
        boxesToSpend.foreach { box =>
          // checking EIP-27 rules for emission box
          // for efficiency, skip boxes with less than 100K ERG
          // secure, as charging emission box will be stopped
          // before 100K ERG left in the emission box
          // todo: for efficiency, we can raise the bar probably
          if (box.value > 100000 * EmissionRules.CoinsInOneErgo) {
            // on activation height, emissionNft is not in emission box yet, but in injection box
            // injection box index (1) is enforced by injection box contract
            if (box.tokens.contains(emissionNftId) ||
              (height == activationHeight && boxesToSpend(1).tokens.contains(emissionNftId))) {

              // in this branch, we are checking spending of re-emission tokens from the emission boxes

              // if emission contract NFT is in the input, remission tokens should be there also
              val reemissionTokensIn = if (height == activationHeight) {
                boxesToSpend(1).tokens.getOrElse(reemissionTokenId, 0L)
              } else {
                box.tokens.getOrElse(reemissionTokenId, 0L)
              }
              require(reemissionTokensIn > 0, "No re-emission tokens in the emission or injection box")

              // output positions guaranteed by emission contract
              val emissionOut = outputCandidates(0)
              val rewardsOut = outputCandidates(1)

              // check positions of emission NFT and reemission token
              val firstEmissionBoxTokenId = emissionOut.additionalTokens.apply(0)._1
              val secondEmissionBoxTokenId = emissionOut.additionalTokens.apply(1)._1
              require(
                firstEmissionBoxTokenId.sameElements(emissionNftIdBytes),
                "No emission box NFT in the emission box"
              )
              require(
                secondEmissionBoxTokenId.sameElements(reemissionTokenIdBytes),
                "No re-emission token in the emission box"
              )

              //we're checking how emission box is paying reemission tokens below
              val emissionTokensOut = emissionOut.tokens.getOrElse(reemissionTokenId, 0L)
              val rewardsTokensOut = rewardsOut.tokens.getOrElse(reemissionTokenId, 0L)
              // it is prohibited to burn re-emission tokens on spending the emission box
              require(reemissionTokensIn == emissionTokensOut + rewardsTokensOut, "Reemission tokens not preserved")

              val properReemissionRewardPart = reemissionRules.reemissionForHeight(height, emissionRules)
              require(rewardsTokensOut == properReemissionRewardPart, "Rewards out condition violated")
            } else {
              // this path can be removed after EIP-27 activation in 5.0
              // that is not so easy though, see https://github.com/ergoplatform/ergo/issues/1736
              if (height >= activationHeight && box.ergoTree == chainSettings.monetary.emissionBoxProposition) {
                //we require emission contract NFT and reemission token to be presented in emission output
                val emissionOutTokens = outputCandidates(0).tokens
                require(emissionOutTokens.contains(emissionNftId))
                require(emissionOutTokens.contains(reemissionTokenId))
              }
            }
          } else if (box.tokens.contains(reemissionTokenId) && height > activationHeight) {
            // reemission tokens spent after EIP-27 activation
            reemissionSpending = true
          }
        }

        // if box with reemission tokens spent
        if (reemissionSpending) {
          val payToReemissionContract = reemissionRules.payToReemission
          val toBurn = boxesToSpend.map { box =>
            box.tokens.getOrElse(reemissionTokenId, 0L)
          }.sum
          log.debug(s"Reemission tokens to burn: $toBurn")
          val reemissionOutputs = outputCandidates.filter { out =>
            require(!out.tokens.contains(reemissionTokenId), "outputs contain reemission token")
            // we compare by trees in the mainnet (to avoid disagreement with versions 4.0.29-31 doing that),
            // and by propositions in the testnet (as there are v0 & v1 transactions paying to pay-to-reemission there)
            // see https://github.com/ergoplatform/ergo/pull/1728 for details.
            if (chainSettings.isMainnet) {
              out.ergoTree == payToReemissionContract
            } else {
              out.ergoTree.toProposition(true) == payToReemissionContract.toProposition(true)
            }
          }
          val sentToReemission = reemissionOutputs.map(_.value).sum
          require(sentToReemission == toBurn, "Burning condition violated")
        }
      } else {
        Success(())
      }
    }

    res match {
      case Failure(e) => log.error(s"EIP-27 check failed due to ${e.getMessage} : ", e)
      case _ =>
    }

    res
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
    val maxCost = stateContext.currentParameters.maxBlockCost.toLong

    // We sum up previously accumulated cost and transaction initialization cost
    val startCost = addExact(initialCost, accumulatedCost)
    ModifierValidator(stateContext.validationSettings)
      // Check that the initial transaction cost is not too exceeding block limit
      .validate(bsBlockTransactionsCost, maxCost >= startCost, InvalidModifier(s"$id: initial cost", id, modifierTypeId))
      // Starting validation
      .payload(startCost)
      // Perform cheap checks first
      .validateNoFailure(txAssetsInOneBox, outAssetsTry, id, modifierTypeId)
      .validate(txPositiveAssets,
        outputCandidates.forall(_.additionalTokens.forall(_._2 > 0)),
        InvalidModifier(s"$id: ${outputCandidates.map(_.additionalTokens)}", id, modifierTypeId))
      // Check that outputs are not dust, and not created in future
      .validateSeq(outputs) { case (validationState, out) => verifyOutput(validationState, out, stateContext) }
      // Just to be sure, check that all the input boxes to spend (and to read) are presented.
      // Normally, this check should always pass, if the client is implemented properly
      // so it is not part of the protocol really.
      .validate(txBoxesToSpend, boxesToSpend.size == inputs.size, InvalidModifier(s"$id: ${boxesToSpend.size} == ${inputs.size}", id, modifierTypeId))
      .validate(txDataBoxes, dataBoxes.size == dataInputs.size, InvalidModifier(s"$id: ${dataBoxes.size} == ${dataInputs.size}", id, modifierTypeId))
      // Check that there are no overflow in input and output values
      .validate(txInputsSum, inputSumTry.isSuccess, InvalidModifier(s"$id as invalid Inputs Sum", id, modifierTypeId))
      // Check that transaction is not creating money out of thin air.
      .validate(txErgPreservation, inputSumTry == outputsSumTry, InvalidModifier(s"$id: $inputSumTry == $outputsSumTry", id, modifierTypeId))
      .validateTry(outAssetsTry, e => ModifierValidator.fatal("Incorrect assets", id, modifierTypeId, e)) { case (validation, (outAssets, outAssetsNum)) =>
        verifyAssets(validation, outAssets, outAssetsNum, boxesToSpend, stateContext)
      }
      // Check inputs, the most expensive check usually, so done last.
      .validateSeq(boxesToSpend.zipWithIndex) { case (validation, (box, idx)) =>
        val currentTxCost = validation.result.payload.get
        verifyInput(validation, boxesToSpend, dataBoxes, box, idx.toShort, stateContext, currentTxCost)
       }
      .validate(txReemission, !stateContext.ergoSettings.chainSettings.reemission.checkReemissionRules ||
                                verifyReemissionSpending(boxesToSpend, outputCandidates, stateContext).isSuccess, InvalidModifier(id, id, modifierTypeId))
  }

  /**
    * Same as `validateStateful`, but result is returned as Try[Long]
    **/
  def statefulValidity(boxesToSpend: IndexedSeq[ErgoBox],
                       dataBoxes: IndexedSeq[ErgoBox],
                       stateContext: ErgoStateContext,
                       accumulatedCost: Long = 0L)
                      (implicit verifier: ErgoInterpreter): Try[Int] = {
    validateStateful(boxesToSpend, dataBoxes, stateContext, accumulatedCost).result.toTry.map(_.toInt)
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
