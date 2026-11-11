package org.ergoplatform.wallet.transactions

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform._
import org.ergoplatform.sdk.BlockchainParameters
import org.ergoplatform.sdk.wallet.{AssetUtils, Constants, TokensMap}
import org.ergoplatform.wallet.boxes.{BoxSelector, DefaultBoxSelector, ErgoBoxAssetExtractor}
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import scorex.util.{ModifierId, bytesToId}
import sigma.ast.ErgoTree
import sigma.eval.Extensions.EvalIterableOps
import sigmastate.utils.Extensions._
import sigma.Coll
import sigma.Extensions.{ArrayOps, CollBytesOps}
import sigma.data.SigmaConstants.{MaxBoxSize, MaxPropositionBytes}

import scala.collection.JavaConverters._
import scala.util.Try

object TransactionBuilder {

  /**
    * @param recipientAddress - payment recipient address
    * @param transferAmt - amount of ERGs to transfer
    */
  case class Payment(
    recipientAddress: ErgoAddress,
    transferAmt: Long
  )

  /**
    * Assembles unsigned payment transaction with multiple outputs
    *
    * @param inputIds - identifiers of inputs to be used in transaction
    * @param feeAmt - fee amount
    * @param currentHeight - current blockchain height
    * @param payments - list of addresses and corresponding amounts to make outputs from
    * @param changeAddress - change recipient address
    * @param changeAmt - amount to return back to `changeAddress`
    * @return unsigned transaction
    */
  def multiPaymentTransaction(inputIds: Array[String],
                              feeAmt: Long,
                              payments: java.util.List[Payment],
                              changeAddress: ErgoAddress,
                              changeAmt: Long,
                              currentHeight: Int): UnsignedErgoLikeTransaction = {
    val feeBox = new ErgoBoxCandidate(
      feeAmt,
      ErgoTreePredef.feeProposition(),
      currentHeight,
      Array.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )
    val paymentBoxes =
      payments.asScala.map { case Payment(recipientAddress, transferAmt) =>
        new ErgoBoxCandidate(
          transferAmt,
          recipientAddress.script,
          currentHeight,
          Array.empty[(ErgoBox.TokenId, Long)].toColl,
          Map.empty
        )
      }.toVector

    val outputs =
      if (changeAmt == 0) {
        paymentBoxes :+ feeBox
      } else {
        val changeBox = new ErgoBoxCandidate(
          changeAmt,
          changeAddress.script,
          currentHeight,
          Array.empty[(ErgoBox.TokenId, Long)].toColl,
          Map.empty
        )
        paymentBoxes ++ Vector(feeBox, changeBox)
      }
    val unsignedInputs = unsignedInputsFromIds(inputIds)

    new UnsignedErgoLikeTransaction(
      unsignedInputs,
      dataInputs = IndexedSeq.empty,
      outputs
    )
  }

  /**
    * Assembles unsigned payment transaction.
    *
    * @param recipientAddress - payment recipient address
    * @param changeAddress - change recipient address
    * @param transferAmt - amount of ERGs to transfer
    * @param feeAmt - fee amount
    * @param changeAmt - amount to return back to `changeAddress`
    * @param inputIds - identifiers of inputs to be used in transaction
    * @param currentHeight - current blockchain height
    * @return unsigned transaction
    */
  def paymentTransaction(recipientAddress: ErgoAddress,
                         changeAddress: ErgoAddress,
                         transferAmt: Long,
                         feeAmt: Long,
                         changeAmt: Long,
                         inputIds: Array[String],
                         currentHeight: Int): UnsignedErgoLikeTransaction = {
    val payTo = new ErgoBoxCandidate(
      transferAmt,
      recipientAddress.script,
      currentHeight,
      Array.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )
    val fee = new ErgoBoxCandidate(
      feeAmt,
      ErgoTreePredef.feeProposition(),
      currentHeight,
      Array.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )
    val change = new ErgoBoxCandidate(
      changeAmt,
      changeAddress.script,
      currentHeight,
      Array.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )
    val unsignedInputs = unsignedInputsFromIds(inputIds)

    val dataInputs = IndexedSeq.empty
    val outputs = if (changeAmt == 0) {
      IndexedSeq(payTo, fee)
    } else {
      IndexedSeq(payTo, change, fee)
    }

    new UnsignedErgoLikeTransaction(
      unsignedInputs,
      dataInputs,
      outputs
    )
  }

  def collectOutputTokens(outputCandidates: Seq[ErgoBoxCandidate]): TokensMap = {
    AssetUtils.mergeAssets(
      initialMap = Map.empty[ModifierId, Long],
      maps = outputCandidates.map(b => collTokensToMap(b.additionalTokens)): _*)
  }

  def collTokensToMap(tokens: Coll[(TokenId, Long)]): TokensMap =
    tokens.toArray.map(t => t._1.toModifierId -> t._2).toMap

  def tokensMapToColl(tokens: TokensMap): Coll[(TokenId, Long)] =
    tokens.toArray.map {t => t._1.toTokenId -> t._2}.toColl

  private def unsignedInputsFromIds(inputIds: Array[String]): IndexedSeq[UnsignedInput] =
    inputIds.map { id =>
      val bytes = Base16.decode(id)
        .getOrElse(throw new IllegalArgumentException("Input id should be valid hex"))
      require(bytes.length == Constants.ModifierIdLength,
        s"Input id should be ${Constants.ModifierIdLength} bytes")
      new UnsignedInput(ADKey @@ bytes)
    }.toIndexedSeq

  private def validateStatelessChecks(inputs: IndexedSeq[ErgoBox], dataInputs: IndexedSeq[DataInput],
    outputCandidates: Seq[ErgoBoxCandidate]): Unit = {
    // checks from ErgoTransaction.validateStateless
    require(inputs.nonEmpty, "inputs cannot be empty")
    require(outputCandidates.nonEmpty, "outputCandidates cannot be empty")
    require(inputs.size <= Short.MaxValue, s"too many inputs - ${inputs.size} (max ${Short.MaxValue})")
    require(dataInputs.size <= Short.MaxValue, s"too many dataInputs - ${dataInputs.size} (max ${Short.MaxValue})")
    require(outputCandidates.size <= Short.MaxValue,
      s"too many outputCandidates - ${outputCandidates.size} (max ${Short.MaxValue})")
    require(outputCandidates.forall(_.value >= 0), s"outputCandidate.value must be >= 0")
    val outputSumTry = Try(outputCandidates.map(_.value).reduce(java7.compat.Math.addExact(_, _)))
    require(outputSumTry.isSuccess, s"Sum of transaction output values should not exceed ${Long.MaxValue}")
    require(inputs.distinct.size == inputs.size, s"There should be no duplicate inputs")
    require(dataInputs.distinct.size >= dataInputs.size - 1,
      s"There should be no more than one pair of data inputs with the same box id")
  }

  // Block version at which the monotonic-height rule (txMonotonicHeight) activates. Mirrors
  // Header.HardeningVersion in the node, which lives in ergo-core and is not reachable from here.
  private val HardeningBlockVersion: Byte = 2

  // Block version assumed when no BlockchainParameters are supplied. Past the hardening version, so
  // monotonic-height is enforced by default.
  private val DefaultBlockVersion: Byte = 3

  // Fixed-size, zero-filled stand-in for the (yet unknown) transaction id. Attaching it to an output
  // candidate yields the same serialized length the node measures on the final box - including the
  // tx reference (32-byte tx id plus the VLQ-encoded output index) accounted for by
  // BoxUtils.minimalErgoAmount and the box-size rule. The id value does not affect the length.
  private val mockTxId: ModifierId = bytesToId(Array.fill(32)(0.toByte))

  // Per-output checks from ErgoTransaction.validateStateful that depend only on a single output and the
  // current height. Used both as an early fail-fast on user outputs and inside validateStatefulChecks.
  private def validateStatefulOutputs(outputs: Seq[ErgoBoxCandidate], currentHeight: Int): Unit = {
    require(outputs.forall(_.additionalTokens.forall(_._2 > 0)),
      "all token amounts in outputs must be positive (txPositiveAssets)")
    // txFuture: the node checks against the height of the block that includes the transaction, one
    // above the best height that callers pass as currentHeight
    require(outputs.forall(_.creationHeight <= currentHeight + 1),
      s"output creationHeight must be <= currentHeight + 1 = ${currentHeight + 1} (txFuture)")
    require(outputs.forall(_.creationHeight >= 0),
      "output creationHeight must be >= 0 (txNegHeight)")
  }

  /** Checks from `ErgoTransaction.validateStateful` that are feasible without an `ErgoStateContext`,
    * network `Parameters` or a script interpreter. Run on the final transaction (inputs and outputs
    * including miner's fee and change boxes), so it mirrors what a node validates.
    *
    * Not implemented here, as they require full node state: `txBoxesToSpend`/`txDataBoxes` (in the wallet
    * inputs are the boxes and data inputs are not resolved), `txScriptValidation` (the tx is unsigned, so
    * there are no proofs and no interpreter), the EIP-27 `txReemission` rule (needs chain settings - note
    * the pay-to-reemission *output* is still added by buildUnsignedTx), `txRentDistinctOutputs` (needs the
    * storage-rent context and its activation height) and the block-cost rule (needs the cost model).
    *
    * @param minValuePerByte - storage-rent price used by the dust rule (network parameter or default)
    * @param blockVersion    - protocol block version, gating the monotonic-height rule
    */
  private def validateStatefulChecks(inputs: IndexedSeq[ErgoBox],
                                     outputs: IndexedSeq[ErgoBoxCandidate],
                                     currentHeight: Int,
                                     minValuePerByte: Int,
                                     blockVersion: Byte): Unit = {
    // txAssetsInOneBox: <= 255 tokens per box and per-token sums must not overflow. ErgoBoxCandidate does
    // not cap the token count (only its serializer needs it to fit in a byte), so an output with more
    // tokens reaches this check. Checked first, as the node does.
    require(ErgoBoxAssetExtractor.extractAssets(outputs).isSuccess,
      s"each output must have <= ${ErgoBoxAssetExtractor.MaxAssetsPerBox} tokens and per-token sums must not overflow (txAssetsInOneBox)")

    validateStatefulOutputs(outputs, currentHeight)

    // txMonotonicHeight: activated past the hardening block version (mirrors the node). Before it, an
    // output only needs a non-negative creation height, which validateStatefulOutputs already checks.
    val maxInputCreationHeight = if (blockVersion <= HardeningBlockVersion) 0 else inputs.map(_.creationHeight).max
    require(outputs.forall(_.creationHeight >= maxInputCreationHeight),
      s"output creationHeight must be >= max input creationHeight=$maxInputCreationHeight (txMonotonicHeight)")

    // Measure each output's full serialized size the way the node does, by attaching a fixed-size mock
    // tx reference (candidates have none yet). Reused by both the dust and box-size rules.
    val outputBoxSizes = outputs.zipWithIndex.map { case (out, idx) =>
      out -> out.toBox(mockTxId, idx.toShort).bytes.length
    }

    // txDust: output value must cover its storage cost (full box size * minValuePerByte).
    require(outputBoxSizes.forall { case (out, size) => out.value >= size.toLong * minValuePerByte },
      s"output value is below the dust threshold of $minValuePerByte nanoErg per byte (txDust)")

    // txBoxSize
    require(outputBoxSizes.forall { case (_, size) => size <= MaxBoxSize.value },
      s"output box size must be <= ${MaxBoxSize.value} bytes (txBoxSize)")

    // txBoxPropositionSize. Not independently reachable through buildUnsignedTx: a proposition larger
    // than MaxPropositionBytes also makes the whole box exceed MaxBoxSize, so txBoxSize fires first.
    // Kept as a faithful guard mirroring the node's rule set.
    require(outputs.forall(_.propositionBytes.length <= MaxPropositionBytes.value),
      s"output proposition size must be <= ${MaxPropositionBytes.value} bytes (txBoxPropositionSize)")

    // txInputsSum: input value sum must not overflow. The `changeAmt >= 0` check in buildUnsignedTx
    // does not cover it: an overflowing input sum can wrap so that `changeAmt` stays non-negative.
    val inputSumTry = Try(inputs.map(_.value).reduce(java7.compat.Math.addExact(_, _)))
    require(inputSumTry.isSuccess, s"sum of input values should not exceed ${Long.MaxValue} (txInputsSum)")

    // txErgPreservation: no ERGs are created or destroyed. buildUnsignedTx balances inputs and outputs
    // by construction, so this guards against a misbehaving box selector (e.g. wrong change or a dropped
    // pay-to-reemission output).
    val outputSumTry = Try(outputs.map(_.value).reduce(java7.compat.Math.addExact(_, _)))
    require(inputSumTry.toOption == outputSumTry.toOption,
      s"ERG not preserved: inputs sum ${inputSumTry.toOption}, outputs sum ${outputSumTry.toOption} (txErgPreservation)")

    // txAssetsPreservation: each output token amount must not exceed the input amount, except for the
    // single token minted by this tx (keyed by the first input's box id).
    val firstInputBoxId = bytesToId(inputs.head.id)
    val inTokens = collectOutputTokens(inputs)
    val outTokens = collectOutputTokens(outputs)
    require(
      outTokens.forall { case (tokenId, outAmount) =>
        inTokens.getOrElse(tokenId, 0L) >= outAmount || (tokenId == firstInputBoxId && outAmount > 0)
      },
      "output token amount exceeds input amount and is not the single minted token (txAssetsPreservation)")
  }

  /** Creates unsigned transaction from given inputs and outputs adding outputs with miner's fee and change,
    * with no network parameters and no pay-to-reemission script. See the full overload below for the
    * parameters. This signature stays so that callers compiled against earlier versions (such as appkit
    * 6.0.x) still link; Scala allows default arguments on one overload only, so they stay here.
    */
  def buildUnsignedTx(
    inputs: IndexedSeq[ErgoBox],
    dataInputs: IndexedSeq[DataInput],
    outputCandidates: Seq[ErgoBoxCandidate],
    currentHeight: Int,
    createFeeOutput: Option[Long],
    changeAddress: ErgoAddress,
    minChangeValue: Long,
    minerRewardDelay: Int,
    burnTokens: TokensMap = Map.empty,
    boxSelector: BoxSelector = new DefaultBoxSelector(None)
  ): Try[UnsignedErgoLikeTransaction] =
    buildUnsignedTx(
      inputs,
      dataInputs,
      outputCandidates,
      currentHeight,
      createFeeOutput,
      changeAddress,
      minChangeValue,
      minerRewardDelay,
      burnTokens,
      boxSelector,
      parameters            = None,
      payToReemissionScript = None
    )

  /** Creates unsigned transaction from given inputs and outputs adding outputs with miner's fee and change
    * Runs required checks ensuring that resulted transaction will be successfully validated by a node.
    * All arguments are required here: the default arguments live on the shorter overload above.
    *
    * @param inputs           - input boxes
    * @param dataInputs       - data inputs
    * @param outputCandidates - output candidate boxes
    * @param currentHeight    - current (best) blockchain height, used in miner's fee box and change box.
    *                           Outputs may be created up to one block above it, as the node validates
    *                           against the height of the next block.
    * @param createFeeOutput  - optional fee amount to put in a new miner's fee box, which will be
    *                           created by this method. If None, then feeOut is not created.
    * @param changeAddress    - address where to send change from the input boxes
    * @param minChangeValue   - minimum change value to send, otherwise add to miner's fee
    * @param minerRewardDelay - reward delay to encode in miner's fee box
    * @param burnTokens       - tokens to burn (excluded from change outputs)
    * @param boxSelector      - box selector to compute change boxes
    * @param parameters       - optional network parameters; when given, the dust rule uses the network
    *                           `minValuePerByte` and the monotonic-height rule follows its block version.
    *                           When absent, the protocol default per-byte price and an enforced
    *                           monotonic-height rule are assumed.
    * @param payToReemissionScript - EIP-27 pay-to-reemission proposition. Required only when the box
    *                           selector produces a pay-to-reemission output; the wallet cannot derive
    *                           this contract itself (see ReemissionData) so it must be supplied.
    * @return unsigned transaction
    */
  def buildUnsignedTx(
    inputs: IndexedSeq[ErgoBox],
    dataInputs: IndexedSeq[DataInput],
    outputCandidates: Seq[ErgoBoxCandidate],
    currentHeight: Int,
    createFeeOutput: Option[Long],
    changeAddress: ErgoAddress,
    minChangeValue: Long,
    minerRewardDelay: Int,
    burnTokens: TokensMap,
    boxSelector: BoxSelector,
    parameters: Option[BlockchainParameters],
    payToReemissionScript: Option[ErgoTree]
  ): Try[UnsignedErgoLikeTransaction] = Try {

    validateStatelessChecks(inputs, dataInputs, outputCandidates)

    // fail fast on user outputs before doing box selection; the full set of stateful checks is run on
    // the assembled transaction (incl. fee and change boxes) right before it is returned.
    validateStatefulOutputs(outputCandidates, currentHeight)

    val minValuePerByte = parameters.map(_.minValuePerByte).getOrElse(BoxSelector.MinValuePerByteDefault)
    val blockVersion    = parameters.map(_.blockVersion).getOrElse(DefaultBlockVersion)

    val feeAmount = createFeeOutput.getOrElse(0L)
    require(createFeeOutput.fold(true)(_ > 0), s"expected fee amount > 0, got $feeAmount")
    val inputTotal  = inputs.map(_.value).sum
    val outputSum   = outputCandidates.map(_.value).sum
    val outputTotal = outputSum + feeAmount
    val changeAmt   = inputTotal - outputTotal
    require(changeAmt >= 0, s"total inputs $inputTotal is less then total outputs $outputTotal")

    val firstInputBoxId = bytesToId(inputs(0).id)
    val tokensOut = collectOutputTokens(outputCandidates)
    // remove minted tokens if any
    val tokensOutNoMinted = tokensOut.filterKeys(_ != firstInputBoxId)
    val mintedTokensNum = tokensOut.size - tokensOutNoMinted.size
    require(mintedTokensNum <= 1, s"Only one token can be minted, but found $mintedTokensNum")
    require(burnTokens.values.forall(_ > 0),
      s"Incorrect burnTokens specification, positive values are expected: $burnTokens")

    // add burnTokens to target assets so that they are excluded from the change outputs
    // thus total outputs assets will be reduced which is interpreted as _token burning_
    val tokensOutWithBurned = AssetUtils.mergeAssets(tokensOutNoMinted.toMap, burnTokens)

    val selection = boxSelector.select(inputs.toIterator, outputTotal, tokensOutWithBurned) match {
      case Left(err) => throw new IllegalArgumentException(
        s"failed to calculate change for outputTotal: $outputTotal, \ntokens: $tokensOut, \nburnTokens: $burnTokens, \ninputs: $inputs, \nreason: $err")
      case Right(v) => v
    }
    // although we're only interested in change boxes, make sure selection contains exact inputs
    assert(selection.inputBoxes == inputs, s"unexpected selected boxes, expected: $inputs, got ${selection.inputBoxes}")
    // EIP-27: reemission tokens are burnt, so change never keeps them (the node wallet does the same)
    val reemissionTokenIdOpt = boxSelector.reemissionDataOpt.map(_.reemissionTokenId)
    val changeBoxes = selection.changeBoxes.map { cb =>
      ErgoBoxAssetsHolder(cb.value, cb.tokens.filter {
        case (id, _) => !reemissionTokenIdOpt.contains(id)
      })
    }
    val changeBoxesHaveTokens = changeBoxes.exists(_.tokens.nonEmpty)

    // EIP-27: if the selector carved out a pay-to-reemission output, send the reemitted ERGs to the
    // (externally provided) reemission contract instead of dropping the output. The wallet cannot derive
    // the contract itself (ReemissionData lacks it), so the script must be supplied by the caller.
    val reemissionOutOpt = selection.payToReemissionBox.map { eip27Assets =>
      val p2r = payToReemissionScript.getOrElse(throw new IllegalArgumentException(
        "box selector produced an EIP-27 pay-to-reemission output but no payToReemissionScript was provided"))
      new ErgoBoxCandidate(eip27Assets.value, p2r, currentHeight)
    }

    // the selector's change leaves out the ERGs that go to the pay-to-reemission output, so only the
    // rest is change; small change is folded into the miner's fee
    val changeValue     = changeAmt - reemissionOutOpt.map(_.value).getOrElse(0L)
    val changeGoesToFee = changeValue < minChangeValue && !changeBoxesHaveTokens

    require(!changeGoesToFee || (changeValue == 0 || createFeeOutput.isDefined),
      s"""When change=$changeValue < minChangeValue=$minChangeValue it is added to miner's fee,
        |in this case createFeeOutput should be defined
        |""".stripMargin)

    val feeOutOpt = createFeeOutput.map { fee =>
      // if computed change is too small give it to miner as tips
      val actualFee = if (changeGoesToFee) fee + changeValue else fee
      new ErgoBoxCandidate(
        actualFee,
        ErgoTreePredef.feeProposition(minerRewardDelay),
        currentHeight
      )
    }

    val addedChangeOut = if (!changeGoesToFee) {
      val script = changeAddress.script
      changeBoxes.map { cb =>
        new ErgoBoxCandidate(cb.value, script, currentHeight, tokensMapToColl(cb.tokens))
      }
    } else {
      Seq()
    }

    val finalOutputCandidates = outputCandidates ++ feeOutOpt ++ reemissionOutOpt ++ addedChangeOut

    validateStatefulChecks(inputs, finalOutputCandidates.toIndexedSeq, currentHeight, minValuePerByte, blockVersion)

    new UnsignedErgoLikeTransaction(
      inputs.map(b => new UnsignedInput(b.id)),
      dataInputs,
      finalOutputCandidates.toIndexedSeq
    )
  }

  implicit class EitherOpsFor211[+A, +B](val source: Either[A, B]) extends AnyVal {

    /** The given function is applied if this is a `Right`.
      *
      *  {{{
      *  Right(12).map(x => "flower") // Result: Right("flower")
      *  Left(12).map(x => "flower")  // Result: Left(12)
      *  }}}
      */
    def mapRight[B1](f: B => B1): Either[A, B1] = source match {
      case Right(b) => Right(f(b))
      case _        => source.asInstanceOf[Either[A, B1]]
    }

    /** Binds the given function across `Right`.
      *
      *  @param f The function to bind across `Right`.
      */
    def flatMapRight[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = source match {
      case Right(b) => f(b)
      case _        => source.asInstanceOf[Either[A1, B1]]
    }
  }

}
