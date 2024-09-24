package org.ergoplatform.wallet.transactions

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform._
import org.ergoplatform.sdk.wallet.{AssetUtils, TokensMap}
import org.ergoplatform.wallet.boxes.{BoxSelector, DefaultBoxSelector}
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import scorex.util.{ModifierId, bytesToId}
import sigmastate.utils.Extensions._
import sigma.Coll
import sigma.Extensions.{ArrayOps, CollBytesOps}

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
    val unsignedInputs = inputIds
      .flatMap { id =>
        Base16.decode(id)
          .map(x => new UnsignedInput(ADKey @@ x))
          .toOption
      }.toIndexedSeq

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
    val unsignedInputs = inputIds
      .flatMap { id =>
        Base16.decode(id)
          .map(x => new UnsignedInput(ADKey @@ x))
          .toOption
      }
      .toIndexedSeq

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
  }

  /** Creates unsigned transaction from given inputs and outputs adding outputs with miner's fee and change
    * Runs required checks ensuring that resulted transaction will be successfully validated by a node.
    *
    * @param inputs           - input boxes
    * @param dataInputs       - data inputs
    * @param outputCandidates - output candidate boxes
    * @param currentHeight    - current height (used in miner's fee box and change box)
    * @param createFeeOutput  - optional fee amount to put in a new miner's fee box, which will be
    *                           created by this method. If None, then feeOut is not created.
    * @param changeAddress    - address where to send change from the input boxes
    * @param minChangeValue   - minimum change value to send, otherwise add to miner's fee
    * @param minerRewardDelay - reward delay to encode in miner's fee box
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
    burnTokens: TokensMap = Map.empty,
    boxSelector: BoxSelector = new DefaultBoxSelector(None)
  ): Try[UnsignedErgoLikeTransaction] = Try {

    validateStatelessChecks(inputs, dataInputs, outputCandidates)

    // TODO: implement all appropriate checks from ErgoTransaction.validateStatefull

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
    val changeBoxes = selection.changeBoxes
    val changeBoxesHaveTokens = changeBoxes.exists(_.tokens.nonEmpty)

    val changeGoesToFee = changeAmt < minChangeValue && !changeBoxesHaveTokens

    require(!changeGoesToFee || (changeAmt == 0 || createFeeOutput.isDefined),
      s"""When change=$changeAmt < minChangeValue=$minChangeValue it is added to miner's fee,
        |in this case createFeeOutput should be defined
        |""".stripMargin)

    val feeOutOpt = createFeeOutput.map { fee =>
      // if computed changeAmt is too small give it to miner as tips
      val actualFee = if (changeGoesToFee) fee + changeAmt else fee
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

    val finalOutputCandidates = outputCandidates ++ feeOutOpt ++ addedChangeOut

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
