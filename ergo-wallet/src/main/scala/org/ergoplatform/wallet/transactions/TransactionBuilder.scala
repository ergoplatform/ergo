package org.ergoplatform.wallet.transactions

import java.util

import scala.collection.IndexedSeq
import scala.language.postfixOps
import org.ergoplatform.ErgoBox
import org.ergoplatform.DataInput
import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.ErgoAddress
import org.ergoplatform.ErgoScriptPredef
import org.ergoplatform.UnsignedErgoLikeTransaction
import org.ergoplatform.UnsignedInput
import scala.util.Try

object TransactionBuild {

  // TODO: scaladoc
  def buildUnsignedTx(
    inputs: IndexedSeq[ErgoBox],
    dataInputs: IndexedSeq[DataInput],
    outputCandidates: Seq[ErgoBoxCandidate],
    feeAmount: Long,
    changeAddress: Option[ErgoAddress],
    currentHeight: Int,
    minFee: Long,
    minChangeValue: Long,
    minerRewardDelay: Int
  ): Try[UnsignedErgoLikeTransaction] = Try {

    // from ErgoTransaction.validateStateless
    require(inputs.nonEmpty, "inputs cannot be empty")
    require(outputCandidates.nonEmpty, "outputCandidates cannot be empty")
    require(inputs.size <= Short.MaxValue, s"too many inputs - ${inputs.size} (max ${Short.MaxValue})")
    require(dataInputs.size <= Short.MaxValue, s"too many dataInputs - ${dataInputs.size} (max ${Short.MaxValue})")
    require(outputCandidates.size <= Short.MaxValue, 
      s"too many outputCandidates - ${outputCandidates.size} (max ${Short.MaxValue})")
    require(outputCandidates.forall(_.value >= 0), s"outputCandidate.value must be >= 0")
    val outputSumTry = Try(outputCandidates.map(_.value).reduce(Math.addExact(_, _)))
    require(outputSumTry.isSuccess, s"Sum of transaction output values should not exceed ${Long.MaxValue}")
    require(inputs.distinct.size == inputs.size, s"There should be no duplicate inputs")

    // TODO: implement all appropriate checks from ErgoTransaction.validateStatefull

    require(feeAmount > 0, "Fee amount should be defined")
    val inputTotal  = inputs.map(_.value).sum
    val outputSum   = outputCandidates.map(_.value).sum
    val outputTotal = outputSum + feeAmount
    val changeAmt   = inputTotal - outputTotal
    require(
      changeAmt >= 0,
      s"total inputs $inputTotal is less then total outputs $outputTotal"
    )
    val noChange = changeAmt < minChangeValue
    // if computed changeAmt is too small give it to miner as tips
    val actualFee = if (noChange) feeAmount + changeAmt else feeAmount
    require(
      actualFee >= minFee,
      s"Fee ($actualFee) must be greater then minimum amount ($minFee NanoErg)"
    )
    val feeOut = new ErgoBoxCandidate(
      actualFee,
      ErgoScriptPredef.feeProposition(minerRewardDelay),
      currentHeight
    )

    val addedChangeOut = if (!noChange) {
      require(changeAddress.isDefined, s"change address is required for $changeAmt")
      val changeOut =
        new ErgoBoxCandidate(changeAmt, changeAddress.get.script, currentHeight)
      Seq(changeOut)
    } else Seq()

    val finalOutputCandidates = outputCandidates ++ Seq(feeOut) ++ addedChangeOut

    val mintedTokensNum = finalOutputCandidates
      .flatMap(_.additionalTokens.toArray)
      .count(t => util.Arrays.equals(t._1, inputs.head.id))

    require(
      mintedTokensNum <= 1,
      s"Only one token can be minted, but found $mintedTokensNum"
    )

    new UnsignedErgoLikeTransaction(
      inputs.map(b => new UnsignedInput(b.id)),
      dataInputs,
      finalOutputCandidates.toIndexedSeq
    )
  }
}