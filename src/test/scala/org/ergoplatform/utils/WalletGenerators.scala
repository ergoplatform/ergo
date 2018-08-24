package org.ergoplatform.utils

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet._
import org.scalacheck.Gen
import sigmastate.Values

trait WalletGenerators extends ErgoTransactionGenerators {

  def trackedBoxGen: Gen[TrackedBox] = {
    Gen.oneOf(unspentBoxGen, spentOffchainBoxGen, spentOnchainBoxGen)
  }

  def unspentBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- Gen.option(heightGen())
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
    } yield TrackedBox(tx, outIndex, height, ergoBox, certainty)
  }

  def spentOffchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      heightOpt <- Gen.option(heightGen())
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
    } yield TrackedBox(tx, outIndex, heightOpt, Some(spendingTx), None, ergoBox, certainty)
  }

  def spentOnchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      spendingHeight <- heightGen(height)
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
    } yield TrackedBox(tx, outIndex, Some(height), Some(spendingTx), Some(spendingHeight), ergoBox, certainty)
  }

  def paymentRequestGen: Gen[PaymentRequest] = {
    for {
      value <- Gen.choose(1L, 100000L)
      assets <- Gen.option(additionalTokensGen)
      registers <- Gen.option(additionalRegistersGen)
    } yield PaymentRequest(Pay2SAddress(Values.FalseLeaf), value, assets, registers)
  }

  private def outIndexGen(tx: ErgoTransaction) =
    Gen.choose(0: Short, tx.outputCandidates.length.toShort)

  private def heightGen(min: Int = 0) = {
    Gen.choose(min + 1, Integer.MAX_VALUE / 2 + min)
  }
}
