package org.ergoplatform.utils

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet._
import org.scalacheck.Gen

trait WalletGenerators extends ErgoTransactionGenerators {

  def unspentOffchainBoxGen: Gen[UnspentOffchainBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
    } yield UnspentOffchainBox(tx, outIndex, ergoBox, certainty)
  }

  def unspentOnchainBoxGen: Gen[UnspentOnchainBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
    } yield UnspentOnchainBox(tx, outIndex, height, ergoBox, certainty)
  }


  def spentOffchainBoxGen: Gen[SpentOffchainBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      heightOpt <- Gen.option(heightGen())
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
    } yield SpentOffchainBox(tx, outIndex, heightOpt, spendingTx, ergoBox, certainty)
  }

  def spentOnchainBoxGen: Gen[SpentOnchainBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      spendingHeight <- heightGen(height)
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
    } yield SpentOnchainBox(tx, outIndex, height, spendingTx, spendingHeight, ergoBox, certainty)
  }

  private def outIndexGen(tx: ErgoTransaction) =
    Gen.choose(0: Short, tx.outputCandidates.length.toShort)

  private def heightGen(min: Int = 0) = {
    Gen.choose(min + 1, Integer.MAX_VALUE / 2 + min)
  }
}
