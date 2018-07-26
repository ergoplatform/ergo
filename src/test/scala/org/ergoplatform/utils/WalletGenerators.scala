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
    } yield UnspentOffchainBox(tx, outIndex, ergoBox)
  }

  def uncertainUnspentOffchainBoxGen: Gen[UncertainUnspentOffchainBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      ergoBox <- Gen.oneOf(boxes)
    } yield UncertainUnspentOffchainBox(tx, outIndex, ergoBox)
  }

  def unspentOnchainBoxGen: Gen[UnspentOnchainBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      ergoBox <- Gen.oneOf(boxes)
    } yield UnspentOnchainBox(tx, outIndex, height, ergoBox)
  }

  def uncertainUnspentOnchainBoxGen: Gen[UncertainUnspentOnchainBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      ergoBox <- Gen.oneOf(boxes)
    } yield UncertainUnspentOnchainBox(tx, outIndex, height, ergoBox)
  }


  def spentOffchainBoxGen: Gen[SpentOffchainBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      heightOpt <- Gen.option(heightGen())
      ergoBox <- Gen.oneOf(boxes)
    } yield SpentOffchainBox(tx, outIndex, heightOpt, spendingTx, ergoBox)
  }

  def uncertainSpentOffchainBoxGen: Gen[UncertainSpentOffchainBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      heightOpt <- Gen.option(heightGen())
      ergoBox <- Gen.oneOf(boxes)
    } yield UncertainSpentOffchainBox(tx, outIndex, heightOpt, spendingTx, ergoBox)
  }

  def spentOnchainBoxGen: Gen[SpentOnchainBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      spendingHeight <- heightGen(height)
      ergoBox <- Gen.oneOf(boxes)
    } yield SpentOnchainBox(tx, outIndex, height, spendingTx, spendingHeight, ergoBox)
  }

  def uncertainSpentOnchainBoxGen: Gen[UncertainSpentOnchainBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      spendingHeight <- heightGen(height)
      ergoBox <- Gen.oneOf(boxes)
    } yield UncertainSpentOnchainBox(tx, outIndex, height, spendingTx, spendingHeight, ergoBox)
  }

  private def outIndexGen(tx: ErgoTransaction) =
    Gen.choose(0: Short, tx.outputCandidates.length.toShort)

  private def heightGen(min: Int = 0) = {
    Gen.choose(min + 1, Integer.MAX_VALUE / 2 + min)
  }
}
