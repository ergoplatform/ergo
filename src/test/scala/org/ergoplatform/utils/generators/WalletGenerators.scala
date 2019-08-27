package org.ergoplatform.utils.generators

import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.nodeView.wallet.persistence.{PostponedBlock, RegistryIndex}
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.wallet.boxes.{BoxCertainty, TrackedBox}
import org.scalacheck.Gen

trait WalletGenerators extends ErgoTransactionGenerators {

  private val ergoSettings = ErgoSettings.read()

  def trackedBoxGen: Gen[TrackedBox] = {
    Gen.oneOf(
      unspentOffchainBoxGen,
      unspentOnchainBoxGen,
      spentOffchainBoxGen,
      spentPartiallyOffchainBoxGen,
      spentOnchainBoxGen
    )
  }

  def unspentOffchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
      appId <- Gen.posNum[Short]
    } yield TrackedBox(tx, outIndex, None, ergoBox, certainty, appId)
  }

  def unspentOnchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
      appId <- Gen.posNum[Short]
    } yield TrackedBox(tx, outIndex, Some(height), ergoBox, certainty, appId)
  }

  def spentOffchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
      appId <- Gen.posNum[Short]
    } yield TrackedBox(tx.id, outIndex, None, Some(spendingTx.id), None, ergoBox, certainty, appId)
  }

  def spentPartiallyOffchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
      appId <- Gen.posNum[Short]
    } yield TrackedBox(tx.id, outIndex, Some(height), Some(spendingTx.id), None, ergoBox, certainty, appId)
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
      appId <- Gen.posNum[Short]
    } yield TrackedBox(
      tx.id, outIndex, Some(height), Some(spendingTx.id), Some(spendingHeight), ergoBox, certainty, appId)
  }

  def paymentRequestGen: Gen[PaymentRequest] = {
    for {
      value <- Gen.choose(1L, 100000L)
      assets <- additionalTokensGen
      registers <- additionalRegistersGen
    } yield PaymentRequest(Pay2SAddress(Constants.FalseLeaf), value, assets, registers)
  }

  def assetIssueRequestGen: Gen[AssetIssueRequest] = {
    for {
      amount <- Gen.choose(1L, 100000L)
      name <- Gen.alphaUpperStr
      description <- Gen.alphaLowerStr
      decimals <- Gen.choose(4, 16)
    } yield AssetIssueRequest(Pay2SAddress(Constants.FalseLeaf), amount, name, description, decimals)
  }

  def registryIndexGen: Gen[RegistryIndex] = {
    for {
      height <- Gen.posNum[Int]
      amount <- Gen.choose(1L, 100000L)
      balances <- additionalTokensGen
      uncertain <- Gen.listOf(boxIdGen)
    } yield {
      val encodedBalances = balances.map { case (x1, x2) => encodedTokenId(x1) -> x2 }.toMap
      RegistryIndex(height, amount, encodedBalances, uncertain.map(encodedBoxId))
    }
  }

  def postponedBlockGen: Gen[PostponedBlock] = for {
    height <- Gen.posNum[Int]
    id <- modifierIdGen
    txs <- Gen.listOf(invalidErgoTransactionGen)
  } yield PostponedBlock(id, height, txs)

  private def outIndexGen(tx: ErgoTransaction) = Gen.choose(0: Short, tx.outputCandidates.length.toShort)

  private def heightGen(min: Int = 0) = Gen.choose(min + 1, Integer.MAX_VALUE / 2 + min)

}
