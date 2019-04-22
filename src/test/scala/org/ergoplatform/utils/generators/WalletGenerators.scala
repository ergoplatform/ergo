package org.ergoplatform.utils.generators

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.wallet.boxes.{BoxCertainty, TrackedBox}
import org.ergoplatform.{ErgoAddressEncoder, Pay2SAddress}
import org.scalacheck.Gen

trait WalletGenerators extends ErgoTransactionGenerators {

  private val ergoSettings = ErgoSettings.read(None)
  private implicit val ergoAddressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)

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
    } yield TrackedBox(tx, outIndex, None, ergoBox, certainty)
  }

  def unspentOnchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
    } yield TrackedBox(tx, outIndex, Some(height), ergoBox, certainty)
  }

  def spentOffchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
    } yield TrackedBox(tx.id, outIndex, None, Some(spendingTx.id), None, ergoBox, certainty)
  }

  def spentPartiallyOffchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      ergoBox <- Gen.oneOf(boxes)
      certainty <- Gen.oneOf(BoxCertainty.Certain, BoxCertainty.Uncertain)
    } yield TrackedBox(tx.id, outIndex, Some(height), Some(spendingTx.id), None, ergoBox, certainty)
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
    } yield TrackedBox(tx.id, outIndex, Some(height), Some(spendingTx.id), Some(spendingHeight), ergoBox, certainty)
  }

  def paymentRequestGen: Gen[PaymentRequest] = {
    for {
      value <- Gen.choose(1L, 100000L)
      assets <- Gen.option(additionalTokensGen)
      registers <- Gen.option(additionalRegistersGen)
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

  private def outIndexGen(tx: ErgoTransaction) = Gen.choose(0: Short, tx.outputCandidates.length.toShort)

  private def heightGen(min: Int = 0) = Gen.choose(min + 1, Integer.MAX_VALUE / 2 + min)

}
