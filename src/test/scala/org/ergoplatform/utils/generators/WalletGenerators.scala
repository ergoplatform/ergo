package org.ergoplatform.utils.generators

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.nodeView.wallet.persistence.{PostponedBlock, RegistryIndex}
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.wallet.boxes.{BoxCertainty, TrackedBox}
import org.ergoplatform._
import org.scalacheck.Gen
import scorex.crypto.authds.ADKey
import scorex.util.{ModifierId, idToBytes}

trait WalletGenerators extends ErgoTransactionGenerators {

  private val ergoSettings = ErgoSettings.read()
  private implicit val ergoAddressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)

  def trackedAddressGen: Gen[ErgoAddress] = proveDlogGen.map(P2PKAddress.apply)

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
