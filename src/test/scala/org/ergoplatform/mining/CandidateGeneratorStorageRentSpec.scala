package org.ergoplatform.mining

import org.ergoplatform.Input
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.settings.Constants.{FalseTree, TrueTree}
import org.ergoplatform.utils.{ErgoCorePropertyTest, StorageRentTestHelpers}
import sigma.interpreter.ProverResult

import scala.util.{Failure, Success}

/**
  * Candidate assembly under the EIP draft "Storage Rent Claims Restricted to the First Transaction of a
  * Block": `CandidateGenerator.collectTxs` never places a storage rent claim, evaluated at the candidate's
  * height, after the first transaction of the candidate.
  */
class CandidateGeneratorStorageRentSpec extends ErgoCorePropertyTest with StorageRentTestHelpers {

  import org.ergoplatform.utils.ErgoCoreTestConstants._

  /** Height of the best block; the candidate is for nextHeight */
  private val tip: Int = StoragePeriod + 2001
  private val nextHeight: Int = tip + 1

  private def claimsAfterFirst(txs: Seq[ErgoTransaction], us: UtxoState): Seq[ErgoTransaction] =
    txs.drop(1).filter(tx => ErgoTransaction.hasStorageRentClaim(tx, tx.inputs.flatMap(i => us.boxById(i.boxId)), nextHeight))

  property("collectTxsPosition: candidate generation never places a rent claim after the first transaction, " +
    "including a pool transaction which becomes a rent claim at nextHeight") {
    val plain = boxAt(TrueTree, tip - 10, seed = 31)
    // `false` script: valid only through the rent branch
    val expired = boxAt(FalseTree, nextHeight - StoragePeriod - 50, seed = 32)
    // StoragePeriod - 1 blocks old for a block at tip (when it was admitted, the tip was tip - 1),
    // exactly StoragePeriod old at nextHeight
    val crossing = boxAt(TrueTree, nextHeight - StoragePeriod, seed = 33)

    val ctx = stateContext(tip, RentPositionVersion, rentSettings, validationSettingsNoIl)
    val us = utxoStateAt(Seq(plain, expired, crossing), None, ctx, rentSettings)
    val upcoming = us.stateContext.simplifiedUpcoming()
    upcoming.currentHeight shouldBe nextHeight
    upcoming.blockVersion shouldBe RentPositionVersion

    val plainTx = ErgoTransaction(IndexedSeq(Input(plain.id, ProverResult.empty)), IndexedSeq(recreated(plain, nextHeight)))
    val claimTx = rentShapedTx(expired, nextHeight)
    val crossingTx = rentShapedTx(crossing, nextHeight)

    ErgoTransaction.hasStorageRentClaim(crossingTx, IndexedSeq(crossing), tip) shouldBe false
    ErgoTransaction.hasStorageRentClaim(crossingTx, IndexedSeq(crossing), nextHeight) shouldBe true
    ErgoTransaction.hasStorageRentClaim(claimTx, IndexedSeq(expired), nextHeight) shouldBe true
    // each of them is valid on its own at nextHeight, so a skip below is due to position only
    Seq(plainTx, claimTx, crossingTx).foreach { tx =>
      us.validateWithCost(tx, upcoming, parameters.maxBlockCost, None) shouldBe a[Success[_]]
    }

    def collect(txs: Seq[ErgoTransaction]): (Seq[ErgoTransaction], Seq[scorex.util.ModifierId]) =
      CandidateGenerator.collectTxs(defaultMinerPk, parameters.maxBlockCost, parameters.maxBlockSize, us, upcoming, txs)

    // (1) the first accepted transaction is not a claim: both claims are skipped and marked invalid
    val (collected1, invalid1) = collect(Seq(plainTx, claimTx, crossingTx))
    collected1 shouldBe Seq(plainTx)
    invalid1 should contain theSameElementsAs Seq(claimTx.id, crossingTx.id)
    claimsAfterFirst(collected1, us) shouldBe empty

    // (2) a claim offered first is placed at t_0; no second claim follows it
    val (collected2, invalid2) = collect(Seq(claimTx, crossingTx, plainTx))
    collected2 shouldBe Seq(claimTx, plainTx)
    invalid2 shouldBe Seq(crossingTx.id)
    claimsAfterFirst(collected2, us) shouldBe empty

    // (3) the collected transactions pass block validation at block version 5, including rule 308
    Seq(collected1, collected2).foreach { txs =>
      ErgoState.execTransactions(txs, upcoming, rentSettings.nodeSettings) { id =>
        us.boxById(id).fold[scala.util.Try[org.ergoplatform.ErgoBox]](Failure(new Exception("box not found")))(Success(_))
      }.isValid shouldBe true
    }
  }

}
