package org.ergoplatform.nodeView.mempool

import org.ergoplatform.Input
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.settings.Constants.{FalseTree, TrueTree}
import org.ergoplatform.utils.{ErgoCorePropertyTest, StorageRentTestHelpers}
import sigma.interpreter.ProverResult

/**
  * Mempool policy of the EIP draft "Storage Rent Claims Restricted to the First Transaction of a Block":
  * with `declineStorageRentClaims` (default true), `ErgoMemPool.process` declines a transaction spending an
  * input as a storage rent claim at the height of the next block. The policy is not gated on block version.
  */
class ErgoMemPoolStorageRentSpec extends ErgoCorePropertyTest with StorageRentTestHelpers {

  import org.ergoplatform.utils.ErgoCoreTestConstants.validationSettingsNoIl

  /** Height of the best block of the state; transactions are evaluated for the next block, at tip + 1 */
  private val tip: Int = StoragePeriod + 3001

  private val DeclineReason = "Mempool policy declines a storage rent claim"

  private def declinedByPolicy(outcome: ProcessingOutcome): Boolean = outcome match {
    case d: ProcessingOutcome.Declined => Option(d.e.getMessage).exists(_.contains(DeclineReason))
    case _ => false
  }

  property("mempoolDeclines: ErgoMemPool.process declines a rent claim with declineStorageRentClaims = true, " +
    "and accepts it with the setting false") {
    // `false` script: an Accepted outcome can only come from the rent branch
    val expired = boxAt(FalseTree, tip + 1 - StoragePeriod, seed = 21) // exactly StoragePeriod old at tip + 1
    val almost = boxAt(TrueTree, tip + 2 - StoragePeriod, seed = 22) // one block short at tip + 1
    val plain = boxAt(TrueTree, tip - 5, seed = 23)

    val ctx = stateContext(tip, Header.Interpreter60Version, rentSettings, validationSettingsNoIl)
    val us = utxoStateAt(Seq(expired, almost, plain), None, ctx, rentSettings)
    us.stateContext.currentHeight shouldBe tip

    val claim = rentShapedTx(expired, tip + 1)
    ErgoTransaction.hasStorageRentClaim(claim, IndexedSeq(expired), tip + 1) shouldBe true

    // setting true (the default)
    rentSettings.nodeSettings.declineStorageRentClaims shouldBe true
    val (declinedPool, declined) = ErgoMemPool.empty(rentSettings).process(UnconfirmedTransaction(claim, None), us)
    declined shouldBe a[ProcessingOutcome.Declined]
    declinedByPolicy(declined) shouldBe true
    declinedPool.size shouldBe 0
    declinedPool.isInvalidated(claim.id) shouldBe true

    // setting false: the same transaction is accepted, so the decline above is the policy and nothing else
    val off = rentSettings.copy(nodeSettings = rentSettings.nodeSettings.copy(declineStorageRentClaims = false))
    val (acceptedPool, accepted) = ErgoMemPool.empty(off).process(UnconfirmedTransaction(claim, None), us)
    accepted shouldBe a[ProcessingOutcome.Accepted]
    acceptedPool.size shouldBe 1

    // no false positives with the setting true: a rent-claim-shaped input one block short of expiry,
    // and an ordinary spend
    val almostTx = rentShapedTx(almost, tip + 1)
    ErgoTransaction.hasStorageRentClaim(almostTx, IndexedSeq(almost), tip + 1) shouldBe false
    ErgoMemPool.empty(rentSettings).process(UnconfirmedTransaction(almostTx, None), us)._2 shouldBe
      a[ProcessingOutcome.Accepted]

    val plainTx = ErgoTransaction(IndexedSeq(Input(plain.id, ProverResult.empty)), IndexedSeq(recreated(plain, tip + 1)))
    ErgoMemPool.empty(rentSettings).process(UnconfirmedTransaction(plainTx, None), us)._2 shouldBe
      a[ProcessingOutcome.Accepted]
  }

}
