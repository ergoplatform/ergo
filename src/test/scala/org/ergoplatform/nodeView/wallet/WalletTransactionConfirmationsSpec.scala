package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox
import org.ergoplatform.mining.CandidateGenerator
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedMempool
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.wallet.scanning.{EqualsScanningPredicate, ScanRequest, ScanWalletInteraction}
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.wallet.Constants.{MiningScanId, PaymentsScanId}
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId
import sigma.ast.ByteArrayConstant

import scala.concurrent.duration._

/** Exercises the wallet transaction read used by GET /wallet/transactions. */
class WalletTransactionConfirmationsSpec
  extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  import org.ergoplatform.utils.ErgoNodeTestConstants.genesisEmissionBox

  private implicit val patience: PatienceConfig = PatienceConfig(5.seconds, 300.millis)

  private def read(minConf: Int, maxConf: Int, maxHeight: Int = Int.MaxValue)
                  (implicit w: WalletFixture): Set[ModifierId] =
    await(wallet.filteredScanTransactions(List(PaymentsScanId, MiningScanId), 0, maxHeight,
      minConf, maxConf, includeUnconfirmed = false)).map(_.wtx.id).toSet

  /** Runs test(t1, t2) once the wallet has scanned both blocks. */
  private def withTwoTransactions(test: (ModifierId, ModifierId) => WalletFixture => Unit): Unit =
    withFixture { implicit w =>
      val pk = getPublicKeys.head.pubkey
      val block1 = makeGenesisBlock(pk)
      applyBlock(block1) shouldBe 'success
      val tx1 = block1.transactions.head
      val emissionBox = tx1.outputs.find(_.ergoTree == genesisEmissionBox.ergoTree).get
      val tx2 = CandidateGenerator.collectRewards(Some(emissionBox), block1.height, Seq.empty, pk,
        getUtxoState.stateContext).head
      val block2 = makeNextBlock(getUtxoState, Seq(tx2))
      applyBlock(block2) shouldBe 'success
      eventually {
        getHistory.bestFullBlockOpt.map(_.id) shouldBe Some(block2.id)
        val all = await(wallet.filteredScanTransactions(List(PaymentsScanId, MiningScanId), 0, Int.MaxValue,
          0, Int.MaxValue, includeUnconfirmed = false))
        all.map(_.wtx.id).toSet shouldBe Set(tx1.id, tx2.id)
        all.map(t => t.wtx.id -> t.numConfirmations).toMap shouldBe Map(tx1.id -> 1, tx2.id -> 0)
      }
      test(tx1.id, tx2.id)(w)
    }

  property("minConfirmations = 1 returns the confirmed transaction only") {
    withTwoTransactions { (t1, _) => implicit w => read(1, Int.MaxValue) shouldBe Set(t1) }
  }

  property("minConfirmations = 1 and maxConfirmations = 1 return the confirmed transaction only") {
    withTwoTransactions { (t1, _) => implicit w => read(1, 1) shouldBe Set(t1) }
  }

  property("minConfirmations above every transaction's confirmations returns nothing") {
    withTwoTransactions { (_, _) => implicit w => read(2, Int.MaxValue) shouldBe empty }
  }

  property("minConfirmations above the chain height returns nothing") {
    withTwoTransactions { (_, _) => implicit w => read(3, Int.MaxValue) shouldBe empty }
  }

  property("no confirmation filter returns both transactions") {
    withTwoTransactions { (t1, t2) => implicit w => read(0, Int.MaxValue) shouldBe Set(t1, t2) }
  }

  property("maxConfirmations = 0 returns the tip transaction only") {
    withTwoTransactions { (_, t2) => implicit w => read(0, 0) shouldBe Set(t2) }
  }

  property("maxConfirmations = 1 returns both transactions") {
    withTwoTransactions { (t1, t2) => implicit w => read(0, 1) shouldBe Set(t1, t2) }
  }

  property("maxInclusionHeight = -1 preserves the unlimited confirmed read") {
    withTwoTransactions { (t1, t2) => implicit w =>
      read(0, Int.MaxValue, maxHeight = -1) shouldBe Set(t1, t2)
    }
  }

  property("maxInclusionHeight = -1 still applies a positive confirmation minimum") {
    withTwoTransactions { (t1, _) => implicit w =>
      read(1, Int.MaxValue, maxHeight = -1) shouldBe Set(t1)
    }
  }

  property("inverted heights and the unlimited sentinel exclude a visible mempool transaction") {
    withTwoTransactions { (_, t2Id) => implicit w =>
      val tip = getHistory.bestFullBlockOpt.get
      val tx2 = tip.transactions.find(_.id == t2Id).get
      val emissionBox = tx2.outputs.find(_.ergoTree == genesisEmissionBox.ergoTree).get
      val address = getPublicKeys.head
      val pk = address.pubkey
      val tx3 = CandidateGenerator.collectRewards(Some(emissionBox), tip.height, Seq.empty, pk,
        getUtxoState.stateContext).head
      val minerBox = tx3.outputs.find(_.ergoTree != genesisEmissionBox.ergoTree).get
      val scanId = await(wallet.addScan(ScanRequest(
        "mempool confirmation window",
        EqualsScanningPredicate(ErgoBox.R1, ByteArrayConstant(minerBox.ergoTree.bytes)),
        Some(ScanWalletInteraction.Off),
        Some(false)
      ))).response.get.scanId

      val mempool = ErgoMemPool.empty(w.settings).put(Seq(UnconfirmedTransaction(tx3, None)))
      wallet.walletActor ! ChangedMempool(mempool)
      eventually {
        val visible = await(wallet.filteredScanTransactions(List(scanId), 0, Int.MaxValue,
          0, Int.MaxValue, includeUnconfirmed = true))
        visible.map(_.wtx.id) should contain (tx3.id)
      }

      val unlimitedSentinel = await(wallet.filteredScanTransactions(List(scanId), 0, -1,
        0, Int.MaxValue, includeUnconfirmed = true))
      withClue("maxInclusionHeight=-1: ") { unlimitedSentinel shouldBe empty }

      val inverted = await(wallet.filteredScanTransactions(List(scanId), tip.height + 2, tip.height + 1,
        0, Int.MaxValue, includeUnconfirmed = true))
      withClue("inverted above-tip heights: ") { inverted shouldBe empty }
    }
  }
}
