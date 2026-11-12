package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration._

class UtxoSetScannerSpecification extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private def withUtxoBootstrapFixture[T](test: WalletFixture => T): T = {
    val utxoBootstrapSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)
      )
    )
    new WalletFixture(utxoBootstrapSettings, parameters, getCurrentView(_).vault).apply(test)
  }

  property("UtxoSetScanner scans UTXO set when synced and handles wallet rescan") {
    withUtxoBootstrapFixture { implicit w =>
      val scannerRef = UtxoSetScanner(w.nodeViewHolderRef, depth = 3)(w.actorSystem)
      try {
        val address = getPublicKeys.head
        val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
        val expectedGenesisBalance = balanceAmount(boxesAvailable(genesisBlock, address.pubkey))

        applyBlock(genesisBlock) shouldBe 'success

        implicit val patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 100.millis)
        eventually {
          val balances = getConfirmedBalances
          balances.walletBalance shouldBe expectedGenesisBalance
          balances.height shouldBe genesisBlock.height
        }

        // Apply a subsequent block after UTXO set scan completed
        val spendAmt = expectedGenesisBalance / 2
        val req = Seq(PaymentRequest(address, spendAmt, Array.empty, Map.empty))
        val tx = await(wallet.generateTransaction(req)).get
        val block2 = makeNextBlock(getUtxoState, Seq(tx))
        applyBlock(block2) shouldBe 'success

        val balanceAfterBlock2 = eventually {
          val balances = getConfirmedBalances
          balances.height shouldBe block2.height
          balances.walletBalance should be > 0L
          balances.walletBalance
        }

        // Trigger wallet rescan with utxoBootstrap = true
        await(wallet.rescanWallet(0)) shouldBe 'success

        eventually {
          val rescannedBalances = getConfirmedBalances
          rescannedBalances.height shouldBe block2.height
          rescannedBalances.walletBalance shouldBe balanceAfterBlock2
        }
      } finally {
        w.actorSystem.stop(scannerRef)
      }
    }
  }

}
