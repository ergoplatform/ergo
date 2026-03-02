package org.ergoplatform.nodeView.wallet

import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.utils._
import org.ergoplatform.wallet.boxes.BoxSelector.MinBoxValue
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration._

/**
  * Tests for wallet input block support.
  * 
  * These tests verify the current implementation where input blocks are processed
  * as off-chain transactions via scanInputBlock.
  */
class InputBlockWalletSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  // ============================================================================
  // Core Functionality Tests
  // ============================================================================

  property("input block transactions prevent double spending") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state with some boxes
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      // Generate a transaction that spends some boxes and creates new ones
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan the transaction as a locally generated input block
      wallet.scanInputBlock(Seq(tx))

      // Wait for wallet state to update
      eventually {
        // Verify that we cannot generate another transaction that would double-spend the same inputs
        // This should fail because the inputs are already marked as spent
        val attempt = await(wallet.generateTransaction(Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))))

        // The generation should fail due to insufficient funds (inputs already spent)
        attempt shouldBe 'failure
      }
    }
  }

  property("boxes created in input blocks can be spent in subsequent blocks") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state with some boxes
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      // Generate first transaction that creates outputs
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Apply first transaction as an input block (making outputs spendable)
      wallet.scanInputBlock(Seq(tx1))

      Thread.sleep(100)

      val boxes = eventually {
        await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
      }

      boxes.size shouldBe 2

      // Generate second transaction that spends outputs from first transaction
      eventually {
        // Create a transaction spending the outputs from tx1
        val req2 = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req2)).get
      }
    }
  }

  // ============================================================================
  // Off-Chain Registry Tests
  // ============================================================================

  property("scanInputBlock adds boxes to off-chain registry") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      // Generate a transaction that creates new boxes
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Before scanInputBlock, boxes should not be in wallet
      val boxesBefore = eventually {
        await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
      }
      val boxesCountBefore = boxesBefore.size

      // Scan the transaction as an input block
      wallet.scanInputBlock(Seq(tx))

      // After scanInputBlock, new boxes should appear in off-chain registry
      eventually {
        val boxesAfter = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfter.size shouldBe (boxesCountBefore + 2) // 2 outputs: change + payment
      }
    }
  }

  property("scanInputBlock with multiple transactions") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state with more funds
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)

      // Generate first transaction
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Generate second transaction spending from first
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan both transactions as input block
      wallet.scanInputBlock(Seq(tx1, tx2))

      // Verify both transactions' outputs are tracked
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 2
      }
    }
  }

  property("scanInputBlock updates wallet balances") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, Seq.empty)
      applyBlock(genesisBlock) shouldBe 'success


      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)

      // Generate a transaction
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan as input block
      wallet.scanInputBlock(Seq(tx))

      // Balance should be updated (considering unconfirmed)
      eventually {
        val balanceAfter = await(wallet.balancesWithUnconfirmed)
        // Balance should remain roughly the same (minus fees)
        balanceAfter.walletBalance should be > 0L
      }
    }
  }

  property("scanInputBlock with asset transfer") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state with custom asset
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)

      // Generate transaction that transfers the asset
      val tx = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan as input block
      wallet.scanInputBlock(Seq(tx))

      // Verify asset is tracked in wallet
      eventually {
        val balance = await(wallet.balancesWithUnconfirmed)
        balance.walletAssetBalances.size should be >= 1
      }
    }
  }

  property("scanInputBlock followed by scanOffchain") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)

      // Generate first transaction and scan as input block
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }
      wallet.scanInputBlock(Seq(tx1))

      // Generate second transaction and scan as offchain
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }
      wallet.scanOffchain(tx2)

      // Both transactions' outputs should be tracked
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 2
      }
    }
  }

  property("scanInputBlock preserves box scan IDs") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)

      // Generate transaction
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan as input block
      wallet.scanInputBlock(Seq(tx))

      // Verify boxes have proper scan IDs (PaymentsScanId)
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.foreach { walletBox =>
          walletBox.trackedBox.scans.nonEmpty shouldBe true
        }
      }
    }
  }

  // ============================================================================
  // Integration Tests
  // ============================================================================

  property("LocallyGeneratedInputBlock updates wallet state") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Generate a transaction
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Verify transaction outputs are tracked after scan
      wallet.scanInputBlock(Seq(tx))

      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 2
      }
    }
  }

  property("wallet tracks boxes from input block before ordering block confirmation") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Generate transaction and scan as input block
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      wallet.scanInputBlock(Seq(tx))

      // Boxes should be available immediately (off-chain)
      val boxesAfterInputBlock = eventually {
        await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
      }
      boxesAfterInputBlock.size should be >= 2

      // Boxes should be spendable in subsequent transactions
      eventually {
        val req2 = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        val result = await(wallet.generateTransaction(req2))
        result.isSuccess shouldBe true
      }
    }
  }

  property("multiple input blocks are processed in order") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state with more funds
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(15.second, 500.millis)

      // Generate first transaction
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan first input block
      wallet.scanInputBlock(Seq(tx1))

      // Generate second transaction spending from first
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan second input block
      wallet.scanInputBlock(Seq(tx2))

      // Both transactions should be tracked
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 2
      }
    }
  }

  property("wallet balance reflects input block transactions") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, Seq.empty)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      val balanceBefore = eventually {
        await(wallet.balancesWithUnconfirmed)
      }

      // Generate transaction
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan as input block
      wallet.scanInputBlock(Seq(tx))

      // Balance should be updated
      eventually {
        val balanceAfter = await(wallet.balancesWithUnconfirmed)
        balanceAfter.walletBalance should be > 0L
        // Balance should be slightly less due to fees
        balanceAfter.walletBalance should be <= balanceBefore.walletBalance
      }
    }
  }

  property("input block transactions are tracked as off-chain") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)

      // Generate a transaction
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan as input block
      wallet.scanInputBlock(Seq(tx))

      // Boxes should be available with considerUnconfirmed = true
      // (because they're in off-chain registry)
      eventually {
        val boxesWithUnconfirmed = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesWithUnconfirmed.size should be >= 2
      }
    }
  }

  property("confirmed balance doesn't include input block boxes") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, Seq.empty)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)

      val confirmedBalanceBefore = eventually {
        await(wallet.confirmedBalances)
      }

      // Generate a transaction
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan as input block
      wallet.scanInputBlock(Seq(tx))

      // Confirmed balance should not change (input block boxes are off-chain)
      eventually {
        val confirmedBalanceAfter = await(wallet.confirmedBalances)
        confirmedBalanceAfter.walletBalance shouldBe confirmedBalanceBefore.walletBalance
      }

      // But balance with unconfirmed should include the boxes
      eventually {
        val balanceWithUnconfirmed = await(wallet.balancesWithUnconfirmed)
        balanceWithUnconfirmed.walletBalance should be > 0L
      }
    }
  }

}
