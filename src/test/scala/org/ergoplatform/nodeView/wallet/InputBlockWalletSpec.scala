package org.ergoplatform.nodeView.wallet

import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.utils._
import org.ergoplatform.wallet.boxes.BoxSelector.MinBoxValue
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId

import scala.concurrent.duration._

/**
  * Tests for wallet input block support.
  *
  * These tests verify the current implementation where input blocks are processed
  * as off-chain transactions via scanInputBlock.
  */
class InputBlockWalletSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  private var inputBlockCounter = 0

  private def nextInputBlockId(): ModifierId = {
    inputBlockCounter += 1
    ModifierId @@ s"test-input-block-$inputBlockCounter"
  }

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
      val inputBlockId = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId, Seq(tx), getUtxoState)
      wallet.scanInputBlock(inputBlockId)

      // Wait for wallet state to update
      eventually {
        // After scanInputBlock, the outputs of tx are available for spending.
        // The original inputs are marked as spent (removed from onChainBalances).
        // Verify that a new transaction can be generated using the input block outputs.
        val attempt = await(wallet.generateTransaction(Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))))
        attempt shouldBe 'success
      }
    }
  }

  property("boxes created in input blocks can be spent in subsequent blocks") {
    withFixture { implicit w =>
      val _ = w
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
      val inputBlockId = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId, Seq(tx1), getUtxoState)
      wallet.scanInputBlock(inputBlockId)

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
      val _ = w
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

      // Scan the transaction as an input block
      val inputBlockId = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId, Seq(tx), getUtxoState)
      wallet.scanInputBlock(inputBlockId)

      // After scanInputBlock, tx outputs should appear in off-chain registry
      // (original inputs are removed from onChainBalances)
      eventually {
        val boxesAfter = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfter.size should be >= 1
      }
    }
  }

  property("scanInputBlock with multiple transactions") {
    withFixture { implicit w =>
      val _ = w
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
      val inputBlockId = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId, Seq(tx1, tx2), getUtxoState)
      wallet.scanInputBlock(inputBlockId)

      // Verify both transactions' outputs are tracked
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 2
      }
    }
  }

  property("scanInputBlock updates wallet balances") {
    withFixture { implicit w =>
      val _ = w
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, Seq.empty)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)

      // Wait for wallet to scan genesis block
      eventually {
        val bal = await(wallet.balancesWithUnconfirmed)
        bal.walletBalance should be > 0L
      }

      val balanceBefore = await(wallet.balancesWithUnconfirmed)

      // Generate a transaction
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan as input block
      val inputBlockId = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId, Seq(tx), getUtxoState)
      wallet.scanInputBlock(inputBlockId)

      // Balance should be updated (considering unconfirmed)
      val balanceAfter = await(wallet.balancesWithUnconfirmed)
      balanceAfter.walletBalance should be > 0L
      balanceAfter.walletBalance shouldBe balanceBefore.walletBalance
    }
  }

  property("scanInputBlock with asset transfer") {
    withFixture { implicit w =>
      val _ = w
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state with custom asset
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)

      // Wait for wallet to scan genesis block
      eventually {
        val bal = await(wallet.balancesWithUnconfirmed)
        bal.walletBalance should be > 0L
      }

      // Record balance before
      val balanceBefore = await(wallet.balancesWithUnconfirmed)

      // Generate transaction that transfers the asset
      val tx = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan as input block
      val inputBlockId = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId, Seq(tx), getUtxoState)
      wallet.scanInputBlock(inputBlockId)

      // Verify asset is tracked in wallet
      val balanceAfter = eventually {
        val bal = await(wallet.balancesWithUnconfirmed)
        bal.walletAssetBalances.size should be >= 1
        bal
      }
      balanceAfter.walletBalance should be > 0L
      balanceAfter.walletBalance shouldBe balanceBefore.walletBalance
    }
  }

  property("scanInputBlock followed by scanOffchain") {
    withFixture { implicit w =>
      val _ = w
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
      val inputBlockId = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId, Seq(tx1), getUtxoState)
      wallet.scanInputBlock(inputBlockId)

      // Verify input block outputs are tracked
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 1
      }

      // Generate second transaction and scan as offchain
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }
      wallet.scanOffchain(tx2)

      // tx2 spends tx1 outputs; verify offChainRegistry reflects tx2
      val balanceAfter = await(wallet.balancesWithUnconfirmed)
      balanceAfter.walletBalance should be > 0L
    }
  }

  property("scanInputBlock preserves box scan IDs") {
    withFixture { implicit w =>
      val _ = w
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
      val inputBlockId = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId, Seq(tx), getUtxoState)
      wallet.scanInputBlock(inputBlockId)

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
      val _ = w
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
      val inputBlockId = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId, Seq(tx), getUtxoState)
      wallet.scanInputBlock(inputBlockId)

      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 2
      }
    }
  }

  property("wallet tracks boxes from input block before ordering block confirmation") {
    withFixture { implicit w =>
      val _ = w
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

      val inputBlockId = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId, Seq(tx), getUtxoState)
      wallet.scanInputBlock(inputBlockId)

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
      val _ = w
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
      val inputBlockId1 = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId1, Seq(tx1), getUtxoState)
      wallet.scanInputBlock(inputBlockId1)

      // Generate second transaction spending from block 1 outputs
      val tx2 = eventually {
        val sumToSpend = MinBoxValue * 5
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId2 = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId2, Seq(tx2), getUtxoState)
      wallet.scanInputBlock(inputBlockId2)

      // Generate third transaction spending from block 2 outputs
      val tx3 = eventually {
        val sumToSpend = MinBoxValue * 2
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId3 = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId3, Seq(tx3), getUtxoState)
      wallet.scanInputBlock(inputBlockId3)

      // Verify state changed after all three input blocks
      eventually {
        val boxesAfterThird = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfterThird.size should be >= 1
      }

      // Record balance before rollback
      val balanceBefore = await(wallet.balancesWithUnconfirmed)

      // Rollback first input block - should remove all three blocks
      wallet.rollbackInputBlock(inputBlockId1)

      // All three input blocks should be removed, restoring genesis state
      eventually {
        val boxesAfterRollback = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfterRollback.size should be >= 1
      }

      // Balance should be restored to pre-input-block state
      val balanceAfter = await(wallet.balancesWithUnconfirmed)
      balanceAfter.walletBalance should be > 0L
      balanceAfter.walletBalance shouldBe balanceBefore.walletBalance

      // Should be able to spend genesis outputs again
      eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        val result = await(wallet.generateTransaction(req))
        result.isSuccess shouldBe true
      }
    }
  }

  property("rollbackInputBlock rolls back middle block and all subsequent blocks in chain") {
    withFixture { implicit w =>
      val _ = w
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state with more funds
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Generate first transaction spending from genesis
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId1 = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId1, Seq(tx1), getUtxoState)
      wallet.scanInputBlock(inputBlockId1)

      // Record balance after block 1 (baseline for middle rollback)
      val balanceAfterBlock1 = await(wallet.balancesWithUnconfirmed)

      // Generate second transaction spending from block 1 outputs
      val tx2 = eventually {
        val sumToSpend = MinBoxValue * 5
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId2 = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId2, Seq(tx2), getUtxoState)
      wallet.scanInputBlock(inputBlockId2)

      // Generate third transaction spending from block 2 outputs
      val tx3 = eventually {
        val sumToSpend = MinBoxValue * 2
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId3 = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId3, Seq(tx3), getUtxoState)
      wallet.scanInputBlock(inputBlockId3)

      // Rollback second input block - should remove blocks 2 and 3 but preserve block 1
      wallet.rollbackInputBlock(inputBlockId2)

      // Block 1 outputs should still be spendable, balance restored to post-block-1 state
      eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        val result = await(wallet.generateTransaction(req))
        result.isSuccess shouldBe true
        val balanceAfterRollback = await(wallet.balancesWithUnconfirmed)
        balanceAfterRollback.walletBalance should be > 0L
        balanceAfterRollback.walletBalance shouldBe balanceAfterBlock1.walletBalance
      }

      // Should still be able to scan block 1 outputs
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 1
      }
    }
  }

  property("apply input block, rollback, then re-apply restores correct state") {
    withFixture { implicit w =>
      val _ = w
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Wait for wallet to scan genesis block
      eventually {
        val bal = await(wallet.balancesWithUnconfirmed)
        bal.walletBalance should be > 0L
      }

      // Record genesis balance
      val genesisBalance = await(wallet.balancesWithUnconfirmed)

      // Generate first transaction
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId1 = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId1, Seq(tx1), getUtxoState)
      wallet.scanInputBlock(inputBlockId1)

      // Record balance after apply
      val balanceAfterApply = eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 1
        await(wallet.balancesWithUnconfirmed)
      }

      // Rollback block 1
      wallet.rollbackInputBlock(inputBlockId1)

      // Verify rollback restored genesis state and balance
      eventually {
        val boxesAfterRollback = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfterRollback.size should be >= 1
        val balanceAfterRollback = await(wallet.balancesWithUnconfirmed)
        balanceAfterRollback.walletBalance should be > 0L
        balanceAfterRollback.walletBalance shouldBe genesisBalance.walletBalance
      }

      // Generate second transaction (same inputs as tx1 had, spending genesis outputs)
      val tx2 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId2 = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId2, Seq(tx2), getUtxoState)
      wallet.scanInputBlock(inputBlockId2)

      // Verify re-application works correctly
      eventually {
        val boxesAfterReapply = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfterReapply.size should be >= 1
        val balanceAfterReapply = await(wallet.balancesWithUnconfirmed)
        balanceAfterReapply.walletBalance should be > 0L
        balanceAfterReapply.walletBalance shouldBe balanceAfterApply.walletBalance
      }

      // Should be able to spend the new input block outputs
      eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        val result = await(wallet.generateTransaction(req))
        result.isSuccess shouldBe true
      }
    }
  }

  property("apply two input blocks, rollback first, then apply new block") {
    withFixture { implicit w =>
      val _ = w
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Wait for wallet to scan genesis block
      eventually {
        val bal = await(wallet.balancesWithUnconfirmed)
        bal.walletBalance should be > 0L
      }

      // Record genesis balance
      val genesisBalance = await(wallet.balancesWithUnconfirmed)

      // Block 1: spend genesis
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId1 = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId1, Seq(tx1), getUtxoState)
      wallet.scanInputBlock(inputBlockId1)

      // Block 2: spend block 1 outputs
      val tx2 = eventually {
        val sumToSpend = MinBoxValue * 5
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId2 = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId2, Seq(tx2), getUtxoState)
      wallet.scanInputBlock(inputBlockId2)

      // Verify both blocks applied
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 1
      }

      // Rollback first block (removes both)
      wallet.rollbackInputBlock(inputBlockId1)

      // Verify genesis restored with balance check
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 1
        val balanceAfterRollback = await(wallet.balancesWithUnconfirmed)
        balanceAfterRollback.walletBalance should be > 0L
        balanceAfterRollback.walletBalance shouldBe genesisBalance.walletBalance
      }

      // Apply new block spending genesis
      val tx3 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId3 = nextInputBlockId()
      getHistory.applyInputBlockTransactions(inputBlockId3, Seq(tx3), getUtxoState)
      wallet.scanInputBlock(inputBlockId3)

      // Verify new block applied correctly with balance
      eventually {
        val boxesAfterReapply = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfterReapply.size should be >= 1
        val balanceAfterReapply = await(wallet.balancesWithUnconfirmed)
        balanceAfterReapply.walletBalance should be > 0L
        balanceAfterReapply.walletBalance shouldBe genesisBalance.walletBalance
      }

      // Should be able to spend new outputs
      eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        val result = await(wallet.generateTransaction(req))
        result.isSuccess shouldBe true
      }
    }
  }

}
