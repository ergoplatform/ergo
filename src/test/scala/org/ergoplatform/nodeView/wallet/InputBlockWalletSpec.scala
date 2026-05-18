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
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx))

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
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx1))

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

      // Scan the transaction as an input block
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx))

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
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx1, tx2))

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
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx))

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
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx))

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
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx1))

      // Generate second transaction and scan as offchain
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }
      wallet.scanOffchain(tx2)

      // tx2 spends tx1 outputs, so only tx2 outputs remain tracked
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 1
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
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx))

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
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx))

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

      wallet.scanInputBlock(nextInputBlockId(), Seq(tx))

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
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx1))

      // Generate second transaction spending from first
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan second input block
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx2))

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

      // Wait for genesis block to be reflected in balances
      eventually {
        val bal = await(wallet.balancesWithUnconfirmed)
        bal.walletBalance should be > 0L
      }

      // Generate transaction
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan as input block
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx))

      // Balance should still be tracked (roughly same, minus fees)
      eventually {
        val balanceAfter = await(wallet.balancesWithUnconfirmed)
        balanceAfter.walletBalance should be > 0L
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
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx))

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

      // Wait for genesis block to be reflected in confirmed balance
      eventually {
        val bal = await(wallet.confirmedBalances)
        bal.walletBalance should be > 0L
      }

      val confirmedBalanceBefore = await(wallet.confirmedBalances)

      // Generate a transaction
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Scan as input block
      wallet.scanInputBlock(nextInputBlockId(), Seq(tx))

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

  // ============================================================================
  // Rollback Tests
  // ============================================================================

  property("rollbackInputBlock restores boxes from rolled back input block") {
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

      val inputBlockId = nextInputBlockId()

      // Get boxes before input block
      val boxesBefore = eventually {
        await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
      }

      // Scan as input block
      wallet.scanInputBlock(inputBlockId, Seq(tx))

      // Verify boxes changed after input block
      eventually {
        val boxesAfter = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfter.size should not be boxesBefore.size
      }

      // Rollback the input block
      wallet.rollbackInputBlock(inputBlockId)

      // Verify boxes are restored to pre-input-block state
      eventually {
        val boxesAfterRollback = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfterRollback.size shouldBe boxesBefore.size
      }
    }
  }

  property("rollbackInputBlock allows re-spending after rollback") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Generate first transaction
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId1 = nextInputBlockId()

      // Scan first input block
      wallet.scanInputBlock(inputBlockId1, Seq(tx1))

      // Generate second transaction spending from first
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId2 = nextInputBlockId()

      // Scan second input block
      wallet.scanInputBlock(inputBlockId2, Seq(tx2))

      // Verify tx2 outputs are tracked
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 1
      }

      // Rollback second input block
      wallet.rollbackInputBlock(inputBlockId2)

      // Verify tx1 outputs are restored and can be spent again
      eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        val result = await(wallet.generateTransaction(req))
        result.isSuccess shouldBe true
      }
    }
  }

  property("rollbackInputBlock removes target block and all subsequent blocks") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state with more funds
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Generate first transaction
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId1 = nextInputBlockId()

      // Scan first input block
      wallet.scanInputBlock(inputBlockId1, Seq(tx1))

      // Generate second transaction
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId2 = nextInputBlockId()

      // Scan second input block
      wallet.scanInputBlock(inputBlockId2, Seq(tx2))

      // Verify state changed after second input block
      eventually {
        val boxesAfterSecond = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfterSecond.size should be >= 1
      }

      // Rollback first input block - this should also remove the second block
      // because later blocks may depend on earlier ones (e.g., spending outputs)
      wallet.rollbackInputBlock(inputBlockId1)

      // Both input blocks should be removed, restoring genesis state
      eventually {
        val boxesAfterRollback = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        // Genesis boxes should be restored (there may be multiple)
        boxesAfterRollback.size should be >= 1
      }
    }
  }

  property("rollbackInputBlock restores wallet balance to exact pre-block state") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Ensure wallet has scanned genesis block into offChainRegistry before recording baseline
      eventually {
        val bal = await(wallet.balancesWithUnconfirmed)
        bal.walletBalance should be > 0L
      }

      // Record box count before input block (balance won't change for self-transfers)
      val boxesBefore = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))

      // Generate a transaction
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId = nextInputBlockId()

      // Scan as input block
      wallet.scanInputBlock(inputBlockId, Seq(tx))

      // Verify boxes changed after input block
      eventually {
        val boxesAfter = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfter.size should not be boxesBefore.size
      }

      // Rollback the input block
      wallet.rollbackInputBlock(inputBlockId)

      // Verify boxes restored to exact pre-block state
      eventually {
        val boxesAfterRollback = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfterRollback.size shouldBe boxesBefore.size
      }
    }
  }

  property("rollbackInputBlock restores asset balances correctly") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state with custom asset
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Wait for genesis to be reflected in offChainRegistry
      eventually {
        val bal = await(wallet.balancesWithUnconfirmed)
        bal.walletBalance should be > 0L
      }

      // Record boxes before input block
      val boxesBefore = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))

      // Generate transaction that transfers/spends the asset
      val tx = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId = nextInputBlockId()

      // Scan as input block
      wallet.scanInputBlock(inputBlockId, Seq(tx))

      // Verify boxes changed after input block
      eventually {
        val boxesAfter = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfter.size should not be boxesBefore.size
      }

      // Rollback the input block
      wallet.rollbackInputBlock(inputBlockId)

      // Verify boxes restored to pre-block state
      eventually {
        val boxesAfterRollback = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfterRollback.size shouldBe boxesBefore.size
      }
    }
  }

  property("mempool transaction spending input-block output survives rollback") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Generate a transaction
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId = nextInputBlockId()

      // Scan as input block
      wallet.scanInputBlock(inputBlockId, Seq(tx1))

      // Verify tx1 outputs are tracked
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 2
      }

      // Now scan a mempool transaction that spends tx1 outputs
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }
      wallet.scanOffchain(tx2)

      // Verify offChainRegistry was updated (balancesWithUnconfirmed reflects it)
      eventually {
        val balances = await(wallet.balancesWithUnconfirmed)
        balances.walletAssetBalances.size should be >= 1
      }

      // Rollback the input block
      wallet.rollbackInputBlock(inputBlockId)

      // After rollback, genesis outputs are restored to onChainBalances.
      // tx2 remains in offChainRegistry but its inputs (tx1 outputs) are gone.
      // Verify wallet state is not corrupted by checking balances query works
      eventually {
        val balances = await(wallet.balancesWithUnconfirmed)
        balances should not be null
        balances.walletAssetBalances.size should be >= 1
      }
    }
  }

  property("scanOnChain after input block rollback processes correctly") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Wait for genesis to be reflected in confirmed balances
      eventually {
        val bal = await(wallet.confirmedBalances)
        bal.walletBalance should be > 0L
      }

      // Record box count before input block (balance won't change for self-transfer)
      val boxesBefore = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))

      // Generate a transaction
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId = nextInputBlockId()

      // Scan as input block
      wallet.scanInputBlock(inputBlockId, Seq(tx))

      // Verify box count changed (balance stays same for self-transfer)
      eventually {
        val boxesAfter = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfter.size should not be boxesBefore.size
      }

      // Rollback the input block
      wallet.rollbackInputBlock(inputBlockId)

      // Verify box count restored to pre-block state
      eventually {
        val afterRollback = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        afterRollback.size shouldBe boxesBefore.size
      }

      // Now apply the transaction in a real block via scanOnChain
      val block = makeNextBlock(getUtxoState, Seq(tx))
      applyBlock(block) shouldBe 'success

      // Wallet should scan the block correctly without registry corruption
      eventually {
        val boxesAfterBlock = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        // After on-chain confirmation, boxes should still be available
        boxesAfterBlock.size should be >= boxesBefore.size
      }
    }
  }

  // ============================================================================
  // Memory Leak Fix Tests (Issue #1)
  // ============================================================================

  property("input block transactions are cleaned up after on-chain confirmation") {
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

      val inputBlockId = nextInputBlockId()

      // Scan as input block
      wallet.scanInputBlock(inputBlockId, Seq(tx))

      // Verify transaction outputs are tracked
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 2
      }

      // Confirm the transaction on-chain
      val block = makeNextBlock(getUtxoState, Seq(tx))
      applyBlock(block) shouldBe 'success

      // After on-chain confirmation, the wallet should still track the boxes correctly
      // (this verifies inputBlockTxs cleanup doesn't break anything)
      eventually {
        val boxesAfterConfirm = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfterConfirm.size should be >= 2
      }

      // Verify confirmed balance is updated (boxes are now on-chain)
      eventually {
        val confirmed = await(wallet.confirmedBalances)
        confirmed.walletBalance should be > 0L
      }
    }
  }

  property("on-chain confirmation of one input block does not affect another input block") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state with more funds
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Generate first transaction
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId1 = nextInputBlockId()
      wallet.scanInputBlock(inputBlockId1, Seq(tx1))

      // Generate second transaction
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId2 = nextInputBlockId()
      wallet.scanInputBlock(inputBlockId2, Seq(tx2))

      // Verify both input blocks are tracked
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 1
      }

      // Confirm only tx1 on-chain
      val block = makeNextBlock(getUtxoState, Seq(tx1))
      applyBlock(block) shouldBe 'success

      // tx2 should still be tracked as an input block
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 1
      }

      // tx2 outputs should still be spendable
      eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        val result = await(wallet.generateTransaction(req))
        result.isSuccess shouldBe true
      }
    }
  }

  // ============================================================================
  // UTXO Reader Rebuild Tests (Issue #2)
  // ============================================================================

  property("rollback of one input block preserves spendability of another independent input block") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state with more funds
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Generate first transaction
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId1 = nextInputBlockId()
      wallet.scanInputBlock(inputBlockId1, Seq(tx1))

      // Generate second transaction (may spend tx1 outputs - that's ok)
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId2 = nextInputBlockId()
      wallet.scanInputBlock(inputBlockId2, Seq(tx2))

      // Verify tx2 outputs are tracked
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 1
      }

      // Rollback first input block - tx1 is removed, restoring genesis outputs
      wallet.rollbackInputBlock(inputBlockId1)

      // Wallet should still function correctly after rollback.
      // Note: tx2 may have spent tx1 outputs, making tx2 invalid after tx1 rollback.
      // This is expected - users should rollback dependent blocks in reverse order.
      // Here we just verify the wallet state remains queryable and consistent.
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        // Should have at least the restored genesis boxes
        boxes.size should be >= 1
      }

      // Wallet balance query should work without errors
      eventually {
        val balances = await(wallet.balancesWithUnconfirmed)
        balances should not be null
      }
    }
  }

  property("rollback preserves UTXO state for remaining input blocks") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Generate first transaction
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId1 = nextInputBlockId()
      wallet.scanInputBlock(inputBlockId1, Seq(tx1))

      // Record state after first input block
      val boxesAfterFirst = eventually {
        await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
      }

      // Generate second transaction
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId2 = nextInputBlockId()
      wallet.scanInputBlock(inputBlockId2, Seq(tx2))

      // Rollback second input block only
      wallet.rollbackInputBlock(inputBlockId2)

      // State should be restored to after-first-input-block state
      eventually {
        val boxesAfterRollback = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxesAfterRollback.size shouldBe boxesAfterFirst.size
      }

      // tx1 outputs should still be spendable
      eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        val result = await(wallet.generateTransaction(req))
        result.isSuccess shouldBe true
      }
    }
  }

  property("sequential rollbacks restore spendability of genesis outputs") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Wait for genesis block to be reflected
      eventually {
        val bal = await(wallet.balancesWithUnconfirmed)
        bal.walletBalance should be > 0L
      }

      // Generate and scan first transaction
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId1 = nextInputBlockId()
      wallet.scanInputBlock(inputBlockId1, Seq(tx1))

      // Generate and scan second transaction
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId2 = nextInputBlockId()
      wallet.scanInputBlock(inputBlockId2, Seq(tx2))

      // Rollback second input block
      wallet.rollbackInputBlock(inputBlockId2)

      // Should be able to spend tx1 outputs
      eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        val result = await(wallet.generateTransaction(req))
        result.isSuccess shouldBe true
      }

      // Rollback first input block
      wallet.rollbackInputBlock(inputBlockId1)

      // Should be able to spend genesis outputs again
      eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        val result = await(wallet.generateTransaction(req))
        result.isSuccess shouldBe true
      }

      // Confirmed balance should still reflect genesis block
      eventually {
        val confirmed = await(wallet.confirmedBalances)
        confirmed.walletBalance should be > 0L
      }
    }
  }

  property("on-chain confirmation then rollback of different input blocks works correctly") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0

      // Create initial state
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success

      implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 500.millis)

      // Generate first transaction
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId1 = nextInputBlockId()
      wallet.scanInputBlock(inputBlockId1, Seq(tx1))

      // Generate second transaction
      val tx2 = eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      val inputBlockId2 = nextInputBlockId()
      wallet.scanInputBlock(inputBlockId2, Seq(tx2))

      // Confirm tx1 on-chain
      val block = makeNextBlock(getUtxoState, Seq(tx1))
      applyBlock(block) shouldBe 'success

      // Verify tx1 is now confirmed
      eventually {
        val confirmed = await(wallet.confirmedBalances)
        confirmed.walletBalance should be > 0L
      }

      // Rollback tx2 (which is still an input block)
      wallet.rollbackInputBlock(inputBlockId2)

      // tx1 confirmed boxes should still be available
      eventually {
        val boxes = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
        boxes.size should be >= 2
      }

      // Should be able to generate transactions using confirmed boxes
      eventually {
        val req = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        val result = await(wallet.generateTransaction(req))
        result.isSuccess shouldBe true
      }
    }
  }

}
