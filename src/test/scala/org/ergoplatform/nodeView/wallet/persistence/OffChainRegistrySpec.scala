package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.ErgoBox
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedBoxId, encodedBoxId}
import org.ergoplatform.nodeView.wallet.scanning.{EqualsScanningPredicate, Scan, ScanWalletInteraction}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.WalletTestOps
import org.ergoplatform.wallet.Constants
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.ast.ByteArrayConstant

import scala.collection.immutable.TreeSet
import scala.util.Random


class OffChainRegistrySpec
  extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with WalletTestOps {
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 5, sizeRange = 10)

  //registry.updateOnTransaction is called when offchain transaction comes
  it should "calculate indexes correctly on offchain transaction" in {
    forAll(Gen.listOf(trackedBoxGen)) { boxes =>
      //apply transaction outputs to empty offchain registry
      var registry = OffChainRegistry.empty.updateOnTransaction(boxes, Seq.empty, Seq.empty)
      val balance = balanceAmount(boxes.map(_.box))
      val assetsBalance = assetAmount(boxes.map(_.box))
      registry.digest.walletBalance shouldEqual balance
      registry.digest.walletAssetBalances.toMap shouldEqual assetsBalance.toMap

      //spend all the outputs
      registry = registry.updateOnTransaction(Seq.empty, boxes.map(EncodedBoxId @@@ _.boxId), Seq.empty)
      registry.digest.walletBalance shouldEqual 0
      registry.digest.walletAssetBalances shouldEqual Seq.empty


      //check remove-offchain flag
      // Only test scanIds > PaymentsScanId because single-scan boxes with scanId <= PaymentsScanId
      // are always removed by regardless of removeOffchain flag
      boxes.filter(_.scans.size > 1).flatMap(_.scans).find(id => id != Constants.PaymentsScanId && id > Constants.PaymentsScanId).map { scanId =>
        val p = EqualsScanningPredicate(ErgoBox.R1, ByteArrayConstant(TrueTree.bytes))
        val scan = Scan(scanId, "_", p, ScanWalletInteraction.Off, removeOffchain = false)
        val filtered = boxes.filter(tb => tb.scans.contains(scanId))

        val fbalance = balanceAmount(filtered.map(_.box))
        val fassetsBalance = assetAmount(filtered.map(_.box))

        registry = registry.updateOnTransaction(filtered, Seq.empty, Seq.empty)
        registry.digest.walletBalance shouldEqual fbalance
        registry.digest.walletAssetBalances.toMap shouldEqual fassetsBalance.toMap

        registry = registry.updateOnTransaction(Seq.empty, filtered.map(EncodedBoxId @@@ _.boxId), Seq(scan))
        registry.digest.walletBalance shouldEqual fbalance
        registry.digest.walletAssetBalances.toMap shouldEqual fassetsBalance.toMap

        val scan2 = Scan(scanId, "_", p, ScanWalletInteraction.Off, removeOffchain = true)
        registry = registry.updateOnTransaction(Seq.empty, filtered.map(EncodedBoxId @@@ _.boxId), Seq(scan2))
        registry.digest.walletBalance shouldEqual 0
        registry.digest.walletAssetBalances shouldEqual Seq.empty
      }
    }
  }

  //registry.updateOnTransaction is called when a block comes
  it should "calculate indexes correctly on a block" in {
    forAll(Gen.listOf(trackedBoxGen)) { boxes =>
      val height = Random.nextInt(500) + 1

      //apply block to empty registry
      val registry = OffChainRegistry.empty.updateOnBlock(height, boxes, boxes.map(tb => encodedBoxId(tb.box.id)).to[TreeSet])
      val balance = balanceAmount(boxes.map(_.box))
      val assetsBalance = assetAmount(boxes.map(_.box))
      registry.height shouldEqual height
      registry.digest.walletBalance shouldEqual balance
      registry.digest.walletAssetBalances.toMap shouldEqual assetsBalance.toMap

      //a block coming is not making any offchain box on-chain
      val registry2 = OffChainRegistry.empty.updateOnBlock(height, boxes, TreeSet.empty)
      registry2.height shouldEqual height
      registry2.digest.walletBalance shouldEqual balance
      registry2.digest.walletAssetBalances.toMap shouldEqual assetsBalance.toMap
    }
  }

  // ============================================================================
  // Input Block Diff and Rollback Tests
  // ============================================================================

  it should "record diff when applying input block transactions" in {
    forAll(Gen.listOf(trackedBoxGen), Gen.listOf(trackedBoxGen)) { (existingBoxes, newBoxes) =>
      whenever(existingBoxes.nonEmpty || newBoxes.nonEmpty) {
        val registry = OffChainRegistry.empty.updateOnTransaction(existingBoxes, Seq.empty, Seq.empty)
        val spentIds = existingBoxes.take(2).map(EncodedBoxId @@@ _.boxId)

        val (updatedRegistry, removedOffChain, _) =
          registry.updateOnTransactionWithDiff(newBoxes, spentIds, Seq.empty)

        // Verify the diff captures what was removed
        removedOffChain.map(_.boxId) should contain theSameElementsAs spentIds
        removedOffChain.foreach { rb =>
          updatedRegistry.offChainBoxes should not contain rb
        }
        updatedRegistry.offChainBoxes should contain allElementsOf newBoxes
      }
    }
  }

  it should "rollback input block and restore removed boxes" in {
    forAll(Gen.listOf(trackedBoxGen), Gen.listOf(trackedBoxGen)) { (existingBoxes, newBoxes) =>
      whenever(existingBoxes.nonEmpty || newBoxes.nonEmpty) {
        val inputBlockId = scorex.util.ModifierId @@ "test-input-block-1"
        val registry = OffChainRegistry.empty.updateOnTransaction(existingBoxes, Seq.empty, Seq.empty)
        val spentIds = existingBoxes.take(2).map(EncodedBoxId @@@ _.boxId)

        val (updatedRegistry, removedOffChain, removedOnChain) =
          registry.updateOnTransactionWithDiff(newBoxes, spentIds, Seq.empty)

        val diff = InputBlockDiff(newBoxes, removedOffChain, removedOnChain)
        val registryWithDiff = updatedRegistry.copy(
          inputBlockDiffs = updatedRegistry.inputBlockDiffs + (inputBlockId -> diff)
        )

        val rolledBack = registryWithDiff.rollbackInputBlock(inputBlockId)

        // Verify rolled back state matches original (minus added boxes, plus restored boxes)
        rolledBack.offChainBoxes.map(_.boxId) should contain theSameElementsAs existingBoxes.map(_.boxId)
        newBoxes.foreach { nb =>
          rolledBack.offChainBoxes should not contain nb
        }
        rolledBack.inputBlockDiffs should not contain key(inputBlockId)
      }
    }
  }

  it should "rollback chained input blocks in reverse order" in {
    forAll(Gen.listOf(trackedBoxGen), Gen.listOf(trackedBoxGen), Gen.listOf(trackedBoxGen), Gen.listOf(trackedBoxGen)) {
      (initialBoxes, block1Boxes, block2Boxes, block3Boxes) =>
        whenever(block1Boxes.nonEmpty && block2Boxes.nonEmpty && block3Boxes.nonEmpty) {
          val blockId1 = scorex.util.ModifierId @@ "test-block-1"
          val blockId2 = scorex.util.ModifierId @@ "test-block-2"
          val blockId3 = scorex.util.ModifierId @@ "test-block-3"

          val registry = OffChainRegistry.empty.updateOnTransaction(initialBoxes, Seq.empty, Seq.empty)

          // Block 1: adds new boxes, spends some initial boxes
          val spendIds1 = initialBoxes.take(1).map(EncodedBoxId @@@ _.boxId)
          val (reg1, rem1Off, rem1On) = registry.updateOnTransactionWithDiff(
            block1Boxes, spendIds1, Seq.empty
          )
          val diff1 = InputBlockDiff(block1Boxes, rem1Off, rem1On)
          val regWithDiff1 = reg1.copy(inputBlockDiffs = reg1.inputBlockDiffs + (blockId1 -> diff1))

          // Block 2: adds new boxes, spends some block1 boxes
          val spendIds2 = block1Boxes.take(1).map(EncodedBoxId @@@ _.boxId)
          val (reg2, rem2Off, rem2On) = regWithDiff1.updateOnTransactionWithDiff(
            block2Boxes, spendIds2, Seq.empty
          )
          val diff2 = InputBlockDiff(block2Boxes, rem2Off, rem2On)
          val regWithDiff2 = reg2.copy(inputBlockDiffs = reg2.inputBlockDiffs + (blockId2 -> diff2))

          // Block 3: adds new boxes, spends some block2 boxes
          val spendIds3 = block2Boxes.take(1).map(EncodedBoxId @@@ _.boxId)
          val (reg3, rem3Off, rem3On) = regWithDiff2.updateOnTransactionWithDiff(
            block3Boxes, spendIds3, Seq.empty
          )
          val diff3 = InputBlockDiff(block3Boxes, rem3Off, rem3On)
          val regWithDiff3 = reg3.copy(inputBlockDiffs = reg3.inputBlockDiffs + (blockId3 -> diff3))

          // Record expected state after all blocks
          // Rollback block 3: block3Boxes removed, spent block2 boxes restored
          val afterRollback3 = regWithDiff3.rollbackInputBlock(blockId3)
          afterRollback3.offChainBoxes should contain allElementsOf block2Boxes
          block3Boxes.foreach { b =>
            afterRollback3.offChainBoxes should not contain b
          }

          // Rollback block 2: block2Boxes removed, spent block1 boxes restored
          val afterRollback2 = afterRollback3.rollbackInputBlock(blockId2)
          afterRollback2.offChainBoxes should contain allElementsOf block1Boxes
          block2Boxes.foreach { b =>
            afterRollback2.offChainBoxes should not contain b
          }

          // Rollback block 1: block1Boxes removed, initial state restored
          val afterRollback1 = afterRollback2.rollbackInputBlock(blockId1)
          afterRollback1.offChainBoxes.map(_.boxId) should contain theSameElementsAs initialBoxes.map(_.boxId)
          block1Boxes.foreach { b =>
            afterRollback1.offChainBoxes should not contain b
          }
        }
    }
  }

  it should "return same registry when rolling back non-existent input block" in {
    val registry = OffChainRegistry.empty.updateOnTransaction(
      Seq.empty,
      Seq.empty,
      Seq.empty
    )
    val nonExistentId = scorex.util.ModifierId @@ "non-existent"
    val rolledBack = registry.rollbackInputBlock(nonExistentId)
    rolledBack shouldBe registry
  }

  it should "handle input block diff with on-chain balance removal" in {
    val box = trackedBoxGen.sample.get
    val balance = Balance(box)
    val registry = OffChainRegistry.empty.copy(
      onChainBalances = Seq(balance)
    )

    val spentId = EncodedBoxId @@@ box.boxId
    val (updated, _, removedOn) = registry.updateOnTransactionWithDiff(
      Seq.empty, Seq(spentId), Seq.empty
    )

    removedOn should contain(balance)
    updated.onChainBalances should not contain(balance)

    // Restore via rollback
    val diff = InputBlockDiff(Seq.empty, Seq.empty, removedOn)
    val regWithDiff = updated.copy(inputBlockDiffs = updated.inputBlockDiffs + (scorex.util.ModifierId @@ "ib-1" -> diff))
    val rolledBack = regWithDiff.rollbackInputBlock(scorex.util.ModifierId @@ "ib-1")
    rolledBack.onChainBalances should contain(balance)
  }

  it should "not duplicate boxes when rolling back and restoring" in {
    val box = trackedBoxGen.sample.get
    val registry = OffChainRegistry.empty.updateOnTransaction(Seq(box), Seq.empty, Seq.empty)

    val inputBlockId = scorex.util.ModifierId @@ "test-dedup"
    val (updated, _, removedOn) = registry.updateOnTransactionWithDiff(
      Seq.empty, Seq(EncodedBoxId @@@ box.boxId), Seq.empty
    )

    val diff = InputBlockDiff(Seq.empty, Seq(box), removedOn)
    val regWithDiff = updated.copy(inputBlockDiffs = updated.inputBlockDiffs + (inputBlockId -> diff))

    // The box was in offChainBoxes, got removed, then should be restored
    val rolledBack = regWithDiff.rollbackInputBlock(inputBlockId)
    rolledBack.offChainBoxes.count(_.boxId == box.boxId) shouldBe 1
  }

  it should "re-apply input block after rollback" in {
    forAll(Gen.listOf(trackedBoxGen), Gen.listOf(trackedBoxGen)) { (existingBoxes, newBoxes) =>
      whenever(existingBoxes.nonEmpty || newBoxes.nonEmpty) {
        val inputBlockId = scorex.util.ModifierId @@ "test-reapply"
        val registry = OffChainRegistry.empty.updateOnTransaction(existingBoxes, Seq.empty, Seq.empty)
        val spentIds = existingBoxes.take(2).map(EncodedBoxId @@@ _.boxId)

        val (updatedRegistry, removedOffChain, removedOnChain) =
          registry.updateOnTransactionWithDiff(newBoxes, spentIds, Seq.empty)

        val diff = InputBlockDiff(newBoxes, removedOffChain, removedOnChain)
        val registryWithDiff = updatedRegistry.copy(
          inputBlockDiffs = updatedRegistry.inputBlockDiffs + (inputBlockId -> diff)
        )

        // Rollback the input block
        val afterRollback = registryWithDiff.rollbackInputBlock(inputBlockId)
        afterRollback.inputBlockDiffs should not contain key(inputBlockId)

        // Re-apply the same input block by actually re-running the update
        val (afterReapply, newRemovedOff, newRemovedOn) =
          afterRollback.updateOnTransactionWithDiff(newBoxes, spentIds, Seq.empty)
        val newDiff = InputBlockDiff(newBoxes, newRemovedOff, newRemovedOn)
        val registryWithNewDiff = afterReapply.copy(
          inputBlockDiffs = afterReapply.inputBlockDiffs + (inputBlockId -> newDiff)
        )

        // Verify state matches the original updated registry
        registryWithNewDiff.offChainBoxes.map(_.boxId) should contain theSameElementsAs updatedRegistry.offChainBoxes.map(_.boxId)
        registryWithNewDiff.inputBlockDiffs should contain key(inputBlockId)
      }
    }
  }

  it should "rollback and restore asset balances correctly" in {
    forAll(Gen.listOf(trackedBoxGen), Gen.listOf(trackedBoxGen)) { (existingBoxes, newBoxes) =>
      whenever(existingBoxes.nonEmpty || newBoxes.nonEmpty) {
        val inputBlockId = scorex.util.ModifierId @@ "test-assets"
        val registry = OffChainRegistry.empty.updateOnTransaction(existingBoxes, Seq.empty, Seq.empty)
        val initialDigest = registry.digest
        val spentIds = existingBoxes.take(2).map(EncodedBoxId @@@ _.boxId)

        val (updatedRegistry, removedOffChain, removedOnChain) =
          registry.updateOnTransactionWithDiff(newBoxes, spentIds, Seq.empty)

        val diff = InputBlockDiff(newBoxes, removedOffChain, removedOnChain)
        val registryWithDiff = updatedRegistry.copy(
          inputBlockDiffs = updatedRegistry.inputBlockDiffs + (inputBlockId -> diff)
        )

        val updatedDigest = registryWithDiff.digest
        // Digest should have changed after applying input block
        (updatedDigest.walletBalance != initialDigest.walletBalance ||
          updatedDigest.walletAssetBalances != initialDigest.walletAssetBalances) shouldBe true

        // Rollback should restore digest to initial state
        val afterRollback = registryWithDiff.rollbackInputBlock(inputBlockId)
        val rolledBackDigest = afterRollback.digest
        rolledBackDigest.walletBalance shouldBe initialDigest.walletBalance
        rolledBackDigest.walletAssetBalances.toMap shouldBe initialDigest.walletAssetBalances.toMap
      }
    }
  }

  it should "handle out-of-order rollback correctly" in {
    forAll(Gen.listOf(trackedBoxGen), Gen.listOf(trackedBoxGen), Gen.listOf(trackedBoxGen)) {
      (initialBoxes, block1Boxes, block2Boxes) =>
        whenever(block1Boxes.nonEmpty && block2Boxes.nonEmpty && initialBoxes.nonEmpty) {
          val blockId1 = scorex.util.ModifierId @@ "test-oo-block-1"
          val blockId2 = scorex.util.ModifierId @@ "test-oo-block-2"

          val registry = OffChainRegistry.empty.updateOnTransaction(initialBoxes, Seq.empty, Seq.empty)

          // Block 1: adds new boxes, spends some initial boxes
          val spendIds1 = initialBoxes.take(1).map(EncodedBoxId @@@ _.boxId)
          val (reg1, rem1Off, rem1On) = registry.updateOnTransactionWithDiff(
            block1Boxes, spendIds1, Seq.empty
          )
          val diff1 = InputBlockDiff(block1Boxes, rem1Off, rem1On)
          val regWithDiff1 = reg1.copy(inputBlockDiffs = reg1.inputBlockDiffs + (blockId1 -> diff1))

          // Block 2: adds new boxes, spends some block1 boxes
          val spendIds2 = block1Boxes.take(1).map(EncodedBoxId @@@ _.boxId)
          val (reg2, rem2Off, rem2On) = regWithDiff1.updateOnTransactionWithDiff(
            block2Boxes, spendIds2, Seq.empty
          )
          val diff2 = InputBlockDiff(block2Boxes, rem2Off, rem2On)
          val regWithDiff2 = reg2.copy(inputBlockDiffs = reg2.inputBlockDiffs + (blockId2 -> diff2))

          // Out-of-order: Rollback block 1 first while block 2 is still active
          val afterRollback1 = regWithDiff2.rollbackInputBlock(blockId1)

          // Block 1's added boxes should be removed, but block 2's boxes should remain
          block1Boxes.foreach { b =>
            afterRollback1.offChainBoxes should not contain b
          }
          // Block 2 boxes should still be present (they were added by block 2, not block 1)
          block2Boxes.foreach { b =>
            afterRollback1.offChainBoxes should contain(b)
          }

          // Then rollback block 2
          val afterRollback2 = afterRollback1.rollbackInputBlock(blockId2)
          block2Boxes.foreach { b =>
            afterRollback2.offChainBoxes should not contain b
          }

          // Out-of-order rollback leaves intermediate state:
          // block 2 restores boxes it spent (from block 1), but block 1 was already rolled back
          // so we have initialBoxes + the box block2 spent (which was from block1)
          val expectedFinalBoxes = initialBoxes ++ block1Boxes.take(1)
          afterRollback2.offChainBoxes.map(_.boxId) should contain theSameElementsAs expectedFinalBoxes.map(_.boxId)
        }
    }
  }

  it should "handle empty input block diff" in {
    val registry = OffChainRegistry.empty.updateOnTransaction(
      Seq.empty, Seq.empty, Seq.empty
    )

    val inputBlockId = scorex.util.ModifierId @@ "test-empty"
    val (updated, removedOff, removedOn) = registry.updateOnTransactionWithDiff(
      Seq.empty, Seq.empty, Seq.empty
    )

    // Empty diff should have no added or removed boxes
    removedOff shouldBe empty
    removedOn shouldBe empty

    val diff = InputBlockDiff(Seq.empty, Seq.empty, Seq.empty)
    val regWithDiff = updated.copy(inputBlockDiffs = updated.inputBlockDiffs + (inputBlockId -> diff))

    // Rollback empty diff should not change state
    val rolledBack = regWithDiff.rollbackInputBlock(inputBlockId)
    rolledBack shouldBe updated.copy(inputBlockDiffs = updated.inputBlockDiffs - inputBlockId)
  }


}
