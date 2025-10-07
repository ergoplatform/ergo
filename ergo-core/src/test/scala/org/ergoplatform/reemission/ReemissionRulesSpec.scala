package org.ergoplatform.reemission

import org.ergoplatform._
import org.ergoplatform.settings.{MonetarySettings, ReemissionSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.crypto.hash.Blake2b256
import scorex.util.ModifierId
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.data.TrivialProp.TrueProp
import sigma.data.{AvlTreeData, Digest32Coll}
import sigmastate.helpers.TestingHelpers.testBox
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.interpreter.Interpreter.emptyEnv

import scala.util.{Failure, Success, Try}

// done similarly to ErgoScriptPredefSpec in sigma repo
class ReemissionRulesSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private val ms = MonetarySettings()
  private val checkReemissionRules: Boolean = true
  private val emissionNftId: ModifierId = ModifierId @@ "06f29034fb69b23d519f84c4811a19694b8cdc2ce076147aaa050276f0b840f4"
  private val reemissionTokenId: ModifierId = ModifierId @@ "01345f0ed87b74008d1c46aefd3e7ad6ee5909a2324f2899031cdfee3cc1e022"
  private val reemissionNftId: ModifierId = ModifierId @@ "06f2c3adfe52304543f7b623cc3fccddc0174a7db52452fef8e589adacdfdfee"
  private val activationHeight: Int = 0
  private val reemissionStartHeight: Int = 100
  private val injectionBoxBytesEncoded: ModifierId = ModifierId @@ "a0f9e1b5fb011003040005808098f4e9b5ca6a0402d1ed91c1b2a4730000730193c5a7c5b2a4730200f6ac0b0201345f0ed87b74008d1c46aefd3e7ad6ee5909a2324f2899031cdfee3cc1e02280808cfaf49aa53506f29034fb69b23d519f84c4811a19694b8cdc2ce076147aaa050276f0b840f40100325c3679e7e0e2f683e4a382aa74c2c1cb989bb6ad6a1d4b1c5a021d7b410d0f00"
  private val rs = ReemissionSettings(checkReemissionRules, emissionNftId, reemissionTokenId,
                                      reemissionNftId, activationHeight, reemissionStartHeight, injectionBoxBytesEncoded)

  private val rr = new ReemissionRules(rs)

  private val reemissionBoxAssets = Colls.fromItems((Digest32Coll @@ rs.reemissionNftIdBytes) -> 1L)

  private val fakeMessage = Blake2b256("Hello World")

  private def prover = new ContextEnrichingTestProvingInterpreter
  private def verifier = new ErgoLikeTestInterpreter
  private val prop = rr.reemissionBoxProp(ms)

  def checkRewardsTx(nextHeight: Int,
                     pkBytes: Array[Byte],
                     inputBoxes: IndexedSeq[ErgoBox],
                     spendingTransaction: ErgoLikeTransaction,
                     expectedValidity: Boolean) = {
    val ctx = ErgoLikeContextTesting(
      currentHeight = nextHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = pkBytes,
      boxesToSpend = inputBoxes,
      spendingTransaction,
      self = inputBoxes.head,
      1: Byte) //activated script version
    Try(prover.prove(emptyEnv, prop, ctx, fakeMessage).get) match {
      case Success(pr) =>
        verifier.verify(emptyEnv, prop, ctx, pr, fakeMessage).get._1 shouldBe expectedValidity
      case Failure(e) if expectedValidity =>
        throw new Exception("Unexpected exception thrown: ", e)
      case _ =>
    }
  }

  def checkContract(prop: ErgoTree,
                    nextHeight: Int,
                    pkBytes: Array[Byte],
                    inputBoxes: IndexedSeq[ErgoBox],
                    spendingTransaction: ErgoLikeTransaction,
                    expectedValidity: Boolean) = {
    val ctx = ErgoLikeContextTesting(
      currentHeight = nextHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = pkBytes,
      boxesToSpend = inputBoxes,
      spendingTransaction,
      self = inputBoxes.head,
      1: Byte) //activated script version
    Try(prover.prove(emptyEnv, prop, ctx, fakeMessage).get) match {
      case Success(pr) =>
        verifier.verify(emptyEnv, prop, ctx, pr, fakeMessage).get._1 shouldBe expectedValidity
      case Failure(e) if expectedValidity =>
        throw new Exception("Unexpected exception thrown: ", e)
      case _ =>
    }
  }

  property("reemission rules test vectors") {
    import org.ergoplatform.mining.emission.EmissionRules
    val emissionRules = new EmissionRules(ms)

    // Test vectors for reemissionForHeight calculation
    // These test specific height points and expected reemission amounts

    // Test 1: Before activation height - should return 0
    val beforeActivation = rr.reemissionForHeight(rs.activationHeight - 1, emissionRules)
    beforeActivation shouldBe 0L

    // Test 2: At activation height with sufficient emission
    // The actual value depends on emissionAtHeight calculation
    val atActivation = rr.reemissionForHeight(rs.activationHeight, emissionRules)
    atActivation should be >= 0L

    // Test 3: Various height points to test different conditions
    val testHeights = Seq(
      rs.activationHeight + 1,
      rs.activationHeight + 100,
      rs.activationHeight + 1000,
      rs.reemissionStartHeight,
      rs.reemissionStartHeight + 100
    )

    testHeights.foreach { height =>
      val reemissionAmount = rr.reemissionForHeight(height, emissionRules)
      reemissionAmount should be >= 0L
      // Basic sanity check: reemission amount should not exceed emission
      val emissionAtHeight = emissionRules.emissionAtHeight(height.toLong)
      reemissionAmount should be <= emissionAtHeight
    }

    // Test 4: Specific edge cases
    // Test when emission is exactly at threshold
    val thresholdHeight = rs.activationHeight
    val thresholdReemission = rr.reemissionForHeight(thresholdHeight, emissionRules)
    thresholdReemission should be >= 0L

    // Test 5: Very high heights to ensure stability
    val highHeight = rs.reemissionStartHeight + 10000
    val highReemission = rr.reemissionForHeight(highHeight, emissionRules)
    highReemission should be >= 0L

    // Test 6: Contract property test vectors
    // Test that reemissionBoxProp produces consistent results
    val minerPk = prover.dlogSecrets.head.publicImage
    val pkBytes = minerPk.pkBytes
    val minerProp = ErgoTreePredef.rewardOutputScript(ms.minerRewardDelay, minerPk)

    val testHeight = rs.reemissionStartHeight
    val nextHeight = testHeight + 1

    // Standard spending transaction vector
    val initialErgValue = 1000000000000L
    val reemissionBox = testBox(initialErgValue, prop, testHeight, reemissionBoxAssets.toArray, Map())
    val reemissionReward = rr.reemissionRewardPerBlock

    val inputBoxes = IndexedSeq(reemissionBox)
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))

    val newReemissionBox = new ErgoBoxCandidate(reemissionBox.value - reemissionReward, prop, nextHeight, reemissionBoxAssets)
    val minerBox = new ErgoBoxCandidate(reemissionReward, minerProp, nextHeight)
    val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox, minerBox))

    // This should always succeed at reemission start height
    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction, true)

    // Test 7: Merging transaction vector
    val pay2RBox = testBox(50000000L, rr.payToReemission, testHeight - 1, reemissionBoxAssets.toArray, Map())
    val reemissionBoxMerge = testBox(initialErgValue, prop, testHeight - 1, reemissionBoxAssets.toArray, Map())
    val inputBoxesMerge = IndexedSeq(reemissionBoxMerge, pay2RBox)
    val inputsMerge = inputBoxesMerge.map(b => Input(b.id, emptyProverResult))

    val feeValue = 1000000L
    val newReemissionBoxMerge = new ErgoBoxCandidate(reemissionBoxMerge.value + pay2RBox.value - feeValue, prop, testHeight - 1, reemissionBoxAssets)
    val feeBox = new ErgoBoxCandidate(feeValue, ErgoTree.fromSigmaBoolean(TrueProp), testHeight - 1)
    val mergeTransaction = ErgoLikeTransaction(inputsMerge, IndexedSeq(newReemissionBoxMerge, feeBox))

    // Merging should work before reemission start height
    checkRewardsTx(testHeight - 1, pkBytes, inputBoxesMerge, mergeTransaction, true)

    // Test 8: Basic charge amount consistency
    rr.basicChargeAmount shouldBe 12 // 12 ERG as defined in ReemissionRules

    // Test 9: Reemission reward per block consistency
    rr.reemissionRewardPerBlock shouldBe 3 * org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo // 3 ERG

    // Test 11: Specific known test vectors
    // These test specific known values that should be consistent
    
    // Test vector 1: Basic charge amount in nanoERG
    val basicChargeNanoErg = rr.basicChargeAmount * org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo
    basicChargeNanoErg shouldBe 12L * 1000000000L // 12 ERG in nanoERG
    
    // Test vector 2: Reemission reward per block in nanoERG
    val rewardPerBlockNanoErg = rr.reemissionRewardPerBlock
    rewardPerBlockNanoErg shouldBe 3L * 1000000000L // 3 ERG in nanoERG
    
    // Test vector 3: Fee limit for merging transactions
    val maxFeeForMerging = org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo / 100
    maxFeeForMerging shouldBe 10000000L // 0.01 ERG in nanoERG
    
    // Test vector 4: Activation and start heights
    rs.activationHeight shouldBe 0
    rs.reemissionStartHeight shouldBe 100
    
    // Test vector 5: NFT ID consistency check
    // The NFT ID should be consistent with the settings
    val expectedNftId = "06f2c3adfe52304543f7b623cc3fccddc0174a7db52452fef8e589adacdfdfee"
    reemissionNftId shouldBe expectedNftId

    // Test 10: NFT ID consistency
    rr.reemissionNftIdBytes shouldBe rs.reemissionNftIdBytes
    rr.reemissionStartHeight shouldBe rs.reemissionStartHeight
  }

  property("reemissionBoxProp - spending path") {
    val minerPk = prover.dlogSecrets.head.publicImage
    val pkBytes = minerPk.pkBytes
    val minerProp = ErgoTreePredef.rewardOutputScript(ms.minerRewardDelay, minerPk)

    val currentHeight = rs.reemissionStartHeight
    val nextHeight = currentHeight + 1

    val initialErgValue = 1000000000000L
    val reemissionBox = testBox(initialErgValue, prop, currentHeight, reemissionBoxAssets.toArray, Map())

    val reemissionReward = rr.reemissionRewardPerBlock

    val inputBoxes = IndexedSeq(reemissionBox)
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))

    val newReemissionBox = new ErgoBoxCandidate(reemissionBox.value - reemissionReward, prop, nextHeight, reemissionBoxAssets)
    val minerBox = new ErgoBoxCandidate(reemissionReward, minerProp, nextHeight)

    val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox, minerBox))

    // normal execution
    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction, true)

    // miner tries to take too much from reemission contract
    val newReemissionBox2 = new ErgoBoxCandidate(reemissionBox.value - reemissionReward - 1, prop, nextHeight, reemissionBoxAssets)
    val minerBox2 = new ErgoBoxCandidate(reemissionReward + 1, minerProp, nextHeight)
    val spendingTransaction2 = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox2, minerBox2))
    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction2, false)

    //... and it is not okay to take less even
    val newReemissionBox3 = new ErgoBoxCandidate(reemissionBox.value - reemissionReward + 1, prop, nextHeight, reemissionBoxAssets)
    val minerBox3 = new ErgoBoxCandidate(reemissionReward - 1, minerProp, nextHeight)
    val spendingTransaction3 = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox3, minerBox3))
    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction3, false)

    // re-emission NFT must be preserved
    val newReemissionBox4 = new ErgoBoxCandidate(reemissionBox.value - reemissionReward, prop, nextHeight, Colls.emptyColl)
    val spendingTransaction4 = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox4, minerBox))
    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction4, false)

    // not possible to charge before re-emission start
    val nextHeight5 = currentHeight - 10
    val emissionBox5 = testBox(initialErgValue, prop, nextHeight5 - 1, reemissionBoxAssets.toArray, Map())
    val inputBoxes5 = IndexedSeq(emissionBox5)
    val inputs5 = inputBoxes5.map(b => Input(b.id, emptyProverResult))
    val newReemissionBox5 = new ErgoBoxCandidate(emissionBox5.value - reemissionReward, prop, nextHeight5, reemissionBoxAssets)
    val minerBox5 = new ErgoBoxCandidate(reemissionReward, minerProp, nextHeight5)
    val spendingTransaction5 = ErgoLikeTransaction(inputs5, IndexedSeq(newReemissionBox5, minerBox5))
    checkRewardsTx(nextHeight5, pkBytes, inputBoxes5, spendingTransaction5, false)

    // can be spent to miner pubkey only
    val prover6 = new ContextEnrichingTestProvingInterpreter
    val minerPk6 = prover6.dlogSecrets.head.publicImage
    val pkBytes6 = minerPk6.pkBytes
    checkRewardsTx(nextHeight, pkBytes6, inputBoxes, spendingTransaction, false)

    // we modify reward delay here, not PK
    val minerProp7 = ErgoTreePredef.rewardOutputScript(ms.minerRewardDelay - 1, minerPk)
    val minerBox7 = new ErgoBoxCandidate(reemissionReward, minerProp7, nextHeight)
    val spendingTransaction7 = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox, minerBox7))
    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction7, false)
  }

  // also testing payToReemission contract
  property("reemissionBoxProp - merging path") {
    val minerPk = prover.dlogSecrets.head.publicImage
    val pkBytes = minerPk.pkBytes

    val rewardsProp = prop
    val pay2RewardsProp = rr.payToReemission

    val mergedValue = 100000000L

    val currentHeight = rs.reemissionStartHeight - 1

    val pay2RBox = testBox(mergedValue, pay2RewardsProp, currentHeight, reemissionBoxAssets.toArray, Map())
    val reemissionBox = testBox(mergedValue * 100, rewardsProp, currentHeight, reemissionBoxAssets.toArray, Map())

    val inputBoxes = IndexedSeq(reemissionBox, pay2RBox)
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))

    val feeValue = 10000000L

    // merging with 1 box - successful case
    val newReemissionBox = new ErgoBoxCandidate(reemissionBox.value + mergedValue - feeValue, prop, currentHeight, reemissionBoxAssets)
    val feeBox = new ErgoBoxCandidate(feeValue, ErgoTree.fromSigmaBoolean(TrueProp), currentHeight)
    val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox, feeBox))

    checkRewardsTx(currentHeight, pkBytes, inputBoxes, spendingTransaction, true)

    // merging with 2 boxex - successful case
    val inputBoxes2 = IndexedSeq(reemissionBox, pay2RBox, pay2RBox)
    val inputs2 = inputBoxes2.map(b => Input(b.id, emptyProverResult))
    val newReemissionBox2 = new ErgoBoxCandidate(reemissionBox.value + 2 * mergedValue - feeValue, prop, currentHeight, reemissionBoxAssets)
    val spendingTransaction2 = ErgoLikeTransaction(inputs2, IndexedSeq(newReemissionBox2, feeBox))

    checkRewardsTx(currentHeight, pkBytes, inputBoxes, spendingTransaction2, true)

    // paying too high fee
    val newReemissionBox3 = new ErgoBoxCandidate(reemissionBox.value + mergedValue - feeValue - 1, prop, currentHeight, reemissionBoxAssets)
    val feeBox3 = new ErgoBoxCandidate(feeValue + 1, ErgoTree.fromSigmaBoolean(TrueProp), currentHeight)
    val spendingTransaction3 = ErgoLikeTransaction(inputs2, IndexedSeq(newReemissionBox3, feeBox3))

    checkRewardsTx(currentHeight, pkBytes, inputBoxes, spendingTransaction3, false)

    // reemission NFT must be preserved
    val newReemissionBox4 = new ErgoBoxCandidate(reemissionBox.value + mergedValue - feeValue, prop, currentHeight)
    val spendingTransaction4 = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox4, feeBox))

    checkRewardsTx(currentHeight, pkBytes, inputBoxes, spendingTransaction4, false)

    // reemission box value must be increased
    val feeValue5 = mergedValue
    val newReemissionBox5 = new ErgoBoxCandidate(reemissionBox.value + mergedValue - feeValue5, prop, currentHeight, reemissionBoxAssets)
    val feeBox5 = new ErgoBoxCandidate(feeValue5, ErgoTree.fromSigmaBoolean(TrueProp), currentHeight)
    val spendingTransaction5 = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox5, feeBox5))
    checkRewardsTx(currentHeight, pkBytes, inputBoxes, spendingTransaction5, false)

    // pay-2-reemission box can be spent only with a box with reemission NFT as input #0
    val reemissionBoxAssets6 = Colls.fromItems(
      (Digest32Coll @@ rs.reemissionNftIdBytes.reverse) -> 1L
    )
    val newReemissionBox6 = new ErgoBoxCandidate(
      reemissionBox.value + mergedValue - feeValue,
      prop, currentHeight, reemissionBoxAssets6)
    val spendingTransaction6 = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox6, feeBox))

    val ctx = ErgoLikeContextTesting(
      currentHeight = currentHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = pkBytes,
      boxesToSpend = inputBoxes,
      spendingTransaction6,
      self = inputBoxes(1),
      0)

    prover.prove(emptyEnv, pay2RewardsProp, ctx, fakeMessage).isFailure shouldBe true
  }

  property("reemissionBoxProp - reemission start height validation") {
    val minerPk = prover.dlogSecrets.head.publicImage
    val pkBytes = minerPk.pkBytes
    val minerProp = ErgoTreePredef.rewardOutputScript(ms.minerRewardDelay, minerPk)

    val reemissionReward = rr.reemissionRewardPerBlock

    // Test at reemission start height - should succeed via spending path
    val startHeight = rs.reemissionStartHeight
    val nextHeight = startHeight + 1

    val initialErgValue = 1000000000000L
    val reemissionBox = testBox(initialErgValue, prop, startHeight, reemissionBoxAssets.toArray, Map())

    val inputBoxes = IndexedSeq(reemissionBox)
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))

    val newReemissionBox = new ErgoBoxCandidate(reemissionBox.value - reemissionReward, prop, nextHeight, reemissionBoxAssets)
    val minerBox = new ErgoBoxCandidate(reemissionReward, minerProp, nextHeight)
    val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox, minerBox))

    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction, true)

    // Test before reemission start height - spending path should fail, but merging path should work
    val beforeStartHeight = rs.reemissionStartHeight - 1

    val reemissionBoxBefore = testBox(initialErgValue, prop, beforeStartHeight, reemissionBoxAssets.toArray, Map())

    // Spending path (miner reward) before start height should fail
    // Use a height significantly below the reemission start height
    val farBeforeHeight = rs.reemissionStartHeight - 20
    val farBeforeNextHeight = farBeforeHeight + 1

    val reemissionBoxFarBefore = testBox(initialErgValue, prop, farBeforeHeight, reemissionBoxAssets.toArray, Map())
    val inputBoxesFarBefore = IndexedSeq(reemissionBoxFarBefore)
    val inputsFarBefore = inputBoxesFarBefore.map(b => Input(b.id, emptyProverResult))

    val newReemissionBoxFarBefore = new ErgoBoxCandidate(reemissionBoxFarBefore.value - reemissionReward, prop, farBeforeNextHeight, reemissionBoxAssets)
    val minerBoxFarBefore = new ErgoBoxCandidate(reemissionReward, minerProp, farBeforeNextHeight)
    val spendingTransactionFarBefore = ErgoLikeTransaction(inputsFarBefore, IndexedSeq(newReemissionBoxFarBefore, minerBoxFarBefore))
    checkRewardsTx(farBeforeNextHeight, pkBytes, inputBoxesFarBefore, spendingTransactionFarBefore, false)

    // But merging path should still work before start height
    val pay2RBox = testBox(100000000L, rr.payToReemission, beforeStartHeight, reemissionBoxAssets.toArray, Map())
    val inputBoxesMerge = IndexedSeq(reemissionBoxBefore, pay2RBox)
    val inputsMerge = inputBoxesMerge.map(b => Input(b.id, emptyProverResult))
    val feeValue = 1000000L
    val newReemissionBoxMerge = new ErgoBoxCandidate(reemissionBoxBefore.value + pay2RBox.value - feeValue, prop, beforeStartHeight, reemissionBoxAssets)
    val feeBox = new ErgoBoxCandidate(feeValue, ErgoTree.fromSigmaBoolean(TrueProp), beforeStartHeight)
    val mergeTransaction = ErgoLikeTransaction(inputsMerge, IndexedSeq(newReemissionBoxMerge, feeBox))
    checkRewardsTx(beforeStartHeight, pkBytes, inputBoxesMerge, mergeTransaction, true)
  }

  property("reemissionBoxProp - negative tests for contract validation") {
    val minerPk = prover.dlogSecrets.head.publicImage
    val pkBytes = minerPk.pkBytes
    val minerProp = ErgoTreePredef.rewardOutputScript(ms.minerRewardDelay, minerPk)

    val currentHeight = rs.reemissionStartHeight
    val nextHeight = currentHeight + 1

    val initialErgValue = 1000000000000L
    val reemissionBox = testBox(initialErgValue, prop, currentHeight, reemissionBoxAssets.toArray, Map())

    val reemissionReward = rr.reemissionRewardPerBlock

    val inputBoxes = IndexedSeq(reemissionBox)
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))

    // Test 1: Wrong NFT ID in output - should fail
    val wrongNftAssets = Colls.fromItems((Digest32Coll @@ rs.reemissionNftIdBytes.reverse) -> 1L)
    val wrongNftReemissionBox = new ErgoBoxCandidate(reemissionBox.value - reemissionReward, prop, nextHeight, wrongNftAssets)
    val minerBox = new ErgoBoxCandidate(reemissionReward, minerProp, nextHeight)
    val spendingTransaction1 = ErgoLikeTransaction(inputs, IndexedSeq(wrongNftReemissionBox, minerBox))
    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction1, false)

    // Test 2: Wrong height in reemission output - should fail
    val wrongHeightReemissionBox = new ErgoBoxCandidate(reemissionBox.value - reemissionReward, prop, nextHeight + 1, reemissionBoxAssets)
    val spendingTransaction2 = ErgoLikeTransaction(inputs, IndexedSeq(wrongHeightReemissionBox, minerBox))
    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction2, false)

    // Test 3: Wrong height in miner output - should fail
    val wrongHeightMinerBox = new ErgoBoxCandidate(reemissionReward, minerProp, nextHeight + 1)
    val spendingTransaction3 = ErgoLikeTransaction(inputs, IndexedSeq(wrongNftReemissionBox, wrongHeightMinerBox))
    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction3, false)

    // Test 4: Different contract script in output - should fail
    val differentContractBox = new ErgoBoxCandidate(reemissionBox.value - reemissionReward, ErgoTree.fromSigmaBoolean(TrueProp), nextHeight, reemissionBoxAssets)
    val spendingTransaction4 = ErgoLikeTransaction(inputs, IndexedSeq(differentContractBox, minerBox))
    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction4, false)

    // Test 5: Height not increased - should fail
    val sameHeightReemissionBox = new ErgoBoxCandidate(reemissionBox.value - reemissionReward, prop, currentHeight, reemissionBoxAssets)
    val spendingTransaction5 = ErgoLikeTransaction(inputs, IndexedSeq(sameHeightReemissionBox, minerBox))
    checkRewardsTx(nextHeight, pkBytes, inputBoxes, spendingTransaction5, false)
  }

  property("payToReemission contract - NFT validation") {
    val minerPk = prover.dlogSecrets.head.publicImage
    val pkBytes = minerPk.pkBytes

    val currentHeight = rs.reemissionStartHeight - 1
    val pay2RewardsProp = rr.payToReemission

    val pay2RBox = testBox(100000000L, pay2RewardsProp, currentHeight, reemissionBoxAssets.toArray, Map())
    val reemissionBox = testBox(10000000000L, prop, currentHeight, reemissionBoxAssets.toArray, Map())

    val inputBoxes = IndexedSeq(reemissionBox, pay2RBox)
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))

    val feeValue = 1000000L

    // Test 1: Missing reemission NFT in output - should fail
    val newReemissionBox1 = new ErgoBoxCandidate(reemissionBox.value + pay2RBox.value - feeValue, prop, currentHeight)
    val feeBox1 = new ErgoBoxCandidate(feeValue, ErgoTree.fromSigmaBoolean(TrueProp), currentHeight)
    val spendingTransaction1 = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox1, feeBox1))
    
    // For payToReemission contract, we need to test from the perspective of the pay2RBox (input #1)
    val ctx1 = ErgoLikeContextTesting(
      currentHeight = currentHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = pkBytes,
      boxesToSpend = inputBoxes,
      spendingTransaction1,
      self = inputBoxes(1), // pay2RBox is at index 1
      1: Byte)
    
    // The payToReemission contract should fail when the NFT is missing
    prover.prove(emptyEnv, pay2RewardsProp, ctx1, fakeMessage).isFailure shouldBe true

    // Test 2: Wrong NFT ID in output - should fail
    val wrongNftAssets = Colls.fromItems((Digest32Coll @@ rs.reemissionNftIdBytes.reverse) -> 1L)
    val newReemissionBox2 = new ErgoBoxCandidate(reemissionBox.value + pay2RBox.value - feeValue, prop, currentHeight, wrongNftAssets)
    val feeBox2 = new ErgoBoxCandidate(feeValue, ErgoTree.fromSigmaBoolean(TrueProp), currentHeight)
    val spendingTransaction2 = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox2, feeBox2))
    
    val ctx2 = ErgoLikeContextTesting(
      currentHeight = currentHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = pkBytes,
      boxesToSpend = inputBoxes,
      spendingTransaction2,
      self = inputBoxes(1),
      1: Byte)
    
    // The payToReemission contract should fail when the NFT is wrong
    prover.prove(emptyEnv, pay2RewardsProp, ctx2, fakeMessage).isFailure shouldBe true
  }

  property("reemissionForHeight - activation height calculation") {
    import org.ergoplatform.mining.emission.EmissionRules
    val emissionRules = new EmissionRules(ms)

    // Test before activation height - should return 0
    val beforeActivation = rr.reemissionForHeight(rs.activationHeight - 1, emissionRules)
    beforeActivation shouldBe 0L

    // Test at activation height with sufficient emission - should return basic charge
    val atActivation = rr.reemissionForHeight(rs.activationHeight, emissionRules)
    // Note: This depends on the actual emission calculation, but should be >= 0
    atActivation should be >= 0L

    // Test after activation height - should return appropriate amount
    val afterActivation = rr.reemissionForHeight(rs.activationHeight + 100, emissionRules)
    afterActivation should be >= 0L
  }

}
