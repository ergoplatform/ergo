package org.ergoplatform.reemission

import org.ergoplatform._
import org.ergoplatform.settings.{MonetarySettings, ReemissionSettings}
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.crypto.hash.Blake2b256
import scorex.util.ModifierId
import sigma.Colls
import sigmastate.AvlTreeData
import sigmastate.TrivialProp.TrueProp
import sigmastate.eval.Digest32Coll
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

  ignore("reemission rules test vectors") {

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
    val feeBox = new ErgoBoxCandidate(feeValue, TrueProp, currentHeight)
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
    val feeBox3 = new ErgoBoxCandidate(feeValue + 1, TrueProp, currentHeight)
    val spendingTransaction3 = ErgoLikeTransaction(inputs2, IndexedSeq(newReemissionBox3, feeBox3))

    checkRewardsTx(currentHeight, pkBytes, inputBoxes, spendingTransaction3, false)

    // reemission NFT must be preserved
    val newReemissionBox4 = new ErgoBoxCandidate(reemissionBox.value + mergedValue - feeValue, prop, currentHeight)
    val spendingTransaction4 = ErgoLikeTransaction(inputs, IndexedSeq(newReemissionBox4, feeBox))

    checkRewardsTx(currentHeight, pkBytes, inputBoxes, spendingTransaction4, false)

    // reemission box value must be increased
    val feeValue5 = mergedValue
    val newReemissionBox5 = new ErgoBoxCandidate(reemissionBox.value + mergedValue - feeValue5, prop, currentHeight, reemissionBoxAssets)
    val feeBox5 = new ErgoBoxCandidate(feeValue5, TrueProp, currentHeight)
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

}
