package org.ergoplatform.reemission

import org.ergoplatform.{ErgoBoxCandidate, ErgoLikeTransaction, ErgoScriptPredef, Input}
import org.ergoplatform.settings.{MonetarySettings, ReemissionSettings}
import org.ergoplatform.utils.{ErgoPropertyTest, ErgoTestConstants}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.ModifierId
import sigmastate.AvlTreeData
import sigmastate.eval.{IRContext, RuntimeCosting}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers.testBox
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}

// done similarly to ErgoScriptPredefSpec in sigma repo
class ReemissionRulesSpec extends ErgoPropertyTest with ErgoTestConstants {

  private val ms = MonetarySettings()
  private val checkReemissionRules: Boolean = true
  private val emissionNftId: ModifierId = ModifierId @@ "06f29034fb69b23d519f84c4811a19694b8cdc2ce076147aaa050276f0b840f4"
  private val reemissionTokenId: ModifierId = ModifierId @@ "01345f0ed87b74008d1c46aefd3e7ad6ee5909a2324f2899031cdfee3cc1e022"
  private val reemissionNftId: ModifierId = ModifierId @@ "06f2c3adfe52304543f7b623cc3fccddc0174a7db52452fef8e589adacdfdfee"
  private val activationHeight: Int = 0
  private val reemissionStartHeight: Int = 0
  private val injectionBoxBytesEncoded: ModifierId = ModifierId @@ "a0f9e1b5fb011003040005808098f4e9b5ca6a0402d1ed91c1b2a4730000730193c5a7c5b2a4730200f6ac0b0201345f0ed87b74008d1c46aefd3e7ad6ee5909a2324f2899031cdfee3cc1e02280808cfaf49aa53506f29034fb69b23d519f84c4811a19694b8cdc2ce076147aaa050276f0b840f40100325c3679e7e0e2f683e4a382aa74c2c1cb989bb6ad6a1d4b1c5a021d7b410d0f00"
  private val rs = ReemissionSettings(checkReemissionRules, emissionNftId, reemissionTokenId,
                                      reemissionNftId, activationHeight, reemissionStartHeight, injectionBoxBytesEncoded)

  private val rr = new ReemissionRules(rs)

  class TestingIRContext extends IRContext with RuntimeCosting

  private implicit lazy val IR: TestingIRContext = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = false
  }

  ignore("reemission rules test vectors") {

  }

  property("reemissionBoxProp - spending path") {
    val prop = rr.reemissionBoxProp(ms)
    val fakeMessage = Blake2b256("Hello World")

    val prover = new ContextEnrichingTestProvingInterpreter

    val minerPk = prover.dlogSecrets.head.publicImage
    val pkBytes = minerPk.pkBytes
    val minerProp = ErgoScriptPredef.rewardOutputScript(ms.minerRewardDelay, minerPk)

    val verifier = new ErgoLikeTestInterpreter

    val currentHeight = rs.reemissionStartHeight
    val nextHeight = currentHeight + 1

    val emissionBoxAssets = Seq(
      (Digest32 @@ rs.emissionNftIdBytes) -> 1L,
      (Digest32 @@ rs.reemissionTokenIdBytes) -> 1000000000000L,
    )
    val emissionBox = testBox(emission.coinsTotal, prop, 0, emissionBoxAssets, Map())

    val reemissionReward = rr.reemissionRewardPerBlock

    val inputBoxes = IndexedSeq(emissionBox)
    val inputs = inputBoxes.map(b => Input(b.id, emptyProverResult))

    val newEmissionBox = new ErgoBoxCandidate(emissionBox.value - reemissionReward, prop, nextHeight)
    val minerBox = new ErgoBoxCandidate(reemissionReward, minerProp, nextHeight)

    val spendingTransaction = ErgoLikeTransaction(inputs, IndexedSeq(newEmissionBox, minerBox))

    val ctx = ErgoLikeContextTesting(
      currentHeight = nextHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = pkBytes,
      boxesToSpend = inputBoxes,
      spendingTransaction,
      self = inputBoxes.head,
      0)
    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "checkRewardTx_prove"), prop, ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "checkRewardTx_verify"), prop, ctx, pr, fakeMessage).get._1 shouldBe true
    spendingTransaction
  }

  // also testing payToReemission contract
  property("reemissionBoxProp - merging path") {

  }

}
