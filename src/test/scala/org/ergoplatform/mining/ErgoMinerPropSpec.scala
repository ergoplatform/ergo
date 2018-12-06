package org.ergoplatform.mining

import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.MonetarySettings
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scapi.sigma.DLogProtocol.ProveDlog

class ErgoMinerPropSpec extends ErgoPropertyTest {

  val delta: Int = settings.emission.settings.minerRewardDelay
  val expectedBytes: Array[Byte] = ErgoState.rewardOutputScriptStartBytes(delta)

  property("rewardOutputScriptStartBytes correct serialization") {
    def checkBytes(d: Int) = {
      val bytes = ErgoState.rewardOutputScript(d, ProveDlog(group.generator)).bytes.dropRight(PublicKeyLength)
      bytes shouldEqual ErgoState.rewardOutputScriptStartBytes(d)
    }

    forAll { d: Int =>
      checkBytes(d)
    }
    checkBytes(720)
    checkBytes(delta)
  }

  property("collect reward from emission box only") {
    val us = createUtxoState()._1
    us.emissionBoxOpt should not be None
    val expectedReward = us.constants.emission.emissionAtHeight(us.stateContext.currentHeight)

    val incorrectTxs = ErgoMiner.collectRewards(us, Seq(), proveDlogGen.sample.get, us.constants.emission)
    val txs = ErgoMiner.collectRewards(us, Seq(), defaultMinerPk, us.constants.emission)

    txs.size shouldBe 1
    val emissionTx = txs.head
    emissionTx.outputs.length shouldBe 2
    emissionTx.outputs.last.value shouldBe expectedReward
    emissionTx.outputs.last.propositionBytes shouldEqual expectedBytes ++ defaultMinerPk.pkBytes

    us.applyModifier(validFullBlock(None, us, incorrectTxs)) shouldBe 'failure
    us.applyModifier(validFullBlock(None, us, txs)) shouldBe 'success
  }

  property("collect reward from transaction fees only") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh)
    val height = us.stateContext.currentHeight
    val blockTx = validTransactionFromBoxes(bh.boxes.take(10).values.toIndexedSeq, outputsProposition = feeProp)

    val txs = ErgoMiner.collectRewards(None, height, Seq(blockTx), defaultMinerPk, settings.emission)
    val incorrect = ErgoMiner.collectRewards(None, height, Seq(blockTx), proveDlogGen.sample.get, settings.emission)
    txs.length shouldBe 1
    val feeTx = txs.head
    feeTx.outputs.length shouldBe 1
    feeTx.outputs.head.value shouldBe txs.flatMap(_.outputs).map(_.value).sum
    feeTx.outputs.head.propositionBytes shouldEqual expectedBytes ++ defaultMinerPk.pkBytes

    us.applyModifier(validFullBlock(None, us, blockTx +: incorrect)) shouldBe 'failure
    us.applyModifier(validFullBlock(None, us, blockTx +: txs)) shouldBe 'success
  }

  ignore("should not be able to spend recent fee boxes") {

    val delta = 1
    val feeProposition = ErgoState.feeProposition(delta)

    val bh = boxesHolderGen.sample.get
    var us = createUtxoState(bh)
    val height = ErgoHistory.GenesisHeight

    val emissionRules = new EmissionRules(
      MonetarySettings(
        minerRewardDelay = delta,
        afterGenesisStateDigestHex = "584748265afc5bd6d7fb80f750131b923a431ae6e4bedc2b590d49dd81ef64b601"
      )
    )

    val blockTx = validTransactionFromBoxes(bh.boxes.take(5).values.toIndexedSeq, outputsProposition = feeProposition)
    val txs = ErgoMiner.collectRewards(None, height, Seq(blockTx), defaultMinerPk, emissionRules)
    val block = validFullBlock(None, us, blockTx +: txs)

    us = us.applyModifier(block).get

    val blockTx2 = validTransactionFromBoxes(bh.boxes.slice(10, 20).values.toIndexedSeq, outputsProposition = feeProposition)
    val txs2 = ErgoMiner.collectRewards(None, height + 1, Seq(blockTx2), defaultMinerPk, emissionRules)
    val block2 = validFullBlock(Some(block.header), us, blockTx2 +: txs2)

    val earlySpendingTx = validTransactionFromBoxes(txs.head.outputs)

    val invalidBlock2 = validFullBlock(Some(block.header), us, earlySpendingTx +: blockTx2 +: txs2)

    us.applyModifier(invalidBlock2) shouldBe 'failure

    us = us.applyModifier(block2).get

    val blockTx3 = validTransactionFromBoxes(bh.boxes.slice(20, 30).values.toIndexedSeq, outputsProposition = feeProposition)
    val txs3 = ErgoMiner.collectRewards(None, height + 2, Seq(blockTx3), defaultMinerPk, emissionRules)
    val block3 = validFullBlock(Some(block2.header), us, earlySpendingTx +: blockTx3 +: txs3)

    us.applyModifier(block3) shouldBe 'success
  }

  property("collect reward from both emission box and fees") {
    val (us, _) = createUtxoState()
    us.emissionBoxOpt should not be None
    val expectedReward = us.constants.emission.emissionAtHeight(us.stateContext.currentHeight)

    forAll(Gen.nonEmptyListOf(validErgoTransactionGenTemplate(0, propositionGen = feeProp))) { btxs =>
      val blockTxs = btxs.map(_._2)
      val height = Int.MaxValue
      val txs = ErgoMiner.collectRewards(us.emissionBoxOpt, height, blockTxs, defaultMinerPk, settings.emission)
      txs.length shouldBe 2

      val emissionTx = txs.head
      emissionTx.outputs.length shouldBe 2
      emissionTx.outputs.last.value shouldBe expectedReward
      emissionTx.outputs.last.propositionBytes shouldEqual expectedBytes ++ defaultMinerPk.pkBytes

      val feeTx = txs.last
      feeTx.outputs.length shouldBe 1
      feeTx.outputs.head.value shouldBe blockTxs.flatMap(_.outputs).map(_.value).sum
      feeTx.outputs.head.propositionBytes shouldEqual expectedBytes ++ defaultMinerPk.pkBytes
    }
  }
}
