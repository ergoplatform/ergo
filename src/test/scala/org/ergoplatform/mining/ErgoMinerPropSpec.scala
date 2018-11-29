package org.ergoplatform.mining

import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen

class ErgoMinerPropSpec extends ErgoPropertyTest {

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
    emissionTx.outputs.last.proposition shouldBe defaultMinerPk

    us.applyModifier(validFullBlock(None, us, incorrectTxs)) shouldBe 'failure
    us.applyModifier(validFullBlock(None, us, txs)) shouldBe 'success
  }

  property("collect reward from transaction fees only") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh)
    val height = ErgoHistory.GenesisHeight
    val blockTx = validTransactionFromBoxes(bh.boxes.take(10).values.toIndexedSeq,
      outputsProposition = Constants.FeeProposition)

    val txs = ErgoMiner.collectRewards(None, height, Seq(blockTx), defaultMinerPk, settings.emission)
    val incorrect = ErgoMiner.collectRewards(None, height, Seq(blockTx), proveDlogGen.sample.get, settings.emission)
    txs.length shouldBe 1
    val feeTx = txs.head
    feeTx.outputs.length shouldBe 1
    feeTx.outputs.head.value shouldBe txs.flatMap(_.outputs).map(_.value).sum
    feeTx.outputs.head.proposition shouldBe defaultMinerPk

    us.applyModifier(validFullBlock(None, us, blockTx +: incorrect)) shouldBe 'failure
    us.applyModifier(validFullBlock(None, us, blockTx +: txs)) shouldBe 'success
  }

  property("collect reward from both emission box and fees") {
    val (us, _) = createUtxoState()
    us.emissionBoxOpt should not be None
    val expectedReward = us.constants.emission.emissionAtHeight(us.stateContext.currentHeight)

    forAll(Gen.nonEmptyListOf(validErgoTransactionGenTemplate(0, propositionGen = Constants.FeeProposition))) { btxs =>
      val blockTxs = btxs.map(_._2)
      val height = Int.MaxValue
      val txs = ErgoMiner.collectRewards(us.emissionBoxOpt, height, blockTxs, defaultMinerPk, settings.emission)
      txs.length shouldBe 2

      val emissionTx = txs.head
      emissionTx.outputs.length shouldBe 2
      emissionTx.outputs.last.value shouldBe expectedReward
      emissionTx.outputs.last.proposition shouldBe defaultMinerPk

      val feeTx = txs.last
      feeTx.outputs.length shouldBe 1
      feeTx.outputs.head.value shouldBe blockTxs.flatMap(_.outputs).map(_.value).sum
      feeTx.outputs.head.proposition shouldBe defaultMinerPk
    }
  }
}
