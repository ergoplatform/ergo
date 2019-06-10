package org.ergoplatform.mining

import org.ergoplatform.ErgoScriptPredef
import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.{LaunchParameters, MonetarySettings}
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.scalacheck.Gen
import sigmastate.basics.DLogProtocol.ProveDlog

import scala.util.Random

class ErgoMinerPropSpec extends ErgoPropertyTest {

  val delta: Int = settings.chainSettings.monetary.minerRewardDelay

  private def expectedRewardOutputScriptBytes(pk: ProveDlog): Array[Byte] =
    ErgoScriptPredef.rewardOutputScript(delta, pk).bytes

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)

  property("collect reward from emission box only") {
    val us = createUtxoState()._1
    us.emissionBoxOpt should not be None
    val expectedReward = emission.minersRewardAtHeight(us.stateContext.currentHeight)

    val incorrectTxs = ErgoMiner.collectEmission(us, proveDlogGen.sample.get, emission).toSeq
    val txs = ErgoMiner.collectEmission(us, defaultMinerPk, emission).toSeq

    txs.size shouldBe 1
    val emissionTx = txs.head
    emissionTx.outputs.length shouldBe 2
    emissionTx.outputs.last.value shouldBe expectedReward
    emissionTx.outputs.last.propositionBytes shouldEqual expectedRewardOutputScriptBytes(defaultMinerPk)

    us.applyModifier(validFullBlock(None, us, incorrectTxs)) shouldBe 'failure
    us.applyModifier(validFullBlock(None, us, txs)) shouldBe 'success
  }

  property("collect reward from transaction fees only") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh)
    val height = us.stateContext.currentHeight
    val blockTx = validTransactionFromBoxes(bh.boxes.take(2).values.toIndexedSeq, outputsProposition = feeProp)

    val txs = ErgoMiner.collectFees(height, Seq(blockTx), defaultMinerPk, emission).toSeq
    val incorrect = ErgoMiner.collectFees(height, Seq(blockTx), proveDlogGen.sample.get, emission).toSeq
    txs.length shouldBe 1
    val feeTx = txs.head
    feeTx.outputs.length shouldBe 1
    feeTx.outputs.head.value shouldBe txs.flatMap(_.outputs).map(_.value).sum
    feeTx.outputs.head.propositionBytes shouldEqual expectedRewardOutputScriptBytes(defaultMinerPk)

    us.applyModifier(validFullBlock(None, us, blockTx +: incorrect)) shouldBe 'failure
    us.applyModifier(validFullBlock(None, us, blockTx +: txs)) shouldBe 'success
  }


  property("filter out double spend txs") {
    val cost = 1L
    val tx = validErgoTransactionGen.sample.get._2 -> cost
    ErgoMiner.fixTxsConflicts(Seq(tx, tx, tx)) should have length 1

    val inputs = validErgoTransactionGenTemplate(0, -1, 100).sample.get._1
    val (l, r) = inputs.splitAt(50)
    val tx_1 = validTransactionFromBoxes(l) -> cost
    val tx_2 = validTransactionFromBoxes(r :+ l.last) -> cost

    ErgoMiner.fixTxsConflicts(Seq(tx_1, tx_2, tx)) should contain theSameElementsAs Seq(tx_1, tx)
    ErgoMiner.fixTxsConflicts(Seq(tx_2, tx_1, tx)) should contain theSameElementsAs Seq(tx_2, tx)
  }

  property("should only collect valid transactions") {
    def checkCollectTxs(maxCost: Long, maxSize: Int, withTokens: Boolean = false): Unit = {

      val bh = boxesHolderGen.sample.get
      val rnd: Random = new Random
      val us = createUtxoState(bh)
      val inputs = bh.boxes.values.toIndexedSeq.takeRight(100)
      val txsWithFees = inputs.map(i => validTransactionFromBoxes(IndexedSeq(i), rnd, issueNew = withTokens, feeProp))
      val head = txsWithFees.head

      val h = validFullBlock(None, us, bh, rnd).header
      val upcomingContext = us.stateContext.upcoming(h.minerPk, h.timestamp, h.nBits, h.votes, emptyVSUpdate, h.version)
      upcomingContext.currentHeight shouldBe (us.stateContext.currentHeight + 1)

      val fromSmallMempool = ErgoMiner.collectTxs(defaultMinerPk, maxCost, maxSize, us, upcomingContext, Seq(head), Seq())(validationSettingsNoIl)._1
      fromSmallMempool.size shouldBe 2
      fromSmallMempool.contains(head) shouldBe true

      val fromBigMempool = ErgoMiner.collectTxs(defaultMinerPk, maxCost, maxSize, us, upcomingContext, txsWithFees, Seq())(validationSettingsNoIl)._1

      val newBoxes = fromBigMempool.flatMap(_.outputs)
      val costs: Seq[Long] = fromBigMempool.map { tx =>
        us.validateWithCost(tx, Some(upcomingContext)).getOrElse {
          val boxesToSpend = tx.inputs.map(i => newBoxes.find(b => b.id sameElements i.boxId).get)
          tx.statefulValidity(boxesToSpend, IndexedSeq(), upcomingContext).get
        }
      }

      fromBigMempool.length should be > 2
      fromBigMempool.map(_.size).sum should be < maxSize
      costs.sum should be < maxCost
      if (!withTokens) fromBigMempool.size should be < txsWithFees.size
    }

    // transactions reach computation cost block limit
    checkCollectTxs(LaunchParameters.maxBlockCost, Int.MaxValue)

    // transactions reach block size limit
    checkCollectTxs(Long.MaxValue, 4096)

    // miner collects correct transactions from mempool even if they have tokens
    checkCollectTxs(Int.MaxValue, Int.MaxValue, withTokens = true)

  }

  property("should not be able to spend recent fee boxes") {

    val delta = 1
    val inputsNum = 2
    val feeProposition = ErgoScriptPredef.feeProposition(delta)

    val bh = boxesHolderGen.sample.get
    var us = createUtxoState(bh)
    val height = ErgoHistory.EmptyHistoryHeight

    val emissionRules = new EmissionRules(
      MonetarySettings(
        minerRewardDelay = delta
      )
    )
    val txBoxes = bh.boxes.grouped(inputsNum).map(_.values.toIndexedSeq).toSeq

    val blockTx = validTransactionFromBoxes(txBoxes(0), outputsProposition = feeProposition)
    val txs = ErgoMiner.collectFees(height, Seq(blockTx), defaultMinerPk, emissionRules).toSeq
    val block = validFullBlock(None, us, blockTx +: txs)

    us = us.applyModifier(block).get

    val blockTx2 = validTransactionFromBoxes(txBoxes(1), outputsProposition = feeProposition)
    val block2 = validFullBlock(Some(block), us, IndexedSeq(blockTx2))

    val earlySpendingTx = validTransactionFromBoxes(txs.head.outputs, stateCtxOpt = Some(us.stateContext))

    val invalidBlock2 = validFullBlock(Some(block), us, IndexedSeq(earlySpendingTx, blockTx2))

    us.applyModifier(invalidBlock2) shouldBe 'failure

    us = us.applyModifier(block2).get

    val earlySpendingTx2 = validTransactionFromBoxes(txs.head.outputs, stateCtxOpt = Some(us.stateContext))

    val blockTx3 = validTransactionFromBoxes(txBoxes(2), outputsProposition = feeProposition)
    val block3 = validFullBlock(Some(block2), us, IndexedSeq(earlySpendingTx2, blockTx3))

    us.applyModifier(block3) shouldBe 'success
  }

  property("collect reward from both emission box and fees") {
    val (us, _) = createUtxoState()
    us.emissionBoxOpt should not be None
    val expectedReward = emission.minersRewardAtHeight(us.stateContext.currentHeight)

    forAll(Gen.nonEmptyListOf(validErgoTransactionGenTemplate(0, propositionGen = feeProp))) { btxs =>
      val blockTxs = btxs.map(_._2)
      val height = ErgoHistory.EmptyHistoryHeight
      val txs = ErgoMiner.collectRewards(us.emissionBoxOpt, height, blockTxs, defaultMinerPk, emission)
      txs.length shouldBe 2

      val emissionTx = txs.head
      emissionTx.outputs.length shouldBe 2
      emissionTx.outputs.last.value shouldBe expectedReward
      emissionTx.outputs.last.propositionBytes shouldEqual expectedRewardOutputScriptBytes(defaultMinerPk)

      val feeTx = txs.last
      feeTx.outputs.length shouldBe 1
      feeTx.outputs.head.value shouldBe blockTxs.flatMap(_.outputs).map(_.value).sum
      feeTx.outputs.head.propositionBytes shouldEqual expectedRewardOutputScriptBytes(defaultMinerPk)
    }
  }

}
