package org.ergoplatform.mining

import org.ergoplatform.ErgoTreePredef
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.settings.MonetarySettings
import org.ergoplatform.utils.{ErgoPropertyTest, RandomWrapper}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.scalacheck.Gen
import sigmastate.crypto.DLogProtocol.ProveDlog

import scala.concurrent.duration._

class CandidateGeneratorPropSpec extends ErgoPropertyTest {

  val delta: Int = settings.chainSettings.monetary.minerRewardDelay

  private def expectedRewardOutputScriptBytes(pk: ProveDlog): Array[Byte] =
    ErgoTreePredef.rewardOutputScript(delta, pk).bytes

  implicit private val verifier: ErgoInterpreter = ErgoInterpreter(parameters)

  property("minersRewardAtHeight test vectors") {
    emission.minersRewardAtHeight(525000) shouldBe 67500000000L
    emission.minersRewardAtHeight(525600) shouldBe 67500000000L
    emission.minersRewardAtHeight(590400) shouldBe 67500000000L
    emission.minersRewardAtHeight(655200) shouldBe 66000000000L
    emission.minersRewardAtHeight(720000) shouldBe 63000000000L
    emission.minersRewardAtHeight(784800) shouldBe 60000000000L
    emission.minersRewardAtHeight(849600) shouldBe 57000000000L
    emission.minersRewardAtHeight(914400) shouldBe 54000000000L
    emission.minersRewardAtHeight(979200) shouldBe 51000000000L
    emission.minersRewardAtHeight(1044000) shouldBe 48000000000L
    emission.minersRewardAtHeight(1108800) shouldBe 45000000000L
    emission.minersRewardAtHeight(1173600) shouldBe 42000000000L
    emission.minersRewardAtHeight(1238400) shouldBe 39000000000L
    emission.minersRewardAtHeight(1303200) shouldBe 36000000000L
    emission.minersRewardAtHeight(1368000) shouldBe 33000000000L
    emission.minersRewardAtHeight(1432800) shouldBe 30000000000L
    emission.minersRewardAtHeight(1497600) shouldBe 27000000000L
    emission.minersRewardAtHeight(1562400) shouldBe 24000000000L
    emission.minersRewardAtHeight(1627200) shouldBe 21000000000L
    emission.minersRewardAtHeight(1692000) shouldBe 18000000000L
    emission.minersRewardAtHeight(1756800) shouldBe 15000000000L
    emission.minersRewardAtHeight(1821600) shouldBe 12000000000L
    emission.minersRewardAtHeight(1886400) shouldBe 9000000000L
    emission.minersRewardAtHeight(1951200) shouldBe 6000000000L
    emission.minersRewardAtHeight(2016000) shouldBe 3000000000L
    emission.minersRewardAtHeight(2080799) shouldBe 3000000000L
    emission.minersRewardAtHeight(2080800) shouldBe 0L
  }

  property("collect reward from emission box only") {
    val us = createUtxoState(settings)._1
    us.emissionBoxOpt should not be None
    val expectedReward = emission.minersRewardAtHeight(us.stateContext.currentHeight)

    val incorrectTxs =
      CandidateGenerator.collectEmission(us, proveDlogGen.sample.get, emptyStateContext).toSeq
    val txs = CandidateGenerator.collectEmission(us, defaultMinerPk, emptyStateContext).toSeq

    txs.size shouldBe 1
    val emissionTx = txs.head
    emissionTx.outputs.length shouldBe 2
    emissionTx.outputs.last.value shouldBe expectedReward
    emissionTx.outputs.last.propositionBytes shouldEqual expectedRewardOutputScriptBytes(
      defaultMinerPk
    )

    us.applyModifier(validFullBlock(None, us, incorrectTxs), None)(_ => ()) shouldBe 'failure
    us.applyModifier(validFullBlock(None, us, txs), None)(_ => ()) shouldBe 'success
  }

  property("collect reward from transaction fees only") {
    val bh     = boxesHolderGen.sample.get
    val us     = createUtxoState(bh, parameters)
    val height = us.stateContext.currentHeight
    val blockTx = validTransactionFromBoxes(
      bh.boxes.take(2).values.toIndexedSeq,
      outputsProposition = feeProp
    )

    val txs =
      CandidateGenerator.collectFees(height, Seq(blockTx), defaultMinerPk, emptyStateContext).toSeq
    val incorrect = CandidateGenerator
      .collectFees(height, Seq(blockTx), proveDlogGen.sample.get, emptyStateContext)
      .toSeq
    txs.length shouldBe 1
    val feeTx = txs.head
    feeTx.outputs.length shouldBe 1
    feeTx.outputs.head.value shouldBe txs.flatMap(_.outputs).map(_.value).sum
    feeTx.outputs.head.propositionBytes shouldEqual expectedRewardOutputScriptBytes(
      defaultMinerPk
    )

    us.applyModifier(validFullBlock(None, us, blockTx +: incorrect), None)(_ => ()) shouldBe 'failure
    us.applyModifier(validFullBlock(None, us, blockTx +: txs), None)(_ => ()) shouldBe 'success
  }

  property("filter out double spend txs") {
    val tx = validErgoTransactionGen.sample.get._2
    CandidateGenerator.doublespend(Seq(tx), tx) shouldBe true

    val inputs = validErgoTransactionGenTemplate(minAssets = 0, maxAssets = -1).sample.get._1
    val (l, r) = inputs.splitAt(50)
    val tx_1   = validTransactionFromBoxes(l)
    val tx_2   = validTransactionFromBoxes(r :+ l.last) //conflicting with tx_1
    val tx_3   = validTransactionFromBoxes(r) //conflicting with tx_2, not conflicting with tx_1

    CandidateGenerator.doublespend(Seq(tx_1), tx_2) shouldBe true
    CandidateGenerator.doublespend(Seq(tx_1), tx_3) shouldBe false
    CandidateGenerator.doublespend(Seq(tx_1, tx_2), tx_1) shouldBe true
    CandidateGenerator.doublespend(Seq(tx_1, tx_2), tx_2) shouldBe true
    CandidateGenerator.doublespend(Seq(tx_1, tx_3), tx) shouldBe false
  }

  property("should only collect valid transactions") {
    def checkCollectTxs(
      maxCost: Int,
      maxSize: Int,
      withTokens: Boolean = false
    ): Unit = {

      val bh          = boxesHolderGen.sample.get
      val rnd         = new RandomWrapper
      val us          = createUtxoState(bh, parameters)
      val inputs      = bh.boxes.values.toIndexedSeq.takeRight(100)
      val txsWithFees = inputs.map(i =>
        validTransactionFromBoxes(IndexedSeq(i), rnd, issueNew = withTokens, feeProp)
      )
      val head = txsWithFees.head

      val h = validFullBlock(None, us, bh, rnd).header
      val upcomingContext = us.stateContext.upcoming(
        h.minerPk,
        h.timestamp,
        h.nBits,
        h.votes,
        emptyVSUpdate,
        h.version
      )
      upcomingContext.currentHeight shouldBe (us.stateContext.currentHeight + 1)

      val fromSmallMempool = CandidateGenerator
        .collectTxs(
          defaultMinerPk,
          maxCost,
          maxSize,
          us,
          upcomingContext,
          Seq(head)
        )
        ._1
      fromSmallMempool.size shouldBe 2
      fromSmallMempool.contains(head) shouldBe true

      val fromBigMempool = CandidateGenerator
        .collectTxs(
          defaultMinerPk,
          maxCost,
          maxSize,
          us,
          upcomingContext,
          txsWithFees
        )
        ._1

      val newBoxes = fromBigMempool.flatMap(_.outputs)
      val costs: Seq[Int] = fromBigMempool.map { tx =>
        us.validateWithCost(tx, upcomingContext, Int.MaxValue, Some(verifier)).getOrElse {
          val boxesToSpend =
            tx.inputs.map(i => newBoxes.find(b => b.id sameElements i.boxId).get)
          tx.statefulValidity(boxesToSpend, IndexedSeq(), upcomingContext).get
        }
      }

      fromBigMempool.length should be > 2
      fromBigMempool.map(_.size).sum should be < maxSize
      costs.sum should be < maxCost
      if (!withTokens) fromBigMempool.size should be < txsWithFees.size
    }

    // transactions reach computation cost block limit
    checkCollectTxs(parameters.maxBlockCost, Int.MaxValue)

    // transactions reach block size limit
    checkCollectTxs(Int.MaxValue, 4096)

    // miner collects correct transactions from mempool even if they have tokens
    checkCollectTxs(Int.MaxValue, Int.MaxValue, withTokens = true)

  }

  property("should not be able to spend recent fee boxes") {

    val delta          = 1
    val inputsNum      = 2
    val feeProposition = ErgoTreePredef.feeProposition(delta)

    val bh     = boxesHolderGen.sample.get
    var us     = createUtxoState(bh, parameters)
    val height = EmptyHistoryHeight

    val ms = MonetarySettings(minerRewardDelay = delta)
    val st = settings.copy(chainSettings = settings.chainSettings.copy(monetary = ms))
    val sc = ErgoStateContext.empty(genesisStateDigest, st.chainSettings, parameters)
    val txBoxes = bh.boxes.grouped(inputsNum).map(_.values.toIndexedSeq).toSeq

    val blockTx =
      validTransactionFromBoxes(txBoxes.head, outputsProposition = feeProposition)
    val txs = CandidateGenerator
      .collectFees(height, Seq(blockTx), defaultMinerPk, sc)
      .toSeq
    val block = validFullBlock(None, us, blockTx +: txs)

    us = us.applyModifier(block, None)(_ => ()).get

    val blockTx2 =
      validTransactionFromBoxes(txBoxes(1), outputsProposition = feeProposition)
    val block2 = validFullBlock(Some(block), us, IndexedSeq(blockTx2))

    val earlySpendingTx =
      validTransactionFromBoxes(txs.head.outputs, stateCtxOpt = Some(us.stateContext))

    val invalidBlock2 =
      validFullBlock(Some(block), us, IndexedSeq(earlySpendingTx, blockTx2))

    us.applyModifier(invalidBlock2, None)(_ => ()) shouldBe 'failure

    us = us.applyModifier(block2, None)(_ => ()).get

    val earlySpendingTx2 =
      validTransactionFromBoxes(txs.head.outputs, stateCtxOpt = Some(us.stateContext))

    val blockTx3 =
      validTransactionFromBoxes(txBoxes(2), outputsProposition = feeProposition)
    val block3 = validFullBlock(Some(block2), us, IndexedSeq(earlySpendingTx2, blockTx3))

    us.applyModifier(block3, None)(_ => ()) shouldBe 'success
  }

  property("collect reward from both emission box and fees") {
    val (us, _) = createUtxoState(settings)
    us.emissionBoxOpt should not be None
    val expectedReward = emission.minersRewardAtHeight(us.stateContext.currentHeight)

    forAll(
      Gen.nonEmptyListOf(validErgoTransactionGenTemplate(minAssets = 0, propositionGen = feeProp))
    ) { btxs =>
      val blockTxs = btxs.map(_._2)
      val height   = EmptyHistoryHeight
      val txs = CandidateGenerator.collectRewards(
        us.emissionBoxOpt,
        height,
        blockTxs,
        defaultMinerPk,
        emptyStateContext
      )
      txs.length shouldBe 2

      val emissionTx = txs.head
      emissionTx.outputs.length shouldBe 2
      emissionTx.outputs.last.value shouldBe expectedReward
      emissionTx.outputs.last.propositionBytes shouldEqual expectedRewardOutputScriptBytes(
        defaultMinerPk
      )

      val feeTx = txs.last
      feeTx.outputs.length shouldBe 1
      feeTx.outputs.head.value shouldBe blockTxs.flatMap(_.outputs).map(_.value).sum
      feeTx.outputs.head.propositionBytes shouldEqual expectedRewardOutputScriptBytes(
        defaultMinerPk
      )
    }
  }

  property("it should calculate average block mining time from creation timestamps") {
    val timestamps1 = System.currentTimeMillis()
    val timestamps2 = timestamps1 + 100
    val timestamps3 = timestamps2 + 200
    val timestamps4 = timestamps3 + 300
    val avgMiningTime = {
      CandidateGenerator.getBlockMiningTimeAvg(
        Vector(timestamps1, timestamps2, timestamps3, timestamps4)
      )
    }
    avgMiningTime shouldBe 200.millis
  }
}
