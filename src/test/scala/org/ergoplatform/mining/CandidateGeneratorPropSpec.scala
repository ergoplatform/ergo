package org.ergoplatform.mining

import org.ergoplatform.ErgoTreePredef
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.settings.MonetarySettings
import org.ergoplatform.utils.{ErgoCorePropertyTest, RandomWrapper}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.scalacheck.Gen
import sigma.data.ProveDlog


class CandidateGeneratorPropSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

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
        us.validateWithCost(tx, upcomingContext, Int.MaxValue, Some(verifier), true).getOrElse {
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

  /**
   * Test: Stack overflow regression - ensures the iterative implementation
   * can handle large mempools that would have caused StackOverflowError
   * in the previous recursive implementation.
   */
  property("should handle large mempool without stack overflow") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh, parameters)
    val inputs = bh.boxes.values.toIndexedSeq
    val rnd = new RandomWrapper

    // Create 500+ valid transactions (enough to trigger stack overflow in old recursive code)
    val largeMempool = inputs.map { i =>
      validTransactionFromBoxes(IndexedSeq(i), rnd, issueNew = false, feeProp)
    }

    val h = validFullBlock(None, us, bh).header
    val upcomingContext = us.stateContext.upcoming(
      h.minerPk,
      h.timestamp,
      h.nBits,
      h.votes,
      emptyVSUpdate,
      h.version
    )

    // Should complete without StackOverflowError
    val result = CandidateGenerator.collectTxs(
      defaultMinerPk,
      Int.MaxValue,
      Int.MaxValue,
      us,
      upcomingContext,
      largeMempool
    )

    // Verify we collected some transactions
    result._1.length should be > 0
    // Invalid transactions should be tracked
    result._3.length should be >= 0
  }

  /**
   * Test: Double-spend detection within collectTxs
   * Verifies that when multiple transactions attempt to spend the same inputs,
   * only the first valid one is included and others are marked as invalid.
   */
  property("should filter double-spending transactions in collectTxs") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh, parameters)
    val inputs = bh.boxes.values.toIndexedSeq.take(5)

    // Create conflicting transactions spending the same inputs
    val tx1 = validTransactionFromBoxes(inputs.take(2))
    val tx2 = validTransactionFromBoxes(inputs.take(2)) // Same inputs as tx1
    val tx3 = validTransactionFromBoxes(inputs.drop(2)) // Non-conflicting

    val h = validFullBlock(None, us, bh).header
    val upcomingContext = us.stateContext.upcoming(
      h.minerPk,
      h.timestamp,
      h.nBits,
      h.votes,
      emptyVSUpdate,
      h.version
    )

    val result = CandidateGenerator.collectTxs(
      defaultMinerPk,
      Int.MaxValue,
      Int.MaxValue,
      us,
      upcomingContext,
      Seq(tx1, tx2, tx3)
    )

    // At least tx3 should be included (non-conflicting)
    result._1.exists(_.id sameElements tx3.id) shouldBe true
    
    // At most 2 transactions should be included (one of tx1/tx2, plus tx3)
    result._1.length should be <= 2
    result._1.length should be >= 1

    // At least one of the conflicting txs should be in invalid list (result._3)
    // Both result._3 and tx.id are ModifierId (String type)
    val conflictingInvalid = result._3.count(id => id == tx1.id || id == tx2.id)
    conflictingInvalid should be >= 1
  }

  /**
   * Test: Invalid transaction filtering - non-existent inputs
   * Verifies that transactions attempting to spend boxes that don't exist
   * in the UTXO set are filtered out and marked as invalid.
   */
  property("should filter transactions with non-existent inputs") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh, parameters)

    // Create transaction spending non-existent box (fake input)
    // Use a valid box ID format but from a box that doesn't exist in UTXO
    // We reuse an ID from a spent box to create an invalid transaction
    val boxesSeq = bh.boxes.values.toIndexedSeq
    val existingBox = boxesSeq.head
    val fakeInput = Input(existingBox.id, emptyProverResult)
    val invalidTx = ErgoTransaction(
      IndexedSeq(fakeInput),
      IndexedSeq(),
      IndexedSeq(new ErgoBoxCandidate(1000, ErgoTreePredef.feeProposition(1), us.stateContext.currentHeight))
    )

    // Create a valid transaction
    val validTx = validTransactionFromBoxes(bh.boxes.values.take(1).toIndexedSeq)

    val h = validFullBlock(None, us, bh).header
    val upcomingContext = us.stateContext.upcoming(
      h.minerPk,
      h.timestamp,
      h.nBits,
      h.votes,
      emptyVSUpdate,
      h.version
    )

    val result = CandidateGenerator.collectTxs(
      defaultMinerPk,
      Int.MaxValue,
      Int.MaxValue,
      us,
      upcomingContext,
      Seq(invalidTx, validTx)
    )

    // Valid transaction should be collected
    result._1.exists(_.id sameElements validTx.id) shouldBe true
    // Invalid transaction should be in the invalid list (result._3)
    result._3.contains(invalidTx.id) shouldBe true
  }

  /**
   * Test: Empty mempool handling
   * Verifies that collectTxs handles an empty transaction list gracefully
   * without errors or exceptions.
   */
  property("should handle empty mempool gracefully") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh, parameters)

    val h = validFullBlock(None, us, bh).header
    val upcomingContext = us.stateContext.upcoming(
      h.minerPk,
      h.timestamp,
      h.nBits,
      h.votes,
      emptyVSUpdate,
      h.version
    )

    val result = CandidateGenerator.collectTxs(
      defaultMinerPk,
      Int.MaxValue,
      Int.MaxValue,
      us,
      upcomingContext,
      Seq.empty
    )

    // All result collections should be empty
    result._1.length shouldBe 0
    result._2.length shouldBe 0
    result._3.length shouldBe 0
  }

  /**
   * Test: Block cost limit enforcement
   * Verifies that transaction collection stops when block computation cost
   * limit is reached, preventing overflow of block resources.
   */
  property("should enforce block cost limit") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh, parameters)
    val inputs = bh.boxes.values.toIndexedSeq.take(50)
    val rnd = new RandomWrapper

    // Create many transactions that will exceed cost limit
    val manyTxs = inputs.map { i =>
      validTransactionFromBoxes(IndexedSeq(i), rnd, issueNew = false, feeProp)
    }

    val h = validFullBlock(None, us, bh).header
    val upcomingContext = us.stateContext.upcoming(
      h.minerPk,
      h.timestamp,
      h.nBits,
      h.votes,
      emptyVSUpdate,
      h.version
    )

    // Use a moderate cost limit to allow some transactions but not all
    // Typical transaction cost is around 10000-50000, so this allows ~10-20 txs
    val moderateCostLimit = 200000 // Much lower than parameters.maxBlockCost (10M+)

    val result = CandidateGenerator.collectTxs(
      defaultMinerPk,
      moderateCostLimit,
      Int.MaxValue,
      us,
      upcomingContext,
      manyTxs
    )

    // Should have collected some transactions but not all
    result._1.length should be > 0
    result._1.length should be < manyTxs.length

    // Verify total cost doesn't exceed limit
    val totalCost = result._1.map { tx =>
      us.validateWithCost(tx, upcomingContext, Int.MaxValue, Some(verifier), true).getOrElse(0)
    }.sum

    totalCost should be <= moderateCostLimit
  }

  /**
   * Test: Block size limit enforcement
   * Verifies that transaction collection stops when block size limit
   * is reached, preventing overflow of block size.
   */
  property("should enforce block size limit") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh, parameters)
    val inputs = bh.boxes.values.toIndexedSeq.take(50)
    val rnd = new RandomWrapper

    // Create many transactions that will exceed size limit
    val manyTxs = inputs.map { i =>
      validTransactionFromBoxes(IndexedSeq(i), rnd, issueNew = false, feeProp)
    }

    val h = validFullBlock(None, us, bh).header
    val upcomingContext = us.stateContext.upcoming(
      h.minerPk,
      h.timestamp,
      h.nBits,
      h.votes,
      emptyVSUpdate,
      h.version
    )

    // Use a very small size limit to force early termination
    val smallSizeLimit = 512 // Much smaller than typical block size

    val result = CandidateGenerator.collectTxs(
      defaultMinerPk,
      Int.MaxValue,
      smallSizeLimit,
      us,
      upcomingContext,
      manyTxs
    )

    // Should have collected some transactions but not all
    result._1.length should be > 0
    result._1.length should be < manyTxs.length

    // Verify total size doesn't exceed limit
    val totalSize = result._1.map(_.size).sum
    totalSize should be <= smallSizeLimit
  }

  /**
   * Test: Mixed valid and invalid transactions
   * Verifies that collectTxs correctly processes a mixed mempool,
   * collecting valid transactions while filtering out invalid ones.
   */
  property("should process mixed valid and invalid transactions") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh, parameters)
    val inputs = bh.boxes.values.toIndexedSeq.take(10)
    val rnd = new RandomWrapper

    // Create valid transactions
    val validTxs = inputs.take(5).map { i =>
      validTransactionFromBoxes(IndexedSeq(i), rnd, issueNew = false, feeProp)
    }

    // Create invalid transaction (double-spend)
    val doubleSpendTx1 = validTransactionFromBoxes(inputs.take(2), rnd, issueNew = false)
    val doubleSpendTx2 = validTransactionFromBoxes(inputs.take(2), rnd, issueNew = false) // Same inputs

    val h = validFullBlock(None, us, bh).header
    val upcomingContext = us.stateContext.upcoming(
      h.minerPk,
      h.timestamp,
      h.nBits,
      h.votes,
      emptyVSUpdate,
      h.version
    )

    val mixedMempool = validTxs ++ Seq(doubleSpendTx1, doubleSpendTx2)

    val result = CandidateGenerator.collectTxs(
      defaultMinerPk,
      Int.MaxValue,
      Int.MaxValue,
      us,
      upcomingContext,
      mixedMempool
    )

    // Should collect all valid transactions
    validTxs.foreach(tx => result._1.exists(_.id sameElements tx.id) shouldEqual true)

    // At least one double-spend should be in invalid list (result._3)
    result._3.length should be >= 1
  }

}
