package org.ergoplatform.mining

import org.ergoplatform.{ErgoBoxCandidate, ErgoTreePredef}
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.{ErgoStateContext, StateType, UtxoState}
import org.ergoplatform.settings.{MonetarySettings, Parameters}
import org.ergoplatform.utils.{BoxUtils, ErgoCorePropertyTest, HistoryTestHelpers, RandomWrapper}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.scalacheck.Gen
import scorex.util.{ModifierId, bytesToId}
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import sigma.data.ProveDlog

import scala.concurrent.duration._
import scala.util.{Failure, Try}

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
      val minValue    = BoxUtils.sufficientAmount(parameters)
      val inputs      = bh.boxes.values.toIndexedSeq.filter(_.value >= minValue * 2).takeRight(100)
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
      BlockTransactions(h.id, h.version, fromBigMempool).bytes.length should be <= maxSize
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

  property("transaction section size includes framing at the collection boundary") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh, parameters)
    val input = bh.boxes.values.find(_.value >= BoxUtils.sufficientAmount(parameters) * 2).get
    val headerId = bytesToId(Array.fill(32)(0.toByte))

    for (version <- Seq[Byte](1, 2, 3, 4); withFees <- Seq(false, true)) {
      val tx = validTransactionFromBoxes(
        IndexedSeq(input),
        outputsProposition = if (withFees) feeProp else sigma.ast.ErgoTree.fromSigmaBoolean(sigma.data.TrivialProp.TrueProp)
      )
      val context = us.stateContext.upcoming(
        defaultMinerPk.value, 1L, settings.chainSettings.initialNBits,
        Array.fill(3)(0.toByte), emptyVSUpdate, version
      )
      def collect(limit: Int): Seq[ErgoTransaction] = {
        val (collected, invalid) = CandidateGenerator.collectTxs(
          defaultMinerPk, Int.MaxValue, limit, us, context, Seq(tx)
        )
        invalid shouldBe empty
        collected
      }
      val unconstrained = collect(Int.MaxValue)
      unconstrained should contain(tx)
      unconstrained.size shouldBe (if (withFees) 2 else 1)
      val sectionSize = BlockTransactions(headerId, version, unconstrained).bytes.length
      collect(sectionSize) shouldBe unconstrained
      collect(sectionSize - 1) shouldBe empty
    }
  }

  property("transaction section size is checked before normal and fallback work is returned") {
    for (fallback <- Seq(false, true)) {
      val base = createUtxoState(settings)._1
      val history = HistoryTestHelpers.generateHistory(true, StateType.Utxo, false, -1)
      var proofCalls = 0
      val state = new UtxoState(base.persistentProver, base.version, base.store, settings) {
        override def proofsForTransactions(txs: Seq[ErgoTransaction]): Try[(SerializedAdProof, ADDigest)] = {
          proofCalls += 1
          if (fallback && proofCalls == 1) Failure(new IllegalStateException("Proof generation unavailable"))
          else super.proofsForTransactions(txs)
        }
      }
      try {
        val emissionTx = CandidateGenerator.collectEmission(state, defaultMinerPk, emptyStateContext)
        val (result, _) = CandidateGenerator.createCandidate(
          defaultMinerPk, history, emptyVSUpdate, state, Seq.empty, emissionTx, Seq.empty, settings
        ).get
        proofCalls shouldBe (if (fallback) 2 else 1)
        result.candidateBlock.transactions shouldBe emissionTx.toSeq
        val section = BlockTransactions(bytesToId(Array.fill(32)(0.toByte)),
          result.candidateBlock.version, result.candidateBlock.transactions)
        val limit = section.bytes.length
        CandidateGenerator.candidateSizeWithinLimit(result.candidateBlock.transactions,
          result.candidateBlock.version, limit).get shouldBe true
        CandidateGenerator.candidateSizeWithinLimit(result.candidateBlock.transactions,
          result.candidateBlock.version, limit - 1).get shouldBe false
      } finally {
        history.closeStorage()
        base.store.close()
      }
    }
  }

  for (fallback <- Seq(false, true)) {
    val branch = if (fallback) "fallback" else "normal"
    property(s"$branch candidate creation retains the size budget captured before proof generation") {
      for (budgetReduction <- Seq(0, 1)) {
        val base = createUtxoState(settings)._1
        val history = HistoryTestHelpers.generateHistory(true, StateType.Utxo, false, -1)
        val originalContext = base.stateContext
        var testContext = originalContext
        var proofCalls = 0
        val state = new UtxoState(base.persistentProver, base.version, base.store, settings) {
          override def stateContext: ErgoStateContext = testContext

          override def proofsForTransactions(txs: Seq[ErgoTransaction]): Try[(SerializedAdProof, ADDigest)] = {
            proofCalls += 1
            if (fallback && proofCalls == 1) Failure(new IllegalStateException("Proof generation unavailable"))
            else super.proofsForTransactions(txs).map { proof =>
              // A later store observation must not replace the budget used to select this candidate.
              val sectionSize = BlockTransactions(bytesToId(Array.fill(32)(0.toByte)), 1.toByte, txs).bytes.length
              val params = originalContext.currentParameters
              val finalParameters = new Parameters(params.height,
                params.parametersTable.updated(Parameters.MaxBlockSizeIncrease, sectionSize - budgetReduction),
                params.proposedUpdate)
              testContext = new ErgoStateContext(originalContext.lastHeaders, originalContext.lastExtensionOpt,
                originalContext.genesisStateDigest, finalParameters, originalContext.validationSettings,
                originalContext.votingData)(originalContext.chainSettings)
              proof
            }
          }
        }
        try {
          val emissionTx = CandidateGenerator.collectEmission(state, defaultMinerPk, emptyStateContext)
          val result = CandidateGenerator.createCandidate(
            defaultMinerPk, history, emptyVSUpdate, state, Seq.empty, emissionTx, Seq.empty, settings
          )
          proofCalls shouldBe (if (fallback) 2 else 1)
          result.get._1.candidateBlock.transactions shouldBe emissionTx.toSeq
          BlockTransactions.sizeOf(result.get._1.candidateBlock.transactions, 1.toByte) should be <=
            originalContext.currentParameters.maxBlockSize
        } finally {
          history.closeStorage()
          base.store.close()
        }
      }
    }
  }

  property("a final size discrepancy restores the accepted prefix with its matching fee transaction") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh, parameters)
    try {
      val inputs = bh.boxes.values.filter(_.value >= BoxUtils.sufficientAmount(parameters) * 2).take(2).toIndexedSeq
      inputs.size shouldBe 2
      val txs = inputs.map(input => validTransactionFromBoxes(IndexedSeq(input), outputsProposition = feeProp))
      val context = us.stateContext.upcoming(defaultMinerPk.value, 1L, settings.chainSettings.initialNBits,
        Array.fill(3)(0.toByte), emptyVSUpdate, 1.toByte)
      def selected(input: Seq[ErgoTransaction]): Seq[ErgoTransaction] =
        CandidateGenerator.collectTxs(defaultMinerPk, Int.MaxValue, Int.MaxValue, us, context, input)._1
      val prefix = selected(txs.take(1))
      val complete = selected(txs)
      val limit = BlockTransactions.sizeOf(prefix, 1.toByte)
      BlockTransactions.sizeOf(complete, 1.toByte) should be > limit
      val discardedConflict = txs.last.id
      val restoredResult = CandidateGenerator.checkedCandidate(
        Iterator(complete -> Seq(discardedConflict), prefix -> Seq.empty), 1.toByte, limit)
      val restored = restoredResult._1
      restoredResult._2 shouldBe empty
      restored shouldBe prefix
      restored.last shouldBe CandidateGenerator.collectFees(us.stateContext.currentHeight,
        txs.take(1), defaultMinerPk, context).get
      CandidateGenerator.checkedCandidate(Iterator(complete -> Seq.empty, prefix -> Seq.empty),
        1.toByte, limit - 1)._1 shouldBe empty

      var reconstructed = 0
      val retainedConflict = txs.head.id
      def previousCandidates: Iterator[(Seq[ErgoTransaction], Seq[ModifierId])] =
        Iterator[() => (Seq[ErgoTransaction], Seq[ModifierId])](
          () => {
            reconstructed += 1
            throw new IllegalStateException("Previous fee reconstruction unavailable")
          },
          () => {
            reconstructed += 1
            prefix -> Seq(retainedConflict)
          }
        ).map(_())

      val unchanged = CandidateGenerator.checkedCandidate(
        Iterator(complete -> Seq(discardedConflict)) ++ previousCandidates, 1.toByte, Int.MaxValue)
      unchanged shouldBe (complete -> Seq(discardedConflict))
      reconstructed shouldBe 0

      val recovered = CandidateGenerator.checkedCandidate(
        Iterator(complete -> Seq(discardedConflict)) ++ previousCandidates, 1.toByte, limit)
      reconstructed shouldBe 2
      recovered._2 shouldBe Seq(retainedConflict)
      BlockTransactions.sizeOf(recovered._1, 1.toByte) shouldBe limit
      BlockTransactions(bytesToId(Array.fill(32)(0.toByte)), 1.toByte, recovered._1).bytes shouldBe
        BlockTransactions(bytesToId(Array.fill(32)(0.toByte)), 1.toByte, prefix).bytes

      reconstructed = 0
      CandidateGenerator.checkedCandidate(previousCandidates, 1.toByte, limit - 1) shouldBe
        (Seq.empty -> Seq.empty)
      reconstructed shouldBe 2
    } finally us.store.close()
  }

  property("collection restores the prior fee prefix when incremental or final writing fails") {
    for (incrementalFailure <- Seq(true, false); withFees <- Seq(false, true)) {
      val bh = boxesHolderGen.sample.get
      val base = createUtxoState(bh, parameters)
      var failSerialization = false
      try {
        val inputs = bh.boxes.values.filter(_.value >= BoxUtils.sufficientAmount(parameters) * 2).take(2).toIndexedSeq
        inputs.size shouldBe 2
        val proposition = if (withFees) feeProp else sigma.ast.ErgoTree.fromSigmaBoolean(sigma.data.TrivialProp.TrueProp)
        val first = validTransactionFromBoxes(IndexedSeq(inputs.head), outputsProposition = proposition)
        val original = validTransactionFromBoxes(IndexedSeq(inputs.last), outputsProposition = proposition)
        val guardedOutputs = new IndexedSeq[ErgoBoxCandidate] {
          override def length: Int = original.outputCandidates.length
          override def apply(index: Int): ErgoBoxCandidate = {
            if (failSerialization) throw new IllegalStateException("Final section writer unavailable")
            original.outputCandidates(index)
          }
        }
        val last = ErgoTransaction(original.inputs, original.dataInputs, guardedOutputs)
        val missing = validErgoTransactionGen.sample.get._2
        missing.inputs.foreach(input => base.boxById(input.boxId) shouldBe empty)
        val state = new UtxoState(base.persistentProver, base.version, base.store, settings) {
          override def withTransactions(txs: Seq[ErgoTransaction]): UtxoState = {
            val withTxs = super.withTransactions(txs)
            val result = new UtxoState(base.persistentProver, base.version, base.store, settings) {
              override def boxById(id: ADKey): Option[org.ergoplatform.ErgoBox] = withTxs.boxById(id)
              override def validateWithCost(tx: ErgoTransaction, context: ErgoStateContext,
                                            costLimit: Int, interpreterOpt: Option[ErgoInterpreter]): Try[Int] = {
                val validated = super.validateWithCost(tx, context, costLimit, interpreterOpt)
                if (incrementalFailure && (tx eq last) && validated.isSuccess) failSerialization = true
                validated
              }
            }
            // Both incremental measurements completed. Fail only the final complete-section serialization.
            if (!incrementalFailure && txs.exists(_ eq last)) failSerialization = true
            result
          }
        }
        val context = base.stateContext.upcoming(defaultMinerPk.value, 1L, settings.chainSettings.initialNBits,
          Array.fill(3)(0.toByte), emptyVSUpdate, 1.toByte)
        val (selected, eliminated) = CandidateGenerator.collectTxs(defaultMinerPk, Int.MaxValue, Int.MaxValue,
          state, context, Seq(first, last, missing))
        failSerialization shouldBe true
        val expectedFee = CandidateGenerator.collectFees(base.stateContext.currentHeight,
          Seq(first), defaultMinerPk, context)
        expectedFee.isDefined shouldBe withFees
        selected shouldBe (Seq(first) ++ expectedFee)
        eliminated shouldBe empty
      } finally base.store.close()
    }
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

  property("stale emission tx is invalidated when its box was spent by concurrently applied block") {
    val us0 = createUtxoState(settings)._1
    us0.emissionBoxOpt should not be None
    val emissionTx =
      CandidateGenerator.collectEmission(us0, defaultMinerPk, emptyStateContext).toSeq.head

    val appliedBlock = validFullBlock(None, us0, Seq(emissionTx))
    val us1          = us0.applyModifier(appliedBlock, None)(_ => ()).get

    val h = appliedBlock.header
    val upcomingContext = us1.stateContext.upcoming(
      h.minerPk,
      h.timestamp,
      h.nBits,
      h.votes,
      emptyVSUpdate,
      h.version
    )

    val (collected, invalid) = CandidateGenerator.collectTxs(
      defaultMinerPk,
      parameters.maxBlockCost,
      parameters.maxBlockSize,
      us1,
      upcomingContext,
      Seq(emissionTx)
    )

    collected shouldBe empty
    invalid shouldBe Seq(emissionTx.id)
  }

  property("mempool transactions spent by applied block are invalidated at next candidate assembly") {
    val bh       = boxesHolderGen.sample.get
    val rnd      = new RandomWrapper
    val us0      = createUtxoState(bh, parameters)
    val minValue = BoxUtils.sufficientAmount(parameters)
    val inputs   = bh.boxes.values.toIndexedSeq.filter(_.value >= minValue * 2).takeRight(10)
    val mempoolTxs =
      inputs.map(i => validTransactionFromBoxes(IndexedSeq(i), rnd, issueNew = false, feeProp))

    val appliedBlock = validFullBlock(None, us0, mempoolTxs)
    val us1          = us0.applyModifier(appliedBlock, None)(_ => ()).get

    val h = appliedBlock.header
    val upcomingContext = us1.stateContext.upcoming(
      h.minerPk,
      h.timestamp,
      h.nBits,
      h.votes,
      emptyVSUpdate,
      h.version
    )

    val (collected, invalid) = CandidateGenerator.collectTxs(
      defaultMinerPk,
      parameters.maxBlockCost,
      parameters.maxBlockSize,
      us1,
      upcomingContext,
      mempoolTxs
    )

    collected shouldBe empty
    invalid should contain theSameElementsAs mempoolTxs.map(_.id)
  }

  property("zero-fee transactions are collected without creating fee transaction") {
    val bh       = boxesHolderGen.sample.get
    val rnd      = new RandomWrapper
    val us       = createUtxoState(bh, parameters)
    val minValue = BoxUtils.sufficientAmount(parameters)
    val inputs   = bh.boxes.values.toIndexedSeq.filter(_.value >= minValue * 2).takeRight(5)
    val zeroFeeTxs = inputs.map(i => validTransactionFromBoxes(IndexedSeq(i), rnd, issueNew = false))
    zeroFeeTxs should not be empty

    val h = validFullBlock(None, us, bh, rnd).header
    val upcomingContext = us.stateContext.upcoming(
      h.minerPk,
      h.timestamp,
      h.nBits,
      h.votes,
      emptyVSUpdate,
      h.version
    )

    val (collected, invalid) = CandidateGenerator.collectTxs(
      defaultMinerPk,
      parameters.maxBlockCost,
      parameters.maxBlockSize,
      us,
      upcomingContext,
      zeroFeeTxs
    )

    invalid shouldBe empty
    collected should contain theSameElementsAs zeroFeeTxs
  }

  property("excludeAppliedTxs filters transactions of the applied best block only") {
    val now = System.currentTimeMillis()
    def utx(t: ErgoTransaction): UnconfirmedTransaction =
      new UnconfirmedTransaction(t, None, now, now, None, None)

    val tx1 = validErgoTransactionGen.sample.get._2
    val tx2 = validErgoTransactionGen.sample.get._2
    val tx3 = validErgoTransactionGen.sample.get._2
    val pool = Seq(utx(tx1), utx(tx2), utx(tx3))
    val appliedId = bytesToId(Array.fill(32)(11.toByte))
    val otherId = bytesToId(Array.fill(32)(22.toByte))

    CandidateGenerator.excludeAppliedTxs(
      pool,
      Some(appliedId -> Set(tx1.id, tx3.id)),
      Some(appliedId)
    ).map(_.id) shouldBe Seq(tx2.id)

    CandidateGenerator.excludeAppliedTxs(
      pool,
      Some(appliedId -> Set(tx1.id, tx3.id)),
      Some(otherId)
    ).map(_.id) shouldBe Seq(tx1.id, tx2.id, tx3.id)

    CandidateGenerator.excludeAppliedTxs(pool, None, Some(appliedId)) shouldBe pool

    CandidateGenerator.excludeAppliedTxs(
      pool,
      Some(appliedId -> Set.empty[ModifierId]),
      Some(appliedId)
    ) shouldBe pool
  }

  property("isChainSynced compares best full block id with state context last header id") {
    CandidateGenerator.isChainSynced(None, emptyStateContext) shouldBe true
    CandidateGenerator.isChainSynced(Some(bytesToId(Array.fill(32)(33.toByte))), emptyStateContext) shouldBe false
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
