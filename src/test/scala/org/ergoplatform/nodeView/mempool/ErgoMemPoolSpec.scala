package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.{ProcessingOutcome, SortingOption}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.{ErgoSettings, ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.utils.{ErgoTestHelpers, RandomWrapper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.util.encode.Base16
import sigma.ast.ByteArrayConstant
import sigma.Colls
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.{ErgoTreeSerializer, SerializerException}
import sigmastate.eval.Extensions._

import scala.collection.immutable.TreeMap
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId

class ErgoMemPoolSpec extends AnyFlatSpec
  with ErgoTestHelpers
  with ScalaCheckPropertyChecks {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  it should "accept valid transaction" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    val txs = validTransactionsFromUtxoState(wus)
    val pool0 = ErgoMemPool.empty(settings)
    val poolAfter = txs.foldLeft(pool0) { case (pool, tx) =>
      val (p, outcome) = pool.process(UnconfirmedTransaction(tx, None), us)
      if (!outcome.isInstanceOf[ProcessingOutcome.Accepted]) {
        throw new Exception("Transaction not accepted")
      }
      p
    }
    poolAfter.spentInputs.size shouldBe txs.flatMap(_.inputs).size

    // light mode
    val poolLight = ErgoMemPool.empty(lightModeSettings)
    txs.foreach { tx =>
      poolLight.process(UnconfirmedTransaction(tx, None), us)._2.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    }
  }

  it should "respect given sorting order" in {
    implicit val ms = settings.chainSettings.monetary
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    val inputBox = wus.takeBoxes(1).head
    val feeOut = new ErgoBoxCandidate(inputBox.value, feeProp, creationHeight = 0)
    val tx = ErgoTransaction(
      IndexedSeq(new Input(inputBox.id, ProverResult.empty)),
      IndexedSeq(feeOut)
    )

    // Randomly initialized
    settings.nodeSettings.mempoolSorting should (be (SortingOption.FeePerByte) or be (SortingOption.FeePerCycle))

    val sortBySizeSettings: ErgoSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        mempoolSorting = SortingOption.FeePerByte,
      ))

    var poolSize = ErgoMemPool.empty(sortBySizeSettings)
    poolSize = poolSize.process(UnconfirmedTransaction(tx, None), wus)._1
    val size = tx.size
    poolSize.pool.orderedTransactions.firstKey.weight shouldBe OrderedTxPool.weighted(tx, size).weight

    val sortByCostSettings: ErgoSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        mempoolSorting = SortingOption.FeePerCycle,
      ))

    var poolCost = ErgoMemPool.empty(sortByCostSettings)
    poolCost = poolCost.process(UnconfirmedTransaction(tx, None), wus)._1
    val validationContext = wus.stateContext.simplifiedUpcoming()
    val cost = wus.validateWithCost(tx, validationContext, Int.MaxValue, None).get
    poolCost.pool.orderedTransactions.firstKey.weight shouldBe OrderedTxPool.weighted(tx, cost).weight
  }

  it should "decline already contained transaction" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    val txs = validTransactionsFromUtxoState(wus)
    var pool = ErgoMemPool.empty(settings)
    txs.foreach { tx =>
      pool = pool.put(UnconfirmedTransaction(tx, None))
    }
    txs.foreach { tx =>
      pool.process(UnconfirmedTransaction(tx, None), us)._2.isInstanceOf[ProcessingOutcome.Declined] shouldBe true
    }
  }

  it should "reject double-spending transaction if it is paying no more than one already sitting in the pool" in {
    forAll(smallPositiveInt, smallPositiveInt) { case (n1, n2) =>
      whenever(n1 != n2) {
        val (us, bh) = createUtxoState(settings)
        val genesis = validFullBlock(None, us, bh)
        val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get

        val feeProp = settings.chainSettings.monetary.feeProposition
        val trueTree = TrueTree
        val inputBox = wus.takeBoxes(100).collectFirst{
          case box if box.ergoTree == trueTree => box
        }.get
        val feeOut = new ErgoBoxCandidate(inputBox.value, feeProp, creationHeight = 0)

        def rndContext(n: Int): ContextExtension = ContextExtension(Map(
          (1: Byte) -> ByteArrayConstant(Array.fill(1 + n)(0: Byte)))
        )

        val tx1Like = ErgoTransaction(
          IndexedSeq(new Input(inputBox.id, new ProverResult(Array.emptyByteArray, rndContext(n1)))),
          IndexedSeq(feeOut)
        )

        val tx2Like = ErgoTransaction(
          IndexedSeq(new Input(inputBox.id, new ProverResult(Array.emptyByteArray, rndContext(n2)))),
          IndexedSeq(feeOut)
        )

        val tx1 = UnconfirmedTransaction(ErgoTransaction(tx1Like.inputs, tx1Like.outputCandidates), None)
        val tx2 = UnconfirmedTransaction(ErgoTransaction(ErgoTransaction(tx2Like.inputs, tx2Like.outputCandidates)), None)

        val pool0 = ErgoMemPool.empty(settings)
        val (pool, tx1Outcome) = pool0.process(tx1, us)

        tx1Outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true

        // tx1 and tx2 are spending the same input, and paying the same fee.
        // So if tx2 is about a bigger or equal size, it should be rejected as it is paying less for a byte.
        // Otherwise, tx2 is paying more for a byte and then it is replacing tx1.
        if (tx2.transaction.size >= tx1.transaction.size) {
          pool.process(tx2, us)._2.isInstanceOf[ProcessingOutcome.DoubleSpendingLoser] shouldBe true
        } else {
          val (updPool, outcome) = pool.process(tx2, us)
          outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
          updPool.size shouldBe 1
          updPool.take(1).head.transaction.id shouldBe tx2.transaction.id
        }
      }
    }
  }

  it should "decline transactions invalidated earlier" in {
    val us = createUtxoState(settings)._1
    var pool = ErgoMemPool.empty(settings)
    forAll(invalidBlockTransactionsGen) { blockTransactions =>
      val unconfirmedTxs = blockTransactions.txs.map(tx => UnconfirmedTransaction(tx, None))
      unconfirmedTxs.foreach(tx => pool = pool.process(tx, us)._1)
      unconfirmedTxs.foreach(tx =>
        pool.process(tx, us)._2.isInstanceOf[ProcessingOutcome.Declined] shouldBe true)
    }
  }

  it should "decline transactions not meeting min fee" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    val txs = validTransactionsFromUtxoState(wus)
    val unconfirmedTxs = txs.map(tx => UnconfirmedTransaction(tx, None))

    val maxSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(minimalFeeAmount = Long.MaxValue))
    val pool = ErgoMemPool.empty(maxSettings)
    unconfirmedTxs.foreach { tx =>
      val (_, outcome) = pool.process(tx, us)
      outcome.isInstanceOf[ProcessingOutcome.Declined] shouldBe true
      outcome.asInstanceOf[ProcessingOutcome.Declined]
        .e.getMessage.contains("Min fee not met") shouldBe true
    }

    val minSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(minimalFeeAmount = 0))
    val pool2 = ErgoMemPool.empty(minSettings)
    unconfirmedTxs.foreach { tx =>
      val (_, outcome) = pool2.process(tx, us)
      outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    }
  }

  it should "invalidate or reject invalid transaction" in {
    val us = createUtxoState(settings)._1
    val pool = ErgoMemPool.empty(settings)
    forAll(invalidBlockTransactionsGen) { blockTransactions =>
      blockTransactions.txs.forall{tx =>
        val valRes = pool.process(UnconfirmedTransaction(tx, None), us)._2
        valRes.isInstanceOf[ProcessingOutcome.Invalidated] ||
          valRes.isInstanceOf[ProcessingOutcome.Declined]} shouldBe true
    }
  }

  it should "accept only unique transactions" in {
    val pool = ErgoMemPool.empty(settings)
    val tx = UnconfirmedTransaction(invalidErgoTransactionGen.sample.get, None)
    pool.put(Seq(tx, tx, tx)).size shouldBe 1
  }

  it should "drop less prioritized transaction in case of pool overflow" in {
    val limitedPoolSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(mempoolCapacity = 4))
    var pool = ErgoMemPool.empty(limitedPoolSettings)
    val masterTx = invalidErgoTransactionGen.sample.get
    val proposition = settings.chainSettings.monetary.feeProposition
    val txsWithAscendingPriority = (0 to 4).foldLeft(Seq.empty[ErgoTransaction]) { case (acc, idx) =>
      val c = masterTx.outputCandidates.head
      acc :+ masterTx.copy(outputCandidates = IndexedSeq(
        new ErgoBoxCandidate(idx * 10000 + 1, proposition, c.creationHeight, c.additionalTokens, c.additionalRegisters)))
    }
    val lessPrioritizedTxs = txsWithAscendingPriority.init.map(tx => UnconfirmedTransaction(tx, None))
    val mostPrioritizedTx = UnconfirmedTransaction(txsWithAscendingPriority.last, None)
    pool = pool.put(lessPrioritizedTxs)

    pool.size shouldBe 4
    pool.getAll should contain only (lessPrioritizedTxs: _*)
    pool = pool.put(Seq(mostPrioritizedTx))
    pool.size shouldBe 4
    pool.getAll should contain only (mostPrioritizedTx +: lessPrioritizedTxs.tail: _*)
  }

  it should "Accept output of pooled transactions" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    val txs = validTransactionsFromUtxoState(wus).map(tx => UnconfirmedTransaction(tx, None))
    var pool = ErgoMemPool.empty(settings)
    txs.foreach { tx =>
      pool = pool.put(tx)
    }
    txs.foreach { tx =>
      val spendingBox = tx.transaction.outputs.head
      val unconfirmedTransaction = UnconfirmedTransaction(tx.transaction.copy(
        inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
        outputCandidates = IndexedSeq(spendingBox)), None)
      val (newPool, outcome) = pool.process(unconfirmedTransaction, us)
      outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
      pool = newPool
    }
  }

  // Regression test for https://github.com/ergoplatform/ergo/issues/1448
  it should "accept chained unconfirmed transactions with minted tokens (issue #1448)" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get

    // Pick a spendable box from the UTXO state (anyoneCanSpend / TrueTree)
    val inputBox = wus.takeBoxes(100).find(_.ergoTree == TrueTree).get

    val feeProp = settings.chainSettings.monetary.feeProposition

    // TX1: Mint a new token. Per Ergo rules, the token ID must equal the first input's boxId.
    val mintedTokenId = inputBox.id
    val mintedTokenAmount = 1000L
    val tx1ValueOut = inputBox.value - 100000L // leave room for fee
    val tx1FeeOut = new ErgoBoxCandidate(100000L, feeProp, creationHeight = 0)
    val tx1TokenOut = new ErgoBoxCandidate(
      tx1ValueOut,
      TrueTree,
      creationHeight = 0,
      additionalTokens = Colls.fromItems((mintedTokenId.toTokenId, mintedTokenAmount))
    )
    val tx1 = ErgoTransaction(
      IndexedSeq(new Input(inputBox.id, emptyProverResult)),
      IndexedSeq(tx1TokenOut, tx1FeeOut)
    )

    // Verify TX1 is stateless-valid (correct asset minting)
    tx1.statelessValidity().isSuccess shouldBe true

    // Process TX1 through the mempool — should be accepted
    val pool0 = ErgoMemPool.empty(settings)
    val (pool1, tx1Outcome) = pool0.process(UnconfirmedTransaction(tx1, None), wus)
    tx1Outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    pool1.modifierById(tx1.id) shouldBe defined

    // TX2: Spend the token-carrying output of TX1 (which is still unconfirmed).
    // This is the exact scenario from issue #1448: the token was minted in TX1 and
    // TX2 tries to transfer it while TX1 is only in the mempool.
    // Before the fix, verifyAssets() would return inAmount = -1 because the
    // augmented UTXO view did not correctly surface tokens from unconfirmed outputs.
    val tx1Output = tx1.outputs.head // the box carrying the minted token
    val tx2ValueOut = tx1Output.value - 100000L
    val tx2FeeOut = new ErgoBoxCandidate(100000L, feeProp, creationHeight = 0)
    val tx2TokenOut = new ErgoBoxCandidate(
      tx2ValueOut,
      TrueTree,
      creationHeight = 0,
      additionalTokens = Colls.fromItems((mintedTokenId.toTokenId, mintedTokenAmount))
    )
    val tx2 = ErgoTransaction(
      IndexedSeq(new Input(tx1Output.id, emptyProverResult)),
      IndexedSeq(tx2TokenOut, tx2FeeOut)
    )

    tx2.statelessValidity().isSuccess shouldBe true

    // Process TX2 through the mempool — historically failed with:
    // "For every token, its amount in outputs should not exceed its amount in inputs. Amount in = -1, out = 1000"
    val (pool2, tx2Outcome) = pool1.process(UnconfirmedTransaction(tx2, None), wus)
    tx2Outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    pool2.modifierById(tx2.id) shouldBe defined
  }

  it should "consider families for replacement policy" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    var txs = validTransactionsFromUtxoState(wus).map(tx => UnconfirmedTransaction(tx, None))
    val family_depth = 10
    val limitedPoolSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(mempoolCapacity = (family_depth + 1) * txs.size))
    var pool = ErgoMemPool.empty(limitedPoolSettings)
    txs.foreach { tx =>
      pool = pool.put(tx)
    }
    for (_ <- 1 to family_depth) {
      txs = txs.map(tx => {
        val spendingBox = tx.transaction.outputs.head
        val newTx = UnconfirmedTransaction(tx.transaction.copy(inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
          outputCandidates = IndexedSeq(spendingBox)), None)
        val (newPool, outcome) = pool.process(newTx, us)
        outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
        pool = newPool
        newTx
      })
    }
    pool.size shouldBe (family_depth + 1) * txs.size
    txs.foreach { utx =>
      val tx = utx.transaction
      val sb = tx.outputs.head
      val txToDecline = tx.copy(inputs = IndexedSeq(new Input(sb.id, emptyProverResult)),
        outputCandidates = IndexedSeq(new ErgoBoxCandidate(sb.value, sb.ergoTree, sb.creationHeight, sb.additionalTokens, sb.additionalRegisters)))
      val res = pool.process(UnconfirmedTransaction(txToDecline, None), us)._2
      res.isInstanceOf[ProcessingOutcome.Declined] shouldBe true
      res.asInstanceOf[ProcessingOutcome.Declined].e.getMessage.contains("pays less") shouldBe true
      pool.size shouldBe (family_depth + 1) * txs.size
    }
  }

  it should "correctly remove transaction from pool and rebuild families" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    var txs = validTransactionsFromUtxoState(wus).map(tx => UnconfirmedTransaction(tx, None))
    var allTxs = txs
    val family_depth = 10
    val limitedPoolSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(mempoolCapacity = (family_depth + 1) * txs.size))
    var pool = ErgoMemPool.empty(limitedPoolSettings)
    txs.foreach { tx =>
      pool = pool.put(tx)
    }
    for (_ <- 1 to family_depth) {
      txs = txs.map(tx => {
        val spendingBox = tx.transaction.outputs.head
        val newTx = UnconfirmedTransaction(tx.transaction.copy(inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
          outputCandidates = IndexedSeq(spendingBox)), None)
        val (newPool, outcome) = pool.process(newTx, us)
        outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
        pool = newPool
        allTxs = allTxs :+ newTx
        newTx
      })
    }
    pool.size shouldBe (family_depth + 1) * txs.size
    allTxs.foreach { tx =>
      pool = pool.removeTxAndDoubleSpends(tx.transaction)
    }
    pool.size shouldBe 0
  }

  it should "correctly remove doublespents of a transaction from pool" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    val boxes = wus.takeBoxes(4)

    val limit = 10000

    val tx1 = validTransactionsFromBoxes(limit, boxes.take(1), new RandomWrapper)
                ._1.map(tx => UnconfirmedTransaction(tx, None)).head

    val tx2 = validTransactionsFromBoxes(limit, boxes.takeRight(2), new RandomWrapper)
      ._1.map(tx => UnconfirmedTransaction(tx, None)).head

    val tx3 = validTransactionsFromBoxes(limit, boxes.take(1), new RandomWrapper)
      ._1.map(tx => UnconfirmedTransaction(tx, None)).head

    tx1.transaction.inputs.head.boxId shouldBe tx3.transaction.inputs.head.boxId

    var pool = ErgoMemPool.empty(settings)
    Seq(tx2, tx3).foreach { tx =>
      pool = pool.put(tx)
    }

    pool = pool.removeTxAndDoubleSpends(tx1.transaction)
    pool.contains(tx1.transaction) shouldBe false
    pool.contains(tx2.transaction) shouldBe true
    pool.contains(tx3.transaction) shouldBe false
  }

  it should "return results take / getAll / getAllPrioritized sorted by priority" in {
    val feeProp = settings.chainSettings.monetary.feeProposition

    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    var txs = validTransactionsFromUtxoState(wus).map(tx => UnconfirmedTransaction(tx, None))
    val family_depth = 10
    val limitedPoolSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(mempoolCapacity = (family_depth + 1) * txs.size))
    var pool = ErgoMemPool.empty(limitedPoolSettings)
    txs.foreach { tx =>
      pool = pool.put(tx)
    }
    for (_ <- 1 to family_depth) {
      txs = txs.map(tx => {
        val spendingBox = tx.transaction.outputs.head

        val sc = spendingBox.toCandidate
        val out0 = new ErgoBoxCandidate(sc.value - 55000, sc.ergoTree, sc.creationHeight)
        val out1 = new ErgoBoxCandidate(55000, feeProp, sc.creationHeight)

        val newTx = UnconfirmedTransaction(tx.transaction.copy(inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
          outputCandidates = IndexedSeq(out0, out1)), None)
        val (newPool, outcome) = pool.process(newTx, us)
        outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
        pool = newPool
        newTx
      })
    }

    val weights = pool.weightedTransactionIds(11)
    val ids = weights.map(_.id)

    pool.take(11).toSeq.map(_.transaction.id) shouldBe ids
    pool.getAll.map(_.transaction.id) shouldBe ids
    pool.getAllPrioritized.map(_.transaction.id) shouldBe ids

    val conformingTxs = pool.take(3).toSeq
    val stateWithTxs = wus.withUnconfirmedTransactions(conformingTxs)

    conformingTxs.map(_.transaction).flatMap(_.inputs).map(_.boxId).forall(bIb => stateWithTxs.boxById(bIb)
      .isDefined) shouldBe true
  }

  it should "add removed transaction to mempool statistics" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    var txs = validTransactionsFromUtxoState(wus).map(tx => UnconfirmedTransaction(tx, None))
    var allTxs = txs
    val family_depth = 10
    val limitedPoolSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(mempoolCapacity = (family_depth + 1) * txs.size))
    var pool = ErgoMemPool.empty(limitedPoolSettings)
    txs.foreach { tx =>
      pool = pool.put(tx)
    }
    for (_ <- 1 to family_depth) {
      txs = txs.map(tx => {
        val spendingBox = tx.transaction.outputs.head
        val newTx = UnconfirmedTransaction(tx.transaction.copy(inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
          outputCandidates = IndexedSeq(spendingBox)), None)
        val (newPool, outcome) = pool.process(newTx, us)
        outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
        pool = newPool
        allTxs = allTxs :+ newTx
        newTx
      })
    }
    pool.size shouldBe (family_depth + 1) * txs.size
    pool.stats.histogram shouldBe MemPoolStatistics(System.currentTimeMillis(),0,System.currentTimeMillis()).histogram
    pool.stats.takenTxns shouldBe MemPoolStatistics(System.currentTimeMillis(),0,System.currentTimeMillis()).takenTxns
    pool.stats.snapTakenTxns shouldBe MemPoolStatistics(System.currentTimeMillis(),0,System.currentTimeMillis()).snapTakenTxns

    allTxs.foreach { tx =>
      pool = pool.removeTxAndDoubleSpends(tx.transaction)
    }
    pool.size shouldBe 0
    pool.stats.takenTxns shouldBe (family_depth + 1) * txs.size
  }

  it should "put not adding transaction twice" in {
    val pool = ErgoMemPool.empty(settings).pool
    val tx = invalidErgoTransactionGen.sample.get
    val now = System.currentTimeMillis()

    val utx1 = new UnconfirmedTransaction(tx, None, now, now, None, None)
    val utx2 = new UnconfirmedTransaction(tx, None, now, now, None, None)
    val utx3 = new UnconfirmedTransaction(tx, None, now + 1, now + 1, None, None)
    val updPool = pool.put(utx1, 100).remove(utx1).put(utx2, 500).put(utx3, 5000)
    updPool.size shouldBe 1
    updPool.get(utx3.id).get.lastCheckedTime shouldBe (now + 1)
  }

  it should "reject v7 tree spending" in {
    val parameters = new Parameters(height = 0,
      Parameters.DefaultParameters.updated(Parameters.BlockVersion, Header.Interpreter60Version),
      proposedUpdate = ErgoValidationSettingsUpdate.empty)
    val (us, bh) = createUtxoState(initSettings, Some(parameters))
    val wus = WrappedUtxoState(us, bh, settings)
    val txs = validTransactionsFromUtxoState(wus).map(tx => UnconfirmedTransaction(tx, None))
    var pool = ErgoMemPool.empty(settings)
    val tx = txs.head
    pool = pool.put(tx)

    // v7 tree w. sigmaProp(true)
    val bs = "1f06010101d17300"
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(bs).get)

    val spendingBox = tx.transaction.outputs.head
    val o2 = new ErgoBoxCandidate(spendingBox.value, tree, spendingBox.creationHeight, spendingBox.additionalTokens, spendingBox.additionalRegisters)
    val tx2 = UnconfirmedTransaction(tx.transaction.copy(
      inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
      outputCandidates = IndexedSeq(o2)), None)
    val (_, outcome) = pool.process(tx2, us)
    outcome.isInstanceOf[ProcessingOutcome.Invalidated] shouldBe true

    // sigma.serialization.SerializerException: Tree version (7) is above activated script version (3)
    outcome.asInstanceOf[ProcessingOutcome.Invalidated].e.isInstanceOf[SerializerException] shouldBe true
  }

  it should "accept v6 tree" in {
    val parameters = new Parameters(height = 0,
      Parameters.DefaultParameters.updated(Parameters.BlockVersion, Header.Interpreter60Version),
      proposedUpdate = ErgoValidationSettingsUpdate.empty)
    val (us, bh) = createUtxoState(initSettings, Some(parameters))

    val wus = WrappedUtxoState(us, bh, settings)
    val txs = validTransactionsFromUtxoState(wus).map(tx => UnconfirmedTransaction(tx, None))
    var pool = ErgoMemPool.empty(settings)
    val tx = txs.head
    pool = pool.put(tx)

    // sigmaProp(Global.serialize(2).size > 0)
    val bs = "1b110204040400d191b1dc6a03dd0173007301"
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(bs).get)

    val spendingBox = tx.transaction.outputs.head
    val o2 = new ErgoBoxCandidate(spendingBox.value, tree, spendingBox.creationHeight, spendingBox.additionalTokens, spendingBox.additionalRegisters)
    val tx2 = UnconfirmedTransaction(tx.transaction.copy(
      inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
      outputCandidates = IndexedSeq(o2)), None)
    val (newPool, outcome) = pool.process(tx2, wus)
    outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    pool = newPool

    val spendingBox2 = tx2.transaction.outputs.head
    val o3 = new ErgoBoxCandidate(spendingBox2.value, tree, spendingBox2.creationHeight, spendingBox2.additionalTokens, spendingBox2.additionalRegisters)
    val tx3 = UnconfirmedTransaction(tx2.transaction.copy(
      inputs = IndexedSeq(new Input(spendingBox2.id, emptyProverResult)),
      outputCandidates = IndexedSeq(o3)), None)
    val (_, outcome2) = pool.process(tx3, wus)
    outcome2.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
  }


  it should "reject v6 tree reducing to false" in {
    val parameters = new Parameters(height = 0,
      Parameters.DefaultParameters.updated(Parameters.BlockVersion, Header.Interpreter60Version),
      proposedUpdate = ErgoValidationSettingsUpdate.empty)
    val (us, bh) = createUtxoState(initSettings, Some(parameters))

    val wus = WrappedUtxoState(us, bh, settings)
    val txs = validTransactionsFromUtxoState(wus).map(tx => UnconfirmedTransaction(tx, None))
    var pool = ErgoMemPool.empty(settings)
    val tx = txs.head
    pool = pool.put(tx)

    // sigmaProp(Global.serialize(2).size <= 0)
    val bs = "1b110204040400d190b1dc6a03dd0173007301"
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(bs).get)

    val spendingBox = tx.transaction.outputs.head
    val o2 = new ErgoBoxCandidate(spendingBox.value, tree, spendingBox.creationHeight, spendingBox.additionalTokens, spendingBox.additionalRegisters)
    val tx2 = UnconfirmedTransaction(tx.transaction.copy(
      inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
      outputCandidates = IndexedSeq(o2)), None)
    val (newPool, outcome) = pool.process(tx2, wus)
    outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    pool = newPool

    val spendingBox2 = tx2.transaction.outputs.head
    val o3 = new ErgoBoxCandidate(spendingBox2.value, tree, spendingBox2.creationHeight, spendingBox2.additionalTokens, spendingBox2.additionalRegisters)
    val tx3 = UnconfirmedTransaction(tx2.transaction.copy(
      inputs = IndexedSeq(new Input(spendingBox2.id, emptyProverResult)),
      outputCandidates = IndexedSeq(o3)), None)
    val (_, outcome2) = pool.process(tx3, wus)
    outcome2.isInstanceOf[ProcessingOutcome.Invalidated] shouldBe true
  }

<<<<<<< HEAD

  it should "return minimal fee from getRecommendedFee when no statistics is collected" in {
    val pool = ErgoMemPool.empty(settings)
    pool.getRecommendedFee(5, 1024) shouldBe settings.nodeSettings.minimalFeeAmount
  }

  it should "base getRecommendedFee on the fee histogram when statistics is available" in {
    val now = System.currentTimeMillis()
    // 4 transactions paying on average 2000000 nanoErg/Kb were taken within 2..3 minutes
    val histogram = MemPoolStatistics.defaultPoolHistogram.updated(2, FeeHistogramBin(4, 8000000))
    val stats = MemPoolStatistics(startMeasurement = now - 100000, takenTxns = 4,
      snapTime = now - 100000, snapTakenTxns = 0, histogram = histogram)
    val empty = ErgoMemPool.empty(settings)
    val pool = new ErgoMemPool(empty.pool, stats, empty.sortingOption)

    // for a 1Kb transaction the recommended fee is the average fee per Kb of the first non-empty bin
    pool.getRecommendedFee(5, 1024) shouldBe 2000000
  }

  it should "make getExpectedWaitTime consistent with getRecommendedFee" in {
    val now = System.currentTimeMillis()
    val histogram = MemPoolStatistics.defaultPoolHistogram.updated(2, FeeHistogramBin(4, 8000000))
    val stats = MemPoolStatistics(startMeasurement = now - 100000, takenTxns = 4,
      snapTime = now - 100000, snapTakenTxns = 0, histogram = histogram)
    val empty = ErgoMemPool.empty(settings)
    val pool = new ErgoMemPool(empty.pool, stats, empty.sortingOption)

    val txSize = 1024
    val expectedWaitTimeMinutes = 5
    val recommendedFee = pool.getRecommendedFee(expectedWaitTimeMinutes, txSize)
    val waitTimeMs = pool.getExpectedWaitTime(recommendedFee, txSize)

    // a transaction paying the recommended fee is expected to be taken
    // within the wait time the fee was recommended for
    waitTimeMs shouldBe 2 * 60 * 1000
    waitTimeMs should be <= expectedWaitTimeMinutes.toLong * 60 * 1000
  }

  it should "return bounded getExpectedWaitTime when no transactions are taken from the pool for a long time" in {
    // no histogram data and a long period of inactivity (e.g. transactions stuck in the pool)
    val longAgo = System.currentTimeMillis() - 365L * 24 * 60 * 60 * 1000
    val stats = MemPoolStatistics(startMeasurement = longAgo, takenTxns = 1,
      snapTime = longAgo, snapTakenTxns = 0)
    val empty = ErgoMemPool.empty(settings)
    var pool = new ErgoMemPool(empty.pool, stats, empty.sortingOption)

    // fill the pool with some transactions, all with a priority higher than the queried one
    (1 to 5).foreach { _ =>
      pool = pool.put(UnconfirmedTransaction(invalidErgoTransactionGen.sample.get, None))
    }
    val posInPool = pool.size

    // the estimate is bounded by the statistics measurement window:
    // at most 2 * measurementIntervalMsec per pool position (with takenTxns = 1)
    val waitTimeMs = pool.getExpectedWaitTime(0, 1024)
    waitTimeMs should be <= 2L * MemPoolStatistics.measurementIntervalMsec * posInPool
=======
  it should "return random transactions" in {
    val txs = (1 to 10).map(_ => invalidErgoTransactionGen.sample.get)
      .map(tx => UnconfirmedTransaction(tx, None))
    var pool = ErgoMemPool.empty(settings)
    txs.foreach { tx =>
      pool = pool.put(tx)
    }
    pool.size shouldBe 10

    // Request fewer transactions than present
    val random3 = pool.random(3)
    random3.size shouldBe 3
    random3.foreach { tx =>
      pool.contains(tx.transaction.id) shouldBe true
    }

    // Request more than present — should return all
    val random20 = pool.random(20)
    random20.size shouldBe 10
    random20.map(_.transaction.id).toSet shouldBe txs.map(_.transaction.id).toSet
  }

  it should "track invalidated transactions" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    val txs = validTransactionsFromUtxoState(wus).map(tx => UnconfirmedTransaction(tx, None))
    var pool = ErgoMemPool.empty(settings)
    txs.foreach { tx =>
      pool = pool.put(tx)
    }

    val tx = txs.head
    pool.isInvalidated(tx.transaction.id) shouldBe false
    pool = pool.invalidate(tx)
    pool.isInvalidated(tx.transaction.id) shouldBe true
    pool.contains(tx.transaction.id) shouldBe false

    pool.isInvalidated(scorex.util.ModifierId @@ "nonexistent") shouldBe false
  }

  it should "reject blacklisted transactions" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get
    val txs = validTransactionsFromUtxoState(wus).map(tx => UnconfirmedTransaction(tx, None))
    val tx = txs.head

    val blacklistedSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        blacklistedTransactions = Seq(tx.transaction.id)
      )
    )
    val pool = ErgoMemPool.empty(blacklistedSettings)
    val (_, outcome) = pool.process(tx, wus)
    outcome.isInstanceOf[ProcessingOutcome.Invalidated] shouldBe true
    outcome.asInstanceOf[ProcessingOutcome.Invalidated]
      .e.getMessage.contains("blacklisted tx") shouldBe true
  }

  it should "decline transaction with missing UTXOs" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get

    val feeProp = settings.chainSettings.monetary.feeProposition
    val inputBox = wus.takeBoxes(100).find(_.ergoTree == TrueTree).get
    val feeOut = new ErgoBoxCandidate(inputBox.value, feeProp, creationHeight = 0)

    // Create a fake box ID that does not exist in the state or mempool
    val fakeBoxId: org.ergoplatform.ErgoBox.BoxId =
      scorex.crypto.authds.ADKey @@ scorex.util.Random.randomBytes(32)
    val tx = ErgoTransaction(
      IndexedSeq(new Input(fakeBoxId, emptyProverResult)),
      IndexedSeq(feeOut)
    )

    val pool = ErgoMemPool.empty(settings)
    val (_, outcome) = pool.process(UnconfirmedTransaction(tx, None), wus)
    outcome.isInstanceOf[ProcessingOutcome.Declined] shouldBe true
    outcome.asInstanceOf[ProcessingOutcome.Declined]
      .e.getMessage.contains("not all utxos in place yet") shouldBe true
  }

  it should "replace multiple double-spending transactions when new one pays more" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get

    // Use fee-per-byte sorting so weight comparison is deterministic and fair
    val byteSortSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(mempoolSorting = SortingOption.FeePerByte)
    )

    val feeProp = settings.chainSettings.monetary.feeProposition
    val trueTree = TrueTree

    // Pick two anyone-can-spend boxes
    val trueBoxes = wus.takeBoxes(100).filter(_.ergoTree == trueTree).take(2)
    trueBoxes.size shouldBe 2
    val boxA = trueBoxes.head
    val boxB = trueBoxes(1)

    // TX1 spends BoxA with low fee (small value kept, rest as fee)
    val tx1FeeOut = new ErgoBoxCandidate(boxA.value - 100000L, feeProp, creationHeight = 0)
    val tx1ChangeOut = new ErgoBoxCandidate(100000L, trueTree, creationHeight = 0)
    val tx1 = UnconfirmedTransaction(ErgoTransaction(
      IndexedSeq(new Input(boxA.id, emptyProverResult)),
      IndexedSeq(tx1FeeOut, tx1ChangeOut)
    ), None)

    // TX2 spends BoxB with low fee
    val tx2FeeOut = new ErgoBoxCandidate(boxB.value - 100000L, feeProp, creationHeight = 0)
    val tx2ChangeOut = new ErgoBoxCandidate(100000L, trueTree, creationHeight = 0)
    val tx2 = UnconfirmedTransaction(ErgoTransaction(
      IndexedSeq(new Input(boxB.id, emptyProverResult)),
      IndexedSeq(tx2FeeOut, tx2ChangeOut)
    ), None)

    // TX3 spends both BoxA and BoxB with very high fee (almost all value)
    val tx3FeeOut = new ErgoBoxCandidate(boxA.value + boxB.value - 100000L, feeProp, creationHeight = 0)
    val tx3ChangeOut = new ErgoBoxCandidate(100000L, trueTree, creationHeight = 0)
    val tx3 = UnconfirmedTransaction(ErgoTransaction(
      IndexedSeq(
        new Input(boxA.id, emptyProverResult),
        new Input(boxB.id, emptyProverResult)
      ),
      IndexedSeq(tx3FeeOut, tx3ChangeOut)
    ), None)

    tx3.transaction.statelessValidity().isSuccess shouldBe true

    var pool = ErgoMemPool.empty(byteSortSettings)
    val (pool1, outcome1) = pool.process(tx1, wus)
    outcome1.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    val (pool2, outcome2) = pool1.process(tx2, wus)
    outcome2.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    pool = pool2
    pool.size shouldBe 2

    // TX3 pays more fee per byte than TX1 and TX2 on average, so it should replace them
    val (poolAfter, outcome) = pool.process(tx3, wus)
    outcome.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    poolAfter.size shouldBe 1
    poolAfter.contains(tx3.transaction.id) shouldBe true
    poolAfter.contains(tx1.transaction.id) shouldBe false
    poolAfter.contains(tx2.transaction.id) shouldBe false

    // After TX3 replaces TX1+TX2, a lower-fee double-spender should be detected and rejected
    val tx4FeeOut = new ErgoBoxCandidate(boxA.value - 200000L, feeProp, creationHeight = 0)
    val tx4ChangeOut = new ErgoBoxCandidate(boxB.value + 200000L, trueTree, creationHeight = 0)
    val tx4 = UnconfirmedTransaction(ErgoTransaction(
      IndexedSeq(
        new Input(boxA.id, emptyProverResult),
        new Input(boxB.id, emptyProverResult)
      ),
      IndexedSeq(tx4FeeOut, tx4ChangeOut)
    ), None)

    tx4.transaction.statelessValidity().isSuccess shouldBe true

    val (poolFinal, outcome4) = poolAfter.process(tx4, wus)
    outcome4.isInstanceOf[ProcessingOutcome.DoubleSpendingLoser] shouldBe true
    poolFinal.size shouldBe 1
    poolFinal.contains(tx3.transaction.id) shouldBe true
  }

  it should "not produce duplicate ids when stale registry entry prevents proper removal" in {
    // TreeMap requires an Ordering for WeightedTxId keys
    implicit val wtxOrdering: Ordering[WeightedTxId] = Ordering.by(wtx => (-wtx.weight, wtx.id))

    val tx = invalidErgoTransactionGen.sample.get
    val now = System.currentTimeMillis()
    val utx = UnconfirmedTransaction(tx, None)

    // Create two WeightedTxIds for the same transaction with different weights.
    // WeightedTxId.equals uses only 'id', but Ordering[WeightedTxId] compares (-weight, id).
    // Therefore TreeMap treats them as distinct keys, allowing the same transaction
    // to exist under multiple keys.
    val wtxStale = WeightedTxId(tx.id, 100, 100, now)
    val wtxActual = WeightedTxId(tx.id, 200, 200, now)

    // Verify the structural vulnerability
    wtxStale shouldBe wtxActual
    wtxOrdering.compare(wtxStale, wtxActual) should not be 0

    // Simulate an out-of-sync state: registry points to wtxStale,
    // but orderedTransactions stores the tx under wtxActual.
    // This can happen after updateFamily or other weight changes
    // fail to keep the two collections in sync.
    val emptyPool = OrderedTxPool.empty(settings)
    val brokenPool = new OrderedTxPool(
      TreeMap(wtxActual -> utx),
      TreeMap(tx.id -> wtxStale),
      emptyPool.invalidatedTxIds,
      emptyPool.outputs,
      emptyPool.inputs
    )(settings)

    // pool.get traverses registry -> wtxStale -> orderedTransactions,
    // but wtxStale is not a key in orderedTransactions, so get returns None.
    brokenPool.get(tx.id) shouldBe None

    // Yet the transaction IS present under wtxActual
    brokenPool.orderedTransactions.valuesIterator.toSeq.map(_.id) should contain(tx.id)

    val mempool = new ErgoMemPool(
      brokenPool,
      MemPoolStatistics(now, 0, now, 0),
      SortingOption.FeePerByte
    )(settings)

    // invalidate() first tries pool.get (returns None), then falls back to
    // scanning orderedTransactions.valuesIterator. It finds the tx and calls
    // OrderedTxPool.invalidate(utx). Inside that method,
    // transactionsRegistry.get(tx.id) returns Some(wtxStale).
    // With the fix, the stale entry is detected (wtxStale not in orderedTransactions)
    // and the fallback path filters orderedTransactions by transaction id.
    val afterInvalidate = mempool.invalidate(tx.id)

    // Transaction is properly removed from orderedTransactions despite stale registry
    afterInvalidate.pool.orderedTransactions.valuesIterator.toSeq.map(_.id) should not contain(tx.id)
    afterInvalidate.pool.transactionsRegistry.contains(tx.id) shouldBe false

    // Now put the same transaction again. With no registry entry, put()
    // creates a NEW WeightedTxId based on the actual feeFactor.
    val afterPut = afterInvalidate.put(utx)

    // Only ONE entry for the transaction ID exists
    afterPut.pool.orderedTransactions.valuesIterator.toSeq.count(_.id == tx.id) shouldBe 1

    // getAll (used by /transactions/unconfirmed/transactionIds) returns no duplicates
    val all = afterPut.getAll
    all.count(_.id == tx.id) shouldBe 1
    all.map(_.id).distinct.size shouldBe 1
  }

  it should "detect double spend after replace-by-fee replacement" in {
    val (us, bh) = createUtxoState(settings)
    val genesis = validFullBlock(None, us, bh)
    val wus = WrappedUtxoState(us, bh, settings).applyModifier(genesis)(_ => ()).get

    val feeProp = settings.chainSettings.monetary.feeProposition
    val trueTree = TrueTree

    // Pick two anyone-can-spend boxes
    val trueBoxes = wus.takeBoxes(100).filter(_.ergoTree == trueTree).take(2)
    trueBoxes.size shouldBe 2
    val boxA = trueBoxes.head
    val boxB = trueBoxes(1)

    // TX1 spends BoxA with moderate fee
    val tx1FeeOut = new ErgoBoxCandidate(boxA.value - 200000L, feeProp, creationHeight = 0)
    val tx1ChangeOut = new ErgoBoxCandidate(200000L, trueTree, creationHeight = 0)
    val tx1 = UnconfirmedTransaction(ErgoTransaction(
      IndexedSeq(new Input(boxA.id, emptyProverResult)),
      IndexedSeq(tx1FeeOut, tx1ChangeOut)
    ), None)

    // TX2 spends BoxB with moderate fee
    val tx2FeeOut = new ErgoBoxCandidate(boxB.value - 200000L, feeProp, creationHeight = 0)
    val tx2ChangeOut = new ErgoBoxCandidate(200000L, trueTree, creationHeight = 0)
    val tx2 = UnconfirmedTransaction(ErgoTransaction(
      IndexedSeq(new Input(boxB.id, emptyProverResult)),
      IndexedSeq(tx2FeeOut, tx2ChangeOut)
    ), None)

    // TX3 spends both BoxA and BoxB with very high fee, replacing TX1 and TX2
    val tx3FeeOut = new ErgoBoxCandidate(boxA.value + boxB.value - 200000L, feeProp, creationHeight = 0)
    val tx3ChangeOut = new ErgoBoxCandidate(200000L, trueTree, creationHeight = 0)
    val tx3 = UnconfirmedTransaction(ErgoTransaction(
      IndexedSeq(
        new Input(boxA.id, emptyProverResult),
        new Input(boxB.id, emptyProverResult)
      ),
      IndexedSeq(tx3FeeOut, tx3ChangeOut)
    ), None)

    // TX4 spends both BoxA and BoxB with tiny fee — should lose to TX3
    val tx4FeeOut = new ErgoBoxCandidate(200000L, feeProp, creationHeight = 0)
    val tx4ChangeOut = new ErgoBoxCandidate(boxA.value + boxB.value - 200000L, trueTree, creationHeight = 0)
    val tx4 = UnconfirmedTransaction(ErgoTransaction(
      IndexedSeq(
        new Input(boxA.id, emptyProverResult),
        new Input(boxB.id, emptyProverResult)
      ),
      IndexedSeq(tx4FeeOut, tx4ChangeOut)
    ), None)

    var pool = ErgoMemPool.empty(settings)
    val (pool1, outcome1) = pool.process(tx1, wus)
    outcome1.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    val (pool2, outcome2) = pool1.process(tx2, wus)
    outcome2.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    pool = pool2
    pool.size shouldBe 2

    // TX3 replaces TX1 and TX2
    val (poolAfter, outcome3) = pool.process(tx3, wus)
    outcome3.isInstanceOf[ProcessingOutcome.Accepted] shouldBe true
    poolAfter.size shouldBe 1
    poolAfter.contains(tx3.transaction.id) shouldBe true

    // After replacement, TX4 (lower fee) should be rejected as double-spending loser
    val (_, outcome4) = poolAfter.process(tx4, wus)
    outcome4.isInstanceOf[ProcessingOutcome.DoubleSpendingLoser] shouldBe true
>>>>>>> origin/master
  }

}
