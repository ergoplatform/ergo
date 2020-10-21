package org.ergoplatform.nodeView.mempool

import org.ergoplatform.{ErgoBoxCandidate, Input, UnsignedInput}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPool.ProcessingOutcome
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.utils.generators.ErgoGenerators
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import sigmastate.Values.ByteArrayConstant
import sigmastate.interpreter.ContextExtension

import scala.util.Random

class ErgoMemPoolSpec extends FlatSpec
  with ErgoGenerators
  with ErgoTestHelpers
  with PropertyChecks {

  it should "accept valid transaction" in {
    val (us, bh) = createUtxoState()
    val genesis = validFullBlock(None, us, bh, Random)
    val wus = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    val txs = validTransactionsFromUtxoState(wus, Random)
    val pool0 = ErgoMemPool.empty(settings)
    val poolAfter = txs.foldLeft(pool0) { case (pool, tx) =>
      val (p, outcome) = pool.process(tx, us)
      if (outcome != ProcessingOutcome.Accepted) {
        throw new Exception("Transaction not accepted")
      }
      p
    }
    poolAfter.pool.inputs.size shouldBe txs.flatMap(_.inputs).size

    // light mode
    val poolLight = ErgoMemPool.empty(lightModeSettings)
    txs.foreach { tx =>
      poolLight.process(tx, us)._2 shouldBe ProcessingOutcome.Accepted
    }
  }

  it should "decline already contained transaction" in {
    val (us, bh) = createUtxoState()
    val genesis = validFullBlock(None, us, bh, Random)
    val wus = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    val txs = validTransactionsFromUtxoState(wus, Random)
    var pool = ErgoMemPool.empty(settings)
    txs.foreach { tx =>
      pool = pool.putWithoutCheck(Seq(tx))
    }
    txs.foreach { tx =>
      pool.process(tx, us)._2.isInstanceOf[ProcessingOutcome.Declined] shouldBe true
    }
  }

  it should "reject double-spending transaction if it is paying no more than one already sitting in the pool" in {
    forAll(smallPositiveInt, smallPositiveInt) { case (n1, n2) =>
      whenever(n1 != n2) {
        val (us, bh) = createUtxoState()
        val genesis = validFullBlock(None, us, bh, Random)
        val wus = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

        val feeProp = settings.chainSettings.monetary.feeProposition
        val inputBox = wus.takeBoxes(1).head
        val feeOut = new ErgoBoxCandidate(inputBox.value, feeProp, creationHeight = 0)

        val prover = ErgoProvingInterpreter(IndexedSeq.empty, parameters)

        def rndContext(n: Int): ContextExtension = ContextExtension(Map(
          (1: Byte) -> ByteArrayConstant(Array.fill(1 + n)(0: Byte)))
        )

        val tx1Like = prover.sign(UnsignedErgoTransaction(
          IndexedSeq(new UnsignedInput(inputBox.id, rndContext(n1))),
          IndexedSeq(feeOut)
        ), IndexedSeq(inputBox), IndexedSeq.empty, emptyStateContext).get

        val tx2Like = prover.sign(UnsignedErgoTransaction(
          IndexedSeq(new UnsignedInput(inputBox.id, rndContext(n2))),
          IndexedSeq(feeOut)
        ), IndexedSeq(inputBox), IndexedSeq.empty, emptyStateContext).get

        val tx1 = ErgoTransaction(tx1Like.inputs, tx1Like.outputCandidates)
        val tx2 = ErgoTransaction(tx2Like.inputs, tx2Like.outputCandidates)

        val pool0 = ErgoMemPool.empty(settings)
        val (pool, tx1Outcome) = pool0.process(tx1, us)

        tx1Outcome shouldBe ProcessingOutcome.Accepted

        // tx1 and tx2 are spending the same input, and paying the same fee.
        // So if tx2 is about a bigger or equal size, it should be rejected as it is paying less for a byte.
        // Otherwise, tx2 is paying more for a byte and then it is replacing tx1.
        if (tx2.size >= tx1.size) {
          pool.process(tx2, us)._2.isInstanceOf[ProcessingOutcome.DoubleSpendingLoser] shouldBe true
        } else {
          val (updPool, outcome) = pool.process(tx2, us)
          outcome shouldBe ProcessingOutcome.Accepted
          updPool.size shouldBe 1
          updPool.take(1).head.id shouldBe tx2.id
        }
      }
    }
  }

  it should "decline transactions invalidated earlier" in {
    val us = createUtxoState()._1
    var pool = ErgoMemPool.empty(settings)
    forAll(invalidBlockTransactionsGen) { blockTransactions =>
      blockTransactions.txs.foreach(tx => pool = pool.process(tx, us)._1)
      blockTransactions.txs.foreach(tx =>
        pool.process(tx, us)._2.isInstanceOf[ProcessingOutcome.Declined] shouldBe true)
    }
  }

  it should "decline transactions not meeting min fee" in {
    val (us, bh) = createUtxoState()
    val genesis = validFullBlock(None, us, bh, Random)
    val wus = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    val txs = validTransactionsFromUtxoState(wus, Random)

    val maxSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(minimalFeeAmount = Long.MaxValue))
    val pool = ErgoMemPool.empty(maxSettings)
    txs.foreach { tx =>
      val (_, outcome) = pool.process(tx, us)
      outcome.isInstanceOf[ProcessingOutcome.Declined] shouldBe true
      outcome.asInstanceOf[ProcessingOutcome.Declined]
        .e.getMessage.contains("Min fee not met") shouldBe true
    }

    val minSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(minimalFeeAmount = 0))
    val pool2 = ErgoMemPool.empty(minSettings)
    txs.foreach { tx =>
      val (_, outcome) = pool2.process(tx, us)
      outcome shouldBe ProcessingOutcome.Accepted
    }
  }

  it should "invalidate invalid transaction" in {
    val us = createUtxoState()._1
    val pool = ErgoMemPool.empty(settings)
    forAll(invalidBlockTransactionsGen) { blockTransactions =>
      blockTransactions.txs.forall(pool.process(_, us)._2.isInstanceOf[ProcessingOutcome.Invalidated]) shouldBe true
    }
  }

  it should "accept only unique transactions" in {
    val pool = ErgoMemPool.empty(settings)
    val tx = invalidErgoTransactionGen.sample.get
    pool.putWithoutCheck(Seq(tx, tx, tx)).size shouldBe 1
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
    val lessPrioritizedTxs = txsWithAscendingPriority.init
    val mostPrioritizedTx = txsWithAscendingPriority.last
    pool = pool.putWithoutCheck(lessPrioritizedTxs)

    pool.size shouldBe 4
    pool.getAll should contain only (lessPrioritizedTxs: _*)
    pool = pool.putWithoutCheck(Seq(mostPrioritizedTx))
    pool.size shouldBe 4
    pool.getAll should contain only (mostPrioritizedTx +: lessPrioritizedTxs.tail: _*)
  }

  it should "Accept output of pooled transactions" in {
    val (us, bh) = createUtxoState()
    val genesis = validFullBlock(None, us, bh, Random)
    val wus = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    val txs = validTransactionsFromUtxoState(wus, Random)
    var pool = ErgoMemPool.empty(settings)
    txs.foreach { tx =>
      pool = pool.putWithoutCheck(Seq(tx))
    }
    txs.foreach { tx =>
      val spendingBox = tx.outputs.head
      val (newPool, outcome) = pool.process(tx.copy(inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
        outputCandidates = IndexedSeq(spendingBox)), us)
      outcome shouldBe ProcessingOutcome.Accepted
      pool = newPool
    }
  }

  it should "consider families for replacement policy" in {
    val (us, bh) = createUtxoState()
    val genesis = validFullBlock(None, us, bh, Random)
    val wus = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    var txs = validTransactionsFromUtxoState(wus, Random)
    val family_depth = 10
    val limitedPoolSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(mempoolCapacity = (family_depth + 1) * txs.size))
    var pool = ErgoMemPool.empty(limitedPoolSettings)
    txs.foreach { tx =>
      pool = pool.putWithoutCheck(Seq(tx))
    }
    for (i <- 1 to family_depth) {
      txs = txs.map(tx => {
        val spendingBox = tx.outputs.head
        val newTx = tx.copy(inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
          outputCandidates = IndexedSeq(spendingBox))
        val (newPool, outcome) = pool.process(newTx, us)
        outcome shouldBe ProcessingOutcome.Accepted
        pool = newPool
        newTx
      })
    }
    pool.size shouldBe (family_depth + 1) * txs.size
    txs.foreach { tx =>
      val sb = tx.outputs.head
      pool.process(tx.copy(inputs = IndexedSeq(new Input(sb.id, emptyProverResult)),
        outputCandidates = IndexedSeq(new ErgoBoxCandidate(sb.value+1, sb.ergoTree, sb.creationHeight, sb.additionalTokens, sb.additionalRegisters))), us)._2.isInstanceOf[ProcessingOutcome.Declined] shouldBe true
    }
  }

  it should "correctly remove transaction from pool and rebuild families" in {
    val (us, bh) = createUtxoState()
    val genesis = validFullBlock(None, us, bh, Random)
    val wus = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    var txs = validTransactionsFromUtxoState(wus, Random)
    var allTxs = txs
    val family_depth = 10
    val limitedPoolSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(mempoolCapacity = (family_depth + 1) * txs.size))
    var pool = ErgoMemPool.empty(limitedPoolSettings)
    txs.foreach { tx =>
      pool = pool.putWithoutCheck(Seq(tx))
    }
    for (i <- 1 to family_depth) {
      txs = txs.map(tx => {
        val spendingBox = tx.outputs.head
        val newTx = tx.copy(inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
          outputCandidates = IndexedSeq(spendingBox))
        val (newPool, outcome) = pool.process(newTx, us)
        outcome shouldBe ProcessingOutcome.Accepted
        pool = newPool
        allTxs = allTxs :+ newTx
        newTx
      })
    }
    pool.size shouldBe (family_depth + 1) * txs.size
    allTxs.foreach { tx =>
      pool = pool.remove(tx)
    }
    pool.size shouldBe 0
  }

  it should "return results take / getAll / getAllPrioritized sorted by priority" in {
    val feeProp = settings.chainSettings.monetary.feeProposition

    val (us, bh) = createUtxoState()
    val genesis = validFullBlock(None, us, bh, Random)
    val wus = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    var txs = validTransactionsFromUtxoState(wus, Random)
    val family_depth = 10
    val limitedPoolSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(mempoolCapacity = (family_depth + 1) * txs.size))
    var pool = ErgoMemPool.empty(limitedPoolSettings)
    txs.foreach { tx =>
      pool = pool.putWithoutCheck(Seq(tx))
    }
    for (i <- 1 to family_depth) {
      txs = txs.map(tx => {
        val spendingBox = tx.outputs.head

        val sc = spendingBox.toCandidate
        val out0 = new ErgoBoxCandidate(sc.value - 55000, sc.ergoTree, sc.creationHeight)
        val out1 = new ErgoBoxCandidate(55000, feeProp, sc.creationHeight)

        val newTx = tx.copy(inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
          outputCandidates = IndexedSeq(out0, out1))
        val (newPool, outcome) = pool.process(newTx, us)
        outcome shouldBe ProcessingOutcome.Accepted
        pool = newPool
        newTx
      })
    }

    val weights = pool.weightedTransactionIds(11)
    val ids = weights.map(_.id)

    pool.take(11).toSeq.map(_.id) shouldBe ids
    pool.getAll.map(_.id) shouldBe ids
    pool.getAllPrioritized.map(_.id) shouldBe ids

    val conformingTxs = pool.take(3).toSeq
    val stateWithTxs = wus.withTransactions(conformingTxs)

    conformingTxs.flatMap(_.inputs).map(_.boxId).forall(bIb => stateWithTxs.boxById(bIb).isDefined) shouldBe true
  }
}
