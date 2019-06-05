package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool.ProcessingOutcome
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.utils.generators.ErgoGenerators
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

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
    val pool = ErgoMemPool.empty(settings)
    txs.foreach { tx =>
      pool.process(tx, us)._2 shouldBe ProcessingOutcome.Accepted
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
        .e.getMessage.contains("Minimal fee amount not met") shouldBe true
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
    pool.getAll should contain only (mostPrioritizedTx +: lessPrioritizedTxs.tail: _*)
  }

}
