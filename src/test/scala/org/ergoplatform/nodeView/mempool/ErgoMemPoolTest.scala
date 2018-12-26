package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool.ProcessingOutcome
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.utils.generators.ErgoGenerators
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

import scala.util.Random

class ErgoMemPoolTest extends FlatSpec
  with ErgoGenerators
  with ErgoTestHelpers
  with PropertyChecks {

  it should "accept valid transaction" in {
    val (us, bh) = createUtxoState()
    val genesis = validFullBlockWithBlockHolder(None, us, bh, Random)._1
    val wus = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    val txs = validTransactionsFromUtxoState(wus, Random)
    val pool = ErgoMemPool.empty(settings)
    txs.foreach { tx =>
      pool.putIfValid(tx, us)._2 shouldBe ProcessingOutcome.Accepted
    }
  }

  it should "decline already contained transaction" in {
    val (us, bh) = createUtxoState()
    val genesis = validFullBlockWithBlockHolder(None, us, bh, Random)._1
    val wus = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    val txs = validTransactionsFromUtxoState(wus, Random)
    var pool = ErgoMemPool.empty(settings)
    txs.foreach { tx =>
      pool = pool.putWithoutCheck(Seq(tx))
    }
    txs.foreach { tx =>
      pool.putIfValid(tx, us)._2 shouldBe ProcessingOutcome.Declined
    }
  }

  it should "decline transactions invalidated earlier" in {
    val us = createUtxoState()._1
    var pool = ErgoMemPool.empty(settings)
    forAll(invalidBlockTransactionsGen) { blockTransactions =>
      blockTransactions.txs.foreach(tx => pool = pool.putIfValid(tx, us)._1)
      blockTransactions.txs.foreach(tx => pool.putIfValid(tx, us)._2 shouldBe ProcessingOutcome.Declined)
    }
  }

  it should "invalidate invalid transaction" in {
    val us = createUtxoState()._1
    val pool = ErgoMemPool.empty(settings)
    forAll(invalidBlockTransactionsGen) { blockTransactions =>
      blockTransactions.txs.forall(pool.putIfValid(_, us)._2.isInstanceOf[ProcessingOutcome.Invalidated]) shouldBe true
    }
  }

  it should "drop less prioritized transaction in case of pool overflow" in {
    val limitedPoolSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(mempoolCapacity = 4))
    var pool = ErgoMemPool.empty(limitedPoolSettings)
    val txs = Gen.listOfN(5, invalidErgoTransactionGen).sample.get
    val txsWithAscendingPriority = txs.zipWithIndex.foldLeft(Seq.empty[ErgoTransaction]) { case (acc, (tx, idx)) =>
      val proposition = ErgoState.feeProposition(settings.chainSettings.monetary.minerRewardDelay)
      tx.outputCandidates.headOption match {
        case Some(c) =>
          acc :+ tx.copy(outputCandidates = IndexedSeq(
            new ErgoBoxCandidate(idx * 10000 + 1, proposition, c.creationHeight, c.additionalTokens, c.additionalRegisters)))
        case _ =>
          acc
      }
    }
    val lessPrioritizedTxs = txsWithAscendingPriority.init
    val mostPrioritizedTx = txsWithAscendingPriority.last
    pool = pool.putWithoutCheck(lessPrioritizedTxs)

    pool.unconfirmed.size shouldBe 4
    pool.getAll should contain only (lessPrioritizedTxs: _*)
    pool = pool.putWithoutCheck(Seq(mostPrioritizedTx))
    pool.getAll should contain only (mostPrioritizedTx +: lessPrioritizedTxs.tail: _*)
  }

}
