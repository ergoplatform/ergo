package org.ergoplatform.nodeView.viewholder

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.state.StateType.Utxo
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, TestCase}

/**
  * A chain switch returns the rolled-back blocks' transactions to the mempool, also when the header chain is far
  * ahead of the full chain at the moment of the switch (a node catching up across a reorganisation).
  */
class ChainSwitchMempoolSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  private def switchCase(headersAhead: Int) = TestCase(s"chain switch with $headersAhead headers ahead returns rolled-back txs to the pool") { fixture =>
    import fixture._
    val (us1, bh1) = createUtxoState(fixture.settings)
    val (us2, bh2) = createUtxoState(fixture.settings)
    val genesis = validFullBlock(parentOpt = None, us1, bh1)
    val wus1 = WrappedUtxoState(us1, bh1, fixture.settings).applyModifier(genesis)(_ => ()).get
    var wus2 = WrappedUtxoState(us2, bh2, fixture.settings).applyModifier(genesis)(_ => ()).get
    wus1.rootDigest shouldEqual wus2.rootDigest
    applyBlock(genesis) shouldBe 'success

    // chain 1: one block carrying the transaction t
    val tInput = ErgoState.newBoxes(genesis.transactions).find(_.ergoTree == TrueTree).get
    val t: ErgoTransaction = validTransactionFromBoxes(IndexedSeq(tInput))
    val chain1 = validFullBlock(Some(genesis), wus1, Seq(t))
    applyBlock(chain1) shouldBe 'success
    getBestFullBlockOpt shouldBe Some(chain1)
    getPoolSize shouldBe 0

    // chain 2: 2 + headersAhead blocks from genesis, none spending t's input, so t stays valid on it
    val chain2 = (1 to 2 + headersAhead).foldLeft(Vector.empty[ErgoFullBlock]) { (acc, _) =>
      val boxes = wus2.takeBoxes(100).filter(b => b.ergoTree == TrueTree && b.id.sameElements(tInput.id) == false)
      val tx = validTransactionFromBoxes(IndexedSeq(boxes.head))
      val block = validFullBlock(Some(acc.lastOption.getOrElse(genesis)), wus2, Seq(tx))
      wus2 = wus2.applyModifier(block)(_ => ()).get
      acc :+ block
    }

    // all of chain 2's headers first, then the bodies of its first two blocks: the second outweighs chain 1
    chain2.foreach(b => applyHeader(b.header) shouldBe 'success)
    applyPayload(chain2(0)) shouldBe 'success
    applyPayload(chain2(1)) shouldBe 'success
    getBestFullBlockOpt shouldBe Some(chain2(1))
    getHistory.headersHeight - getHistory.fullBlockHeight shouldBe headersAhead

    // t was in the rolled-back block and is valid on chain 2: it should be back in the pool
    withClue(s"headers ahead at the switch: $headersAhead; pool: ") {
      getCurrentView.pool.getAll.map(_.id) should contain(t.id)
    }
  }

  private val cases = List(switchCase(0), switchCase(21))

  NodeViewTestConfig.verifyTxConfigs.filter(_.stateType == Utxo).foreach { c =>
    cases.foreach { tc =>
      property(s"${tc.name} - $c") {
        tc.run(parameters, c)
      }
    }
  }
}
