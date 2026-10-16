package org.ergoplatform.nodeView.viewholder

import akka.testkit.TestProbe
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.RecheckMempool
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, TestCase}

/**
  * What a chain switch does with the rolled-back blocks' transactions, also when the header chain is far ahead of
  * the full chain at the moment of the switch (a node catching up across a reorganisation).
  */
class ChainSwitchMempoolSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  /**
    * Chain 1 is one block carrying t. Chain 2 forks at genesis, is `2 + headersAhead` blocks long, and wins when the
    * body of its second block is applied, with `headersAhead` of its headers still ahead of the full chain. With
    * `chain2SpendsTInput`, chain 2's first block spends t's input in another transaction, so t is invalid on chain 2.
    *
    * Expected: t is back in the pool after the switch if the node was almost synced (fewer than 20 headers ahead) or
    * holds the UTXO set; a digest node taken that far behind drops t, and a resend of t is then accepted. When t is
    * invalid on chain 2, it is still back in a UTXO node's pool, and the recheck ordered at full sync carries it with
    * its input missing from the UTXO set.
    */
  private def switchCase(headersAhead: Int, chain2SpendsTInput: Boolean) = {
    val chain2Kind = if (chain2SpendsTInput) "spending t's input" else "keeping t valid"
    TestCase(s"chain switch to a chain $chain2Kind with $headersAhead headers ahead") { fixture =>
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

      // chain 2: each block spends one box; the first spends t's input if chain2SpendsTInput, no other block does
      val chain2 = (1 to 2 + headersAhead).foldLeft(Vector.empty[ErgoFullBlock]) { (acc, _) =>
        val tx = if (chain2SpendsTInput && acc.isEmpty) {
          // the generator can repeat t for the same input; another transaction is wanted
          Iterator.continually(validTransactionFromBoxes(IndexedSeq(tInput))).find(_.id != t.id).get
        } else {
          val input = wus2.takeBoxes(100).filter(_.ergoTree == TrueTree).filterNot(_.id.sameElements(tInput.id)).head
          validTransactionFromBoxes(IndexedSeq(input))
        }
        val block = validFullBlock(Some(acc.lastOption.getOrElse(genesis)), wus2, Seq(tx))
        wus2 = wus2.applyModifier(block)(_ => ()).get
        acc :+ block
      }
      if (chain2SpendsTInput) {
        chain2.head.transactions.map(_.id) should not contain t.id
        val chain2FirstInputs = chain2.head.transactions.flatMap(_.inputs.map(i => Algos.encode(i.boxId)))
        chain2FirstInputs should contain(Algos.encode(tInput.id))
      }

      // all of chain 2's headers first, then the bodies of its first two blocks: the second outweighs chain 1
      chain2.foreach(b => applyHeader(b.header) shouldBe 'success)
      val recheck = TestProbe()(actorSystem)
      actorSystem.eventStream.subscribe(recheck.ref, classOf[RecheckMempool])
      applyPayload(chain2(0)) shouldBe 'success
      applyPayload(chain2(1)) shouldBe 'success
      getBestFullBlockOpt shouldBe Some(chain2(1))
      getHistory.headersHeight - getHistory.fullBlockHeight shouldBe headersAhead

      val almostSynced = headersAhead < 20
      val poolIds = getCurrentView.pool.getAll.map(_.id)
      withClue(s"$stateType, headers ahead at the switch: $headersAhead; pool: ") {
        if (almostSynced || stateType.holdsUtxoSet) poolIds should contain(t.id) else poolIds should not contain t.id
      }

      if (!almostSynced && !stateType.holdsUtxoSet && !chain2SpendsTInput) {
        // the dropped t can be sent again
        nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(t, None))
        expectMsgType[ProcessingOutcome] shouldBe a[ProcessingOutcome.Accepted]
      }

      if (chain2SpendsTInput && stateType.holdsUtxoSet) {
        // t is back although chain 2 spent its input; the recheck at full sync carries it with its input missing
        chain2.drop(2).foreach(b => applyPayload(b) shouldBe 'success)
        getHistory.fullBlockHeight shouldBe getHistory.headersHeight
        val atFullSync = recheck.expectMsgType[RecheckMempool]
        atFullSync.mempool.getAll.map(_.id) should contain(t.id)
        atFullSync.state.boxById(tInput.id) shouldBe None
      }
    }
  }

  private val cases = for {
    headersAhead <- List(0, 21)
    chain2SpendsTInput <- List(false, true)
  } yield switchCase(headersAhead, chain2SpendsTInput)

  // the UTXO and digest configurations; the digest ones without transaction verification apply no full blocks, so
  // no chain switch happens in them
  NodeViewTestConfig.verifyTxConfigs.foreach { c =>
    cases.foreach { tc =>
      property(s"${tc.name} - $c") {
        tc.run(parameters, c)
      }
    }
  }
}
