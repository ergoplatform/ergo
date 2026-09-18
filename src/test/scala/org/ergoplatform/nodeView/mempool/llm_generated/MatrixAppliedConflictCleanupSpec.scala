package org.ergoplatform.nodeView.mempool.llm_generated

import com.google.common.io.Files.createTempDir
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, OrderedTxPool}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.{BoxHolder, UtxoState}
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import scorex.util.bytesToId
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.data.TrivialProp.TrueProp
import sigma.interpreter.ProverResult

class MatrixAppliedConflictCleanupSpec extends ErgoCorePropertyTest {

  private case class Fixture(
    pool: ErgoMemPool,
    winner: ErgoTransaction,
    restored: ErgoTransaction,
    unrelated: ErgoTransaction
  )

  private def fixture(): Fixture = {
    val boxes = (0 until 3).map { index =>
      new ErgoBox(
        value = 1000000000L,
        ergoTree = ErgoTree.fromProposition(TrueProp),
        creationHeight = 0,
        additionalTokens = Colls.emptyColl,
        additionalRegisters = Map.empty,
        transactionId = bytesToId(Algos.hash(s"matrix-cleanup-box-$index")),
        index = 0
      )
    }
    val state = UtxoState.fromBoxHolder(
      BoxHolder(boxes), None, createTempDir, settings, parameters
    )

    def spend(box: ErgoBox, amount: Long): ErgoTransaction = {
      val height = state.stateContext.currentHeight
      val fee = math.max(1000000L, settings.nodeSettings.minimalFeeAmount)
      new ErgoTransaction(
        IndexedSeq(Input(box.id, ProverResult.empty)),
        IndexedSeq.empty,
        IndexedSeq(
          new ErgoBoxCandidate(amount, box.ergoTree, height),
          new ErgoBoxCandidate(box.value - amount - fee, box.ergoTree, height),
          new ErgoBoxCandidate(fee, settings.chainSettings.monetary.feeProposition, height)
        )
      )
    }

    val winner = spend(boxes(0), 2000000L)
    val restored = spend(boxes(0), 3000000L)
    val unrelated = spend(boxes(1), 4000000L)
    val previouslyRemoved = spend(boxes(2), 5000000L)
    winner.id should not be restored.id
    winner.inputs.head.boxId shouldBe restored.inputs.head.boxId
    Seq(winner, restored, unrelated, previouslyRemoved).foreach { tx =>
      val (_, outcome) = ErgoMemPool.empty(settings)
        .process(UnconfirmedTransaction(tx, None), state)
      outcome match {
        case invalid: ProcessingOutcome.Invalidated => throw invalid.e
        case _ => outcome shouldBe a[ProcessingOutcome.Accepted]
      }
    }

    // Keep nonempty statistics so an absent-transaction reset is observable.
    val pool = ErgoMemPool.empty(settings)
      .put(Seq(unrelated, previouslyRemoved).map(tx => UnconfirmedTransaction(tx, None)))
      .removeWithDoubleSpends(Seq(previouslyRemoved))
    pool.stats.takenTxns shouldBe 1L
    Fixture(pool, winner, restored, unrelated)
  }

  property("an absent applied winner removes a restored conflict and preserves unrelated transactions") {
    val f = fixture()
    val restoredPool = f.pool.put(UnconfirmedTransaction(f.restored, None))
    restoredPool.contains(f.winner.id) shouldBe false
    restoredPool.getAll.map(_.id).toSet shouldBe Set(f.restored.id, f.unrelated.id)

    val cleaned = restoredPool.removeWithDoubleSpends(Seq(f.winner))

    cleaned.getAll.map(_.id) shouldBe Seq(f.unrelated.id)
    cleaned.stats.takenTxns shouldBe restoredPool.stats.takenTxns + 1
  }

  property("a present applied winner is removed and unrelated transactions are preserved") {
    val f = fixture()
    val pool = f.pool.put(UnconfirmedTransaction(f.winner, None))

    val cleaned = pool.removeWithDoubleSpends(Seq(f.winner))

    cleaned.getAll.map(_.id) shouldBe Seq(f.unrelated.id)
    cleaned.stats.takenTxns shouldBe pool.stats.takenTxns + 1
  }

  property("absent nonconflicting cleanup preserves the pool and accumulated statistics") {
    val f = fixture()

    Seq(
      f.pool.removeWithDoubleSpends(Seq(f.winner)),
      f.pool.removeTxAndDoubleSpends(f.winner)
    ).foreach { unchanged =>
      unchanged.getAll shouldBe f.pool.getAll
      unchanged.stats shouldBe f.pool.stats
    }
  }

  property("an empty applied transaction list preserves the pool and statistics") {
    val f = fixture()
    val unchanged = f.pool.removeWithDoubleSpends(Seq.empty)

    unchanged.getAll shouldBe f.pool.getAll
    unchanged.stats shouldBe f.pool.stats
  }

  property("repeated applied transactions do not repeat removal or reset statistics") {
    val f = fixture()
    val restoredPool = f.pool.put(UnconfirmedTransaction(f.restored, None))
    val cleaned = restoredPool.removeWithDoubleSpends(Seq(f.winner, f.winner))

    cleaned.getAll.map(_.id) shouldBe Seq(f.unrelated.id)
    cleaned.stats.takenTxns shouldBe restoredPool.stats.takenTxns + 1
    val repeated = cleaned.removeWithDoubleSpends(Seq(f.winner))
    repeated.getAll shouldBe cleaned.getAll
    repeated.stats shouldBe cleaned.stats
  }

  property("direct cleanup preserves recovery of an ordered entry missing from lookup indexes") {
    val f = fixture()
    val pool = f.pool.put(UnconfirmedTransaction(f.winner, None))
    val ordered = pool.pool
    val incompleteIndexes = new OrderedTxPool(
      ordered.orderedTransactions,
      ordered.transactionsRegistry - f.winner.id,
      ordered.invalidatedTxIds,
      ordered.outputs,
      ordered.inputs -- f.winner.inputs.map(_.boxId)
    )(settings)
    val recovering = new ErgoMemPool(incompleteIndexes, pool.stats, pool.sortingOption)(settings)
    recovering.contains(f.winner.id) shouldBe false
    recovering.getAll.map(_.id).toSet shouldBe Set(f.winner.id, f.unrelated.id)
    f.winner.inputs.foreach(input => recovering.pool.inputs.contains(input.boxId) shouldBe false)

    val cleaned = recovering.removeTxAndDoubleSpends(f.winner)

    cleaned.getAll.map(_.id) shouldBe Seq(f.unrelated.id)
    f.winner.outputs.foreach(output => cleaned.pool.outputs.contains(output.id) shouldBe false)
  }
}
