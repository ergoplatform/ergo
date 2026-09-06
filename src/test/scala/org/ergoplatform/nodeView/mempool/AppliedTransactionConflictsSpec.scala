package org.ergoplatform.nodeView.mempool

import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.ADKey

class AppliedTransactionConflictsSpec extends AnyFlatSpec with ErgoTestHelpers with Matchers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private def transaction(seeds: Byte*)(fee: Long): ErgoTransaction = {
    ErgoTransaction(
      seeds.map(seed => new Input(ADKey @@ Array.fill(32)(seed), emptyProverResult)).toIndexedSeq,
      IndexedSeq(new ErgoBoxCandidate(fee, feeProp, creationHeight = 0))
    )
  }

  private def poolWith(txs: ErgoTransaction*): ErgoMemPool = {
    val pool = ErgoMemPool.empty(settings).put(txs.map(UnconfirmedTransaction(_, None)))
    val now = System.currentTimeMillis()
    new ErgoMemPool(pool.pool, MemPoolStatistics(now, 7, now), pool.sortingOption)(settings)
  }

  it should "remove applied transaction conflicts when the applied transaction is absent" in {
    val applied = transaction(1)(1000000L)
    val conflict = transaction(1)(2000000L)
    val unrelated = transaction(2)(1000000L)
    val before = poolWith(conflict, unrelated)
    before.contains(applied.id) shouldBe false
    before.contains(conflict.id) shouldBe true

    val after = before.removeWithDoubleSpends(Seq(applied))

    after.getAll.map(_.id).toSet shouldBe Set(unrelated.id)
    after.spentInputs.toSet shouldBe unrelated.inputs.map(_.boxId).toSet
    after.pool.outputs.keySet shouldBe unrelated.outputs.map(_.id).toSet
    after.stats.takenTxns shouldBe 8
    before.contains(conflict.id) shouldBe true
  }

  it should "preserve statistics when an absent applied transaction has no conflicts" in {
    val applied = transaction(1)(1000000L)
    val unrelated = transaction(2)(1000000L)
    val before = poolWith(unrelated)

    val after = before.removeWithDoubleSpends(Seq(applied))

    after.getAll shouldBe before.getAll
    after.stats shouldBe before.stats
    before.removeTxAndDoubleSpends(applied).stats shouldBe before.stats
  }

  it should "remove conflicts for every input of an absent applied transaction" in {
    val applied = transaction(1, 2)(1000000L)
    val first = transaction(1)(2000000L)
    val second = transaction(2)(2000000L)
    val unrelated = transaction(3)(1000000L)

    val after = poolWith(first, second, unrelated).removeWithDoubleSpends(Seq(applied))

    after.getAll.map(_.id).toSet shouldBe Set(unrelated.id)
    after.stats.takenTxns shouldBe 9
  }

  it should "count a conflict sharing several inputs only once" in {
    val applied = transaction(1, 2)(1000000L)
    val conflict = transaction(1, 2)(2000000L)

    val after = poolWith(conflict).removeWithDoubleSpends(Seq(applied, applied))

    after.size shouldBe 0
    after.stats.takenTxns shouldBe 8
  }

  it should "remove a present applied transaction once and retain its unconfirmed child" in {
    val applied = transaction(1)(1000000L)
    val child = applied.copy(
      inputs = IndexedSeq(new Input(applied.outputs.head.id, emptyProverResult))
    )
    val unrelated = transaction(2)(1000000L)
    val before = poolWith(applied, child, unrelated)

    val after = before.removeWithDoubleSpends(Seq(applied, applied))

    after.getAll.map(_.id).toSet shouldBe Set(child.id, unrelated.id)
    after.stats.takenTxns shouldBe 8
    after.pool.outputs.keySet shouldBe (child.outputs ++ unrelated.outputs).map(_.id).toSet
  }
}
