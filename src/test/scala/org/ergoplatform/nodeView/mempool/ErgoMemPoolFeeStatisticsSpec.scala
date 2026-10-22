package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.SortingOption
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoNodeTestConstants
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.ADKey
import sigma.interpreter.ProverResult

/**
  * Regression tests for #2635: conflict evictions are not confirmations.
  * Synthetic transactions exercise pool bookkeeping without state validation.
  * Direct cleanup isolates statistics from the batch membership guard in #2526.
  */
class ErgoMemPoolFeeStatisticsSpec extends AnyFlatSpec with Matchers {

  private implicit val settings: ErgoSettings = ErgoNodeTestConstants.settings.copy(
    nodeSettings = ErgoNodeTestConstants.settings.nodeSettings.copy(
      mempoolSorting = SortingOption.FeePerByte
    )
  )

  private def feeTx(inputSeed: Byte, fee: Long): ErgoTransaction =
    ErgoTransaction(
      IndexedSeq(new Input(ADKey @@ Array.fill(32)(inputSeed), ProverResult.empty)),
      IndexedSeq(new ErgoBoxCandidate(
        fee,
        settings.chainSettings.monetary.feeProposition,
        creationHeight = 0
      ))
    )

  private def totals(stats: MemPoolStatistics): (Long, Int, Long) =
    (
      stats.takenTxns,
      stats.histogram.map(_.nTxns).sum,
      stats.histogram.map(_.totalFee).sum
    )

  "Mempool fee statistics" should "exclude conflicts of an absent block transaction" in {
    val winner = feeTx(inputSeed = 1, fee = 2000000L)
    val conflict = feeTx(inputSeed = 1, fee = 1000000L)
    val unrelated = feeTx(inputSeed = 2, fee = 3000000L)
    val before = ErgoMemPool.empty(settings).put(
      Seq(conflict, unrelated).map(tx => UnconfirmedTransaction(tx, None))
    )

    winner.id should not be conflict.id
    winner.inputs.head.boxId shouldBe conflict.inputs.head.boxId
    before.contains(winner.id) shouldBe false
    before.pool.transactionsRegistry.contains(conflict.id) shouldBe true
    totals(before.stats) shouldBe ((0L, 0, 0L))

    val after = before.removeTxAndDoubleSpends(winner)

    after.contains(conflict.id) shouldBe false
    after.getAll.map(_.id).toSet shouldBe Set(unrelated.id)
    withClue("(takenTxns, histogram transaction count, histogram fee total): ") {
      totals(after.stats) shouldBe totals(before.stats)
    }
    after.stats.histogram shouldBe before.stats.histogram
  }

  it should "preserve prior confirmation statistics when evicting a conflict" in {
    val confirmed1 = feeTx(inputSeed = 3, fee = 3000000L)
    val confirmed2 = feeTx(inputSeed = 4, fee = 4000000L)
    val winner = feeTx(inputSeed = 1, fee = 2000000L)
    val conflict = feeTx(inputSeed = 1, fee = 1000000L)
    val populated = ErgoMemPool.empty(settings).put(
      Seq(confirmed1, confirmed2, conflict).map(tx => UnconfirmedTransaction(tx, None))
    )
    val before = populated.removeWithDoubleSpends(Seq(confirmed1, confirmed2))
    before.stats.takenTxns shouldBe 2L
    before.stats.histogram.map(_.nTxns).sum shouldBe 2
    before.contains(winner.id) shouldBe false
    before.contains(conflict.id) shouldBe true

    val after = before.removeTxAndDoubleSpends(winner)

    after.size shouldBe 0
    withClue("(takenTxns, histogram transaction count, histogram fee total): ") {
      totals(after.stats) shouldBe totals(before.stats)
    }
    after.stats shouldBe before.stats
  }

  it should "count a pooled confirmed transaction once across repeated batch cleanup" in {
    val confirmed = feeTx(inputSeed = 1, fee = 2000000L)
    val unrelated = feeTx(inputSeed = 2, fee = 3000000L)
    val before = ErgoMemPool.empty(settings).put(
      Seq(confirmed, unrelated).map(tx => UnconfirmedTransaction(tx, None))
    )

    val after = before.removeWithDoubleSpends(Seq(confirmed))

    after.contains(confirmed.id) shouldBe false
    after.getAll.map(_.id).toSet shouldBe Set(unrelated.id)
    totals(after.stats) shouldBe ((1L, 1, 2000000L * 1024 / confirmed.size))

    val repeated = after.removeWithDoubleSpends(Seq(confirmed))

    repeated.getAll.map(_.id).toSet shouldBe Set(unrelated.id)
    repeated.stats shouldBe after.stats
  }
}
