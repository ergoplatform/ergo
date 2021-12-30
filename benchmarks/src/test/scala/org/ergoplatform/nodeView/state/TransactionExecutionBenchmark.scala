package org.ergoplatform.nodeView.state

import org.ergoplatform.Utils
import org.ergoplatform.Utils.BenchReport
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.NVBenchmark
import org.ergoplatform.settings.{ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.settings.Parameters.MaxBlockCostIncrease
import org.ergoplatform.utils.{HistoryTestHelpers, RandomWrapper}
import scorex.core.validation.ValidationResult.Valid
import scorex.db.ByteArrayWrapper

import scala.collection.mutable
import scala.util.Try

object TransactionExecutionBenchmark extends HistoryTestHelpers with NVBenchmark {

  val WarmupRuns = 3

  def stateContextWithMaxCost(manualCost: Int): UpcomingStateContext = {
    val table2: Map[Byte, Int] = Parameters.DefaultParameters + (MaxBlockCostIncrease -> manualCost)
    val params2 = new Parameters(height = 0,
      parametersTable = table2,
      proposedUpdate = ErgoValidationSettingsUpdate.empty)
    emptyStateContext.copy(currentParameters = params2)(settings)
  }

  def main(args: Array[String]): Unit = {

    val startTs = System.currentTimeMillis()

    val bh = BoxHolder(genesisBoxes)
    val txs = (1 to 5000).foldLeft(mutable.WrappedArray.newBuilder[ErgoTransaction]) { case (txAcc, _) =>
      val (transactions, _) = validTransactionsFromBoxes(10000, bh.boxes.values.toVector, new RandomWrapper)
      val allBoxIds = bh.boxes.keys.toSet
      val txsFromBoxesOnly = transactions.filter { tx =>
        tx.inputs.map(i => ByteArrayWrapper(i.boxId)).forall(allBoxIds.contains) &&
          tx.dataInputs.map(i => ByteArrayWrapper(i.boxId)).forall(allBoxIds.contains)
      }
      txAcc ++= txsFromBoxesOnly
    }.result()

    val boxes = bh.boxes
    val stateContext = stateContextWithMaxCost(Int.MaxValue)
    def bench: Long =
      Utils.time {
        assert(ErgoState.execTransactions(txs, stateContext)(id => Try(boxes(ByteArrayWrapper(id)))) == Valid(178665000))
      }.toLong

    (0 to WarmupRuns).foreach(_ => bench)
    val et = bench

    println(s"Performance of `${txs.size} transactions execution`: $et ms")

    Utils.dumpToFile("TransactionExecutionBenchmark", startTs, Seq(BenchReport(s"${txs.size} transactions execution", et)))

    System.exit(0)

  }

}
