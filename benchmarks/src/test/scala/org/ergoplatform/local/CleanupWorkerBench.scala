package org.ergoplatform.local

import org.ergoplatform.Utils
import org.ergoplatform.Utils.BenchReport
import org.ergoplatform.local.CleanupWorker.CleanupState
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.NVBenchmark
import org.ergoplatform.nodeView.state.BoxHolder
import org.ergoplatform.utils.{HistoryTestHelpers, RandomWrapper}
import scorex.core.transaction.state.TransactionValidation

import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.{Success, Try}

object CleanupWorkerBench extends HistoryTestHelpers with NVBenchmark {

  val WarmupRuns = 3

  def main(args: Array[String]): Unit = {

    val startTs = System.currentTimeMillis()

    val bh = BoxHolder(genesisBoxes)
    val txs = (1 to 5000).foldLeft(mutable.WrappedArray.newBuilder[ErgoTransaction]) { case (txAcc, _) =>
      val (transactions, _) = validTransactionsFromBoxes(10000, bh.boxes.values.toVector, new RandomWrapper)
      txAcc ++= transactions
    }.result()

    val mockValidator = new TransactionValidation {
      override def validate(tx: ErgoTransaction): Try[Unit] = Success(())
    }

    def bench: Long =
      Utils.time {
        CleanupWorker.validatePool(CleanupState(TreeSet.empty, 0), mockValidator, txs, 1.second)
      }.toLong

    (0 to WarmupRuns).foreach(_ => bench)
    val et = bench

    println(s"Performance of `${txs.size} transactions validation`: $et ms")

    Utils.dumpToFile("CleanupWorkerBench", startTs, Seq(BenchReport(s"${txs.size} transactions validation", et)))

    System.exit(0)

  }

}
