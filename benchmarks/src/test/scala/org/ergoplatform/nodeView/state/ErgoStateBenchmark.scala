package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.ErgoStateBenchmark.BenchmarkState
import org.ergoplatform.utils.ErgoTestHelpers
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.options.OptionsBuilder
import org.openjdk.jmh.runner.{Runner, RunnerException}

import scala.util.Random

class ErgoStateBenchmark {
  @Benchmark
  def boxChanges(state: BenchmarkState, bh: Blackhole): Unit = {
    bh.consume {
      ErgoState.boxChanges(state.txs)
    }
  }
}

object ErgoStateBenchmark extends ErgoTestHelpers {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[ErgoStateBenchmark].getSimpleName + ".*")
      .forks(1)
      .addProfiler(classOf[GCProfiler])
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class BenchmarkState {
    val blockSize = 1024 * 400
    val txsSeq: Seq[Seq[ErgoTransaction]] = (0 until 5) map { i =>
      validTransactionsFromBoxHolder(boxesHolderGen.sample.get, new Random, blockSize)._1
    }

    def txs: Seq[ErgoTransaction] = txsSeq(Random.nextInt(txsSeq.length))
  }

}