package org.ergoplatform.popow

import org.ergoplatform.Utils
import org.ergoplatform.nodeView.history.HistoryTestHelpers
import org.ergoplatform.nodeView.state.StateType

object HistoryProvingModeBench extends HistoryTestHelpers with App {

  val WarmupRuns = 2

  override def main(args: Array[String]): Unit = {

    val history = generateHistory(
      verifyTransactions = true, StateType.Utxo, poPowProve = true, BlocksToKeep)

    history.prove(poPowParams) shouldBe 'failure

    genChain(50000, history).flatMap(x => Seq(x.header, x.extension)).foreach(history.append)

    def bench: Long =
      Utils.time {
        history.prove(poPowParams).get
      }.toLong

    (0 to WarmupRuns).foreach(_ => bench)

    val et = (0 to 3).map(_ => bench).sum / 3

    println(s"Performance of `chain proving`: $et ms")

    System.exit(0)

  }

}
