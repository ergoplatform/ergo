package org.ergoplatform.utils

import org.ergoplatform.nodeView.ErgoInterpreter
import sigmastate.eval.{IRContext, RuntimeIRContext}

class CostingBenchmark extends ErgoPropertyTest {

  implicit lazy val context: IRContext = new RuntimeIRContext

  private implicit val verifier: ErgoInterpreter = new ErgoInterpreter(emptyStateContext.currentParameters)

  property("simple but slow trancations (#645)") {
    val txGen = validErgoTransactionGenTemplate(3, 5, 600, 800)
    forAll(txGen) { case (from, tx) =>
      val t0 = System.currentTimeMillis()
      val initTxCost = tx.statefulValidity(from, IndexedSeq(), emptyStateContext).get
      val t = System.currentTimeMillis()

      println("Tx cost:" + initTxCost)
      println("Tx size: " + tx.bytes.length)
      println("Input tokens: " + from.map(_.additionalTokens.size).sum)
      println("Output tokens: " + tx.outputCandidates.map(_.additionalTokens.size).sum)
      println("tx validation time: " + (t - t0))
      println("================================")
    }
  }

}
