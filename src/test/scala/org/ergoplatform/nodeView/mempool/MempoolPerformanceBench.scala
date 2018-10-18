package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.utils.generators.{ErgoGenerators, ErgoTransactionGenerators}
import org.scalacheck.Gen
import org.scalatest.PropSpec
import scorex.testkit.properties.mempool.MempoolFilterPerformanceTest

class MempoolPerformanceBench extends PropSpec
  with MempoolFilterPerformanceTest[ErgoTransaction, ErgoMemPool]
  with ErgoGenerators
  with ErgoTransactionGenerators {

  override val memPool: ErgoMemPool = ErgoMemPool.empty
  override val memPoolGenerator: Gen[ErgoMemPool] = emptyMemPoolGen
  override val transactionGenerator: Gen[ErgoTransaction] = invalidErgoTransactionGen
}
