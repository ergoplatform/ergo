package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.utils.generators.{ErgoTransactionGenerators, ErgoGenerators}
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import scorex.testkit.properties.mempool.MempoolFilterPerformanceTest

class MempoolPerformanceBench extends AnyPropSpec
  with MempoolFilterPerformanceTest[ErgoTransaction, ErgoMemPool]
  with ErgoGenerators
  with ErgoTransactionGenerators {

  override val memPool: ErgoMemPool = ErgoMemPool.empty(settings)
  override val memPoolGenerator: Gen[ErgoMemPool] = emptyMemPoolGen
  override val transactionGenerator: Gen[ErgoTransaction] = invalidErgoTransactionGen
}
