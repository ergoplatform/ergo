package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.testkit.properties.mempool.MempoolFilterPerformanceTest
import org.ergoplatform.utils.generators.{ErgoGenerators, ErgoTransactionGenerators}
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec

class MempoolPerformanceBench extends AnyPropSpec
  with MempoolFilterPerformanceTest
  with ErgoGenerators
  with ErgoTransactionGenerators {

  override val memPool: ErgoMemPool = ErgoMemPool.empty(settings)
  override val memPoolGenerator: Gen[ErgoMemPool] = emptyMemPoolGen
  override val transactionGenerator: Gen[ErgoTransaction] = invalidErgoTransactionGen
  override val unconfirmedTxGenerator: Gen[UnconfirmedTransaction] =
    invalidErgoTransactionGen.map(tx => UnconfirmedTransaction(tx, None))
}
