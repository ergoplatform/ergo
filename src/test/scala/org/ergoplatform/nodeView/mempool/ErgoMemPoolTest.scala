package org.ergoplatform.nodeView.mempool

import org.ergoplatform.utils.ErgoPropertyTest

import scala.concurrent.Await
import scala.concurrent.duration._

class ErgoMemPoolTest extends ErgoPropertyTest {
  ignore("wait for the appearance of transactions") {
    forAll(invalidBlockTransactionsGen) { blockTransactions =>
      val memPool = ErgoMemPool.empty(settings)
      val ids = blockTransactions.txs.map(_.id)
    }
  }
}
