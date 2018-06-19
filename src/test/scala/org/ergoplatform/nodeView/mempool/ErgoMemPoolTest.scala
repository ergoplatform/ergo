package org.ergoplatform.nodeView.mempool

import org.ergoplatform.utils.ErgoPropertyTest

import scala.concurrent.Await
import scala.concurrent.duration._

class ErgoMemPoolTest extends ErgoPropertyTest {
  property("wait for the appearance of transactions") {
    forAll(invalidBlockTransactionsGen) { blockTransactions =>
      val memPool = ErgoMemPool.empty
      val ids = blockTransactions.txs.map(_.id)
      val transactionsFuture = memPool.waitForAll(ids)

      memPool.put(blockTransactions.txs)

      val transactionsFromMempool = Await.result(transactionsFuture, 5.seconds)
      transactionsFromMempool should contain theSameElementsAs blockTransactions.txs
      memPool.waitedForAssembly shouldBe 'empty
    }
  }
}
