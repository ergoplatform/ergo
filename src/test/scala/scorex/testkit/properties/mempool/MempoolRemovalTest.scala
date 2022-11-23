package scorex.testkit.properties.mempool

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.testkit.TestkitHelpers
import scorex.testkit.generators.ArbitraryTransactionsCarryingModifierProducer
import scorex.util.ScorexLogging

trait MempoolRemovalTest extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with ScorexLogging
  with TestkitHelpers
  with MemoryPoolTest
  with ArbitraryTransactionsCarryingModifierProducer {

  val historyGen: Gen[ErgoHistory]

  //todo: this test doesn't check anything. It should be reworked as a test for node view holder
  property("Transactions once added to block should be removed from Mempool") {
    val min = 1
    val max = 10
    forAll(Gen.choose(min, max)) { _ =>
      var m: ErgoMemPool = memPool
      // var h: ErgoHistory = historyGen.sample.get
      forAll(transactionGenerator) { tx: ErgoTransaction =>
        m = m.put(UnconfirmedTransaction(tx, None)).get
      }
      // var prevMempoolSize = m.size
      // val b = modifierWithTransactions(Some(m), None)
      //todo: fix    (m.size + b.transactions.get.size) shouldEqual prevMempoolSize
    }
  }
}


