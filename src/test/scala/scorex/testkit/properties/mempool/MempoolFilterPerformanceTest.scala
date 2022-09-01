package scorex.testkit.properties.mempool

import java.security.MessageDigest
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.utils._

trait MempoolFilterPerformanceTest
  extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with MemoryPoolTest {

  var initializedMempool: Option[ErgoMemPool] = None

  val thresholdInHashes = 500000

  private val HeatJVMHashesCount = 1000000 //to heat up JVM, just in case it is cold

  val thresholdSecs: Double = {
    //heat up
    (1 to HeatJVMHashesCount).foreach(i => MessageDigest.getInstance("SHA-256").digest(("dummy" + i).getBytes()))

    val t0 = System.currentTimeMillis()
    (1 to thresholdInHashes).foreach(i => MessageDigest.getInstance("SHA-256").digest(("dummy" + i).getBytes()))
    val t = System.currentTimeMillis()
    (t - t0) / 1000.0
  }

  property("Mempool should be able to store a lot of transactions") {
    var m: ErgoMemPool = memPool
    (0 until 1000) foreach { _ =>
      forAll(transactionGenerator) { tx: ErgoTransaction =>
        m = m.put(UnconfirmedTransaction(tx, None)).get
      }
    }
    m.size should be > 1000
    initializedMempool = Some(m)
  }

  property("Mempool filter of non-existing transaction should be fast") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val m = initializedMempool.get
    forAll(transactionGenerator) { tx: ErgoTransaction =>
      val (time, _) = profile(m.filter(Seq(UnconfirmedTransaction(tx, None))))
      assert(time < thresholdSecs)
    }
  }

  property("Mempool filter of existing transaction should be fast") {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    var m = initializedMempool.get
    forAll(transactionGenerator) { tx: ErgoTransaction =>
      val unconfirmedTx = UnconfirmedTransaction(tx, None)
      m = m.put(unconfirmedTx).get
      val (time, _) = profile(m.filter(Seq(unconfirmedTx)))
      assert(time < thresholdSecs)
    }
  }

}
