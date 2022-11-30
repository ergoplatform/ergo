package scorex.testkit.properties.mempool

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
trait MempoolTransactionsTest
  extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with MemoryPoolTest {

  val transactionSeqGenerator: Gen[Seq[ErgoTransaction]] = Gen.nonEmptyContainerOf[Seq, ErgoTransaction](transactionGenerator)
  val unconfirmedTxSeqGenerator: Gen[Seq[UnconfirmedTransaction]] =
    transactionSeqGenerator.map(txs => txs.map(tx => UnconfirmedTransaction(tx, None)))

  property("Size of mempool should increase when adding a non-present transaction") {
    forAll(memPoolGenerator, unconfirmedTxGenerator) { (mp: ErgoMemPool, unconfirmedTx: UnconfirmedTransaction) =>
      val m: ErgoMemPool = mp.put(unconfirmedTx).get
      m.size shouldEqual 1
    }
  }

  property("Size of mempool should not increase when adding a present transaction") {
    forAll(memPoolGenerator, unconfirmedTxGenerator) { (mp: ErgoMemPool, unconfirmedTx: UnconfirmedTransaction) =>
      val m: ErgoMemPool = mp.put(unconfirmedTx).get
      val m2: ErgoMemPool = m.put(unconfirmedTx).get
      m2.size shouldEqual 1
    }
  }

  property("Size of mempool should increase when adding a collection of non-present transactions " +
    "without duplicates (with check)") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction]) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs).get
      m.size shouldEqual unconfirmedTxs.size
    }
  }

  property("Size of mempool should increase for a number of unique non-present transactions " +
    "when adding a collection of non-present txs with duplicates (with check)") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction]) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs ++ unconfirmedTxs).get
      m.size shouldEqual unconfirmedTxs.size
    }
  }

  property("Size of mempool should not increase when adding a collection of present transactions (with check)") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction]) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs).get
      val m2: ErgoMemPool = m.put(unconfirmedTxs).get
      m2.size shouldEqual unconfirmedTxs.size
    }
  }

  property("Size of mempool should increase when adding a collection of non-present transactions " +
    "without duplicates (without check)") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction]) =>
      val m: ErgoMemPool = mp.putWithoutCheck(unconfirmedTxs)
      m.size shouldEqual unconfirmedTxs.size
    }
  }

  property("Size of mempool should increase for a number of unique non-present transactions " +
    "when adding a collection of non-present transactions with duplicates (without check)") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction]) =>
      val m: ErgoMemPool = mp.putWithoutCheck(unconfirmedTxs ++ unconfirmedTxs)
      m.size shouldEqual unconfirmedTxs.size
    }
  }

  property("Size of mempool should not increase when adding a collection of present transactions (without check)") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction]) =>
      val m: ErgoMemPool = mp.putWithoutCheck(unconfirmedTxs)
      val m2: ErgoMemPool = m.putWithoutCheck(unconfirmedTxs)
      m2.size shouldEqual unconfirmedTxs.size
    }
  }

  property("Size of mempool should decrease when removing a present transaction") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction]) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs).get
      val m2: ErgoMemPool = m.remove(unconfirmedTxs.headOption.get)
      m2.size shouldBe unconfirmedTxs.size - 1
    }
  }

  property("Size of mempool should not decrease when removing a non-present transaction") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator, unconfirmedTxGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction], unconfirmedTx: UnconfirmedTransaction) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs).get
      val m2: ErgoMemPool = m.remove(unconfirmedTx)
      m2.size shouldBe unconfirmedTxs.size
    }
  }

  property("Mempool transactions should be filtered successfully") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction]) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs).get
      val m2: ErgoMemPool = m.filter(tx => tx equals unconfirmedTxs.headOption.get)
      m2.size shouldBe 1
    }
  }

  property("Present transactions should be available by id") {
    forAll(memPoolGenerator, unconfirmedTxGenerator) { (mp: ErgoMemPool, unconfirmedTx: UnconfirmedTransaction) =>
      val m: ErgoMemPool = mp.put(unconfirmedTx).get
      m.modifierById(unconfirmedTx.transaction.id).isDefined shouldBe true
    }
  }

  property("Non-present transactions should not be available by id") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator, unconfirmedTxGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction], unconfirmedTx: UnconfirmedTransaction) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs).get
      m.modifierById(unconfirmedTx.transaction.id).isDefined shouldBe false
    }
  }

  property("Mempool should contain present transactions") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction]) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs).get
      m.contains(unconfirmedTxs.headOption.get.transaction.id) shouldBe true
    }
  }

  property("Mempool should not contain non-present transactions") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator, unconfirmedTxGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction], unconfirmedTx: UnconfirmedTransaction) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs).get
      m.contains(unconfirmedTx.transaction.id) shouldBe false
    }
  }

  property("Present transactions should be obtained by their ids") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator, unconfirmedTxGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction], unconfirmedTx: UnconfirmedTransaction) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs :+ unconfirmedTx).get
      m.getAll(unconfirmedTxs.map(_.transaction.id)) sameElements unconfirmedTxs
    }
  }

  property("Non-present transactions should not be obtained by their ids") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator, unconfirmedTxGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction], unconfirmedTx: UnconfirmedTransaction) =>
      val m: ErgoMemPool = mp.put(unconfirmedTx).get
      m.getAll(unconfirmedTxs.map(_.transaction.id)).size shouldBe 0
    }
  }

  property("Required number of transactions should be taken from mempool") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator, unconfirmedTxGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction], tx: UnconfirmedTransaction) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs :+ tx).get
      m.take(unconfirmedTxs.size).size shouldBe unconfirmedTxs.size
    }
  }

  property("Maximum number of transactions that can be taken should equals mempool size") {
    forAll(memPoolGenerator, unconfirmedTxSeqGenerator) { (mp: ErgoMemPool, unconfirmedTxs: Seq[UnconfirmedTransaction]) =>
      val m: ErgoMemPool = mp.put(unconfirmedTxs).get
      m.take(unconfirmedTxs.size + 1).size shouldBe m.size
    }
  }
}
