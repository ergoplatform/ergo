package scorex.testkit.properties.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
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

  property("Size of mempool should increase when adding a non-present transaction") {
    forAll(memPoolGenerator, transactionGenerator) { (mp: ErgoMemPool, tx: ErgoTransaction) =>
      val m: ErgoMemPool = mp.put(tx).get
      m.size shouldEqual 1
    }
  }

  property("Size of mempool should not increase when adding a present transaction") {
    forAll(memPoolGenerator, transactionGenerator) { (mp: ErgoMemPool, tx: ErgoTransaction) =>
      val m: ErgoMemPool = mp.put(tx).get
      val m2: ErgoMemPool = m.put(tx).get
      m2.size shouldEqual 1
    }
  }

  property("Size of mempool should increase when adding a collection of non-present transactions " +
    "without duplicates (with check)") {
    forAll(memPoolGenerator, transactionSeqGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction]) =>
      val m: ErgoMemPool = mp.put(txs).get
      m.size shouldEqual txs.size
    }
  }

  property("Size of mempool should increase for a number of unique non-present transactions " +
    "when adding a collection of non-present txs with duplicates (with check)") {
    forAll(memPoolGenerator, transactionSeqGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction]) =>
      val m: ErgoMemPool = mp.put(txs ++ txs).get
      m.size shouldEqual txs.size
    }
  }

  property("Size of mempool should not increase when adding a collection of present transactions (with check)") {
    forAll(memPoolGenerator, transactionSeqGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction]) =>
      val m: ErgoMemPool = mp.put(txs).get
      val m2: ErgoMemPool = m.put(txs).get
      m2.size shouldEqual txs.size
    }
  }

  property("Size of mempool should increase when adding a collection of non-present transactions " +
    "without duplicates (without check)") {
    forAll(memPoolGenerator, transactionSeqGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction]) =>
      val m: ErgoMemPool = mp.putWithoutCheck(txs)
      m.size shouldEqual txs.size
    }
  }

  property("Size of mempool should increase for a number of unique non-present transactions " +
    "when adding a collection of non-present transactions with duplicates (without check)") {
    forAll(memPoolGenerator, transactionSeqGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction]) =>
      val m: ErgoMemPool = mp.putWithoutCheck(txs ++ txs)
      m.size shouldEqual txs.size
    }
  }

  property("Size of mempool should not increase when adding a collection of present transactions (without check)") {
    forAll(memPoolGenerator, transactionSeqGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction]) =>
      val m: ErgoMemPool = mp.putWithoutCheck(txs)
      val m2: ErgoMemPool = m.putWithoutCheck(txs)
      m2.size shouldEqual txs.size
    }
  }

  property("Size of mempool should decrease when removing a present transaction") {
    forAll(memPoolGenerator, transactionSeqGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction]) =>
      val m: ErgoMemPool = mp.put(txs).get
      val m2: ErgoMemPool = m.remove(txs.headOption.get)
      m2.size shouldBe txs.size - 1
    }
  }

  property("Size of mempool should not decrease when removing a non-present transaction") {
    forAll(memPoolGenerator, transactionSeqGenerator, transactionGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction], tx: ErgoTransaction) =>
      val m: ErgoMemPool = mp.put(txs).get
      val m2: ErgoMemPool = m.remove(tx)
      m2.size shouldBe txs.size
    }
  }

  property("Mempool transactions should be filtered successfully") {
    forAll(memPoolGenerator, transactionSeqGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction]) =>
      val m: ErgoMemPool = mp.put(txs).get
      val m2: ErgoMemPool = m.filter(tx => tx equals txs.headOption.get)
      m2.size shouldBe 1
    }
  }

  property("Present transactions should be available by id") {
    forAll(memPoolGenerator, transactionGenerator) { (mp: ErgoMemPool, tx: ErgoTransaction) =>
      val m: ErgoMemPool = mp.put(tx).get
      m.modifierById(tx.id).isDefined shouldBe true
    }
  }

  property("Non-present transactions should not be available by id") {
    forAll(memPoolGenerator, transactionSeqGenerator, transactionGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction], tx: ErgoTransaction) =>
      val m: ErgoMemPool = mp.put(txs).get
      m.modifierById(tx.id).isDefined shouldBe false
    }
  }

  property("Mempool should contain present transactions") {
    forAll(memPoolGenerator, transactionSeqGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction]) =>
      val m: ErgoMemPool = mp.put(txs).get
      m.contains(txs.headOption.get.id) shouldBe true
    }
  }

  property("Mempool should not contain non-present transactions") {
    forAll(memPoolGenerator, transactionSeqGenerator, transactionGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction], tx: ErgoTransaction) =>
      val m: ErgoMemPool = mp.put(txs).get
      m.contains(tx.id) shouldBe false
    }
  }

  property("Present transactions should be obtained by their ids") {
    forAll(memPoolGenerator, transactionSeqGenerator, transactionGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction], tx: ErgoTransaction) =>
      val m: ErgoMemPool = mp.put(txs :+ tx).get
      m.getAll(txs.map(_.id)) sameElements txs
    }
  }

  property("Non-present transactions should not be obtained by their ids") {
    forAll(memPoolGenerator, transactionSeqGenerator, transactionGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction], tx: ErgoTransaction) =>
      val m: ErgoMemPool = mp.put(tx).get
      m.getAll(txs.map(_.id)).size shouldBe 0
    }
  }

  property("Required number of transactions should be taken from mempool") {
    forAll(memPoolGenerator, transactionSeqGenerator, transactionGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction], tx: ErgoTransaction) =>
      val m: ErgoMemPool = mp.put(txs :+ tx).get
      m.take(txs.size).size shouldBe txs.size
    }
  }

  property("Maximum number of transactions that can be taken should equals mempool size") {
    forAll(memPoolGenerator, transactionSeqGenerator) { (mp: ErgoMemPool, txs: Seq[ErgoTransaction]) =>
      val m: ErgoMemPool = mp.put(txs).get
      m.take(txs.size + 1).size shouldBe m.size
    }
  }
}
