package org.ergoplatform.nodeView.mempool

import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256

class TxFamilyLateParentSpec extends AnyFlatSpec with Matchers with ErgoTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  // unvalidated transaction: one unique external input plus `spend`, `outs` fee outputs
  private def mk(ext: String, spend: Seq[ADKey], outs: Int): UnconfirmedTransaction = {
    val fee = settings.chainSettings.monetary.feeProposition
    val inputs = ((ADKey @@ java.util.Arrays.copyOf(Blake2b256(ext), 32)) +: spend).map(new Input(_, emptyProverResult))
    val outputs = IndexedSeq.tabulate(outs)(i => new ErgoBoxCandidate(1000000L + i, fee, creationHeight = 0))
    UnconfirmedTransaction(ErgoTransaction(inputs.toIndexedSeq, IndexedSeq.empty, outputs), None)
  }

  private val parent = mk("parent", Nil, 2)
  private val child = mk("child", Seq(parent.transaction.outputs.head.id), 1)

  "OrderedTxPool" should "link a child to its pooled parent" in {
    val pool = OrderedTxPool.empty(settings).put(parent, 1000).put(child, 1000)
    pool.family.parentsOf(child.id) shouldBe Set(parent.id)
    pool.family.childrenOf(parent.id) shouldBe Set(child.id)
  }

  it should "link a parent put after its pooled child (rolled-back parent re-entering)" in {
    val pool = OrderedTxPool.empty(settings).put(child, 1000).put(parent, 1000)
    pool.family.parentsOf(child.id) shouldBe Set(parent.id)
    pool.family.childrenOf(parent.id) shouldBe Set(child.id)
  }

  it should "keep the latest put spender in inputs when a child boosts an earlier spender" in {
    val sharedBox = ADKey @@ java.util.Arrays.copyOf(Blake2b256("shared-box"), 32)
    val txA = mk("txA", Seq(sharedBox), 1)
    val txB = mk("txB", Seq(sharedBox), 1)
    val childOfA = mk("childOfA", Seq(txA.transaction.outputs.head.id), 1)

    val pool = OrderedTxPool.empty(settings)
      .put(txA, 1000)
      .put(txB, 1000)
      .put(childOfA, 1000)

    pool.inputs.get(sharedBox) shouldBe Some(txB.id)
  }
}
