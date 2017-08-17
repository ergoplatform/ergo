package org.ergoplatform.nodeView.state

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.utils.{ErgoGenerators, ErgoTestHelpers}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}


class UtxoStateSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with ErgoTestHelpers {

  property("fromBoxHolder") {
    forAll(boxesHolderGen) { bh =>
      withDir(s"/tmp/utxotest-${bh.hashCode()}") { dir =>
        val us = UtxoState.fromBoxHolder(bh, dir)
        bh.take(1000)._1.foreach { box =>
          us.boxById(box.id) shouldBe Some(box)
        }
      }
    }
  }

  property("fromBoxHolder  + proofsForTransactions") {
    forAll(boxesHolderGen) { bh =>
      withDir(s"/tmp/utxotest-${bh.hashCode()}") { dir =>
        val us = UtxoState.fromBoxHolder(bh, dir)
        val boxes = bh.take(1000)._1
        val tx = AnyoneCanSpendTransaction(boxes.map(_.nonce).toIndexedSeq, IndexedSeq(boxes.map(_.value).sum))
        us.proofsForTransactions(Seq(tx)).isSuccess shouldBe true
      }
    }
  }

  property("checkTransactions()") {
    val bhGen = boxesHolderGen

    forAll(bhGen) { bh =>
      val txs = validTransactions(bh)._1

      val boxIds = txs.flatMap(_.boxIdsToOpen)
      boxIds.foreach(id => assert(bh.get(ByteArrayWrapper(id)).isDefined))
      assert(boxIds.distinct.size == boxIds.size)

      withDir(s"/tmp/utxotest-${bh.hashCode()}-${txs.hashCode()}") { dir =>
        val us = UtxoState.fromBoxHolder(bh, dir)
        bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))
        val digest = us.proofsForTransactions(txs).get._2
        us.checkTransactions(txs, digest).isSuccess shouldBe true
      }
    }
  }

  property("validate() - valid block") {

  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      withDir("/tmp/utxotest3") { dir =>
        val state = new UtxoState(dir)
        state.validate(b).isFailure shouldBe true
      }
    }
  }

  property("applyModifier() - valid block") {
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      withDir("/tmp/utxotest5") { dir =>
        val state = new UtxoState(dir)
        state.applyModifier(b).isFailure shouldBe true
      }
    }
  }
}