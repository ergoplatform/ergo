package org.ergoplatform.nodeView.state

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.ADProof
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.utils.{ErgoGenerators, ErgoTestHelpers}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.Blake2b256Unsafe


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

  property("proofsForTransactions() to be deterministic") {
    forAll(boxesHolderGen) { bh =>
      withDir(s"/tmp/utxotest-${bh.hashCode()}") { dir =>
        val us = UtxoState.fromBoxHolder(bh, dir)
        val txs = validTransactions(bh)._1

        val (proof1, digest1) = us.proofsForTransactions(txs).get

        val (proof2, digest2) = us.proofsForTransactions(txs).get

        ADProof.proofDigest(proof1) shouldBe ADProof.proofDigest(proof2)
        digest1 shouldBe digest2
      }
    }
  }

  property("checkTransactions()") {
    forAll(boxesHolderGen) { bh =>
      withDir(s"/tmp/utxotest-${bh.hashCode()}") { dir =>
        val txs = validTransactions(bh)._1

        val boxIds = txs.flatMap(_.boxIdsToOpen)
        boxIds.foreach(id => assert(bh.get(ByteArrayWrapper(id)).isDefined))
        assert(boxIds.distinct.size == boxIds.size)

        val us = UtxoState.fromBoxHolder(bh, dir)
        bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))
        val digest = us.proofsForTransactions(txs).get._2
        us.checkTransactions(txs, digest).isSuccess shouldBe true
      }
    }
  }

  property("validate() - valid block after genesis") {

  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      withDir("/tmp/utxotest3") { dir =>
        val state = new UtxoState(dir)
        state.validate(b).isFailure shouldBe true
      }
    }
  }

  property("applyModifier() - valid full block") {
    forAll(boxesHolderGen) { bh =>
      withDir(s"/tmp/utxotest-${bh.hashCode()}}") { dir =>

        val us = UtxoState.fromBoxHolder(bh, dir)
        bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

        val parent = ErgoFullBlock.genesisWithStateDigest(us.rootHash).header
        val block = validFullBlock(parent, us, bh)
        assert(us.rootHash.sameElements(parent.stateRoot))
        us.applyModifier(block).isSuccess shouldBe true
      }
    }
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