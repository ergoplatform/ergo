package org.ergoplatform.nodeView.state

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.utils.{ErgoGenerators, ErgoTestHelpers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.core.VersionTag


class UtxoStateSpecification extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with ErgoTestHelpers {

  property("fromBoxHolder") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.take(1000)._1.foreach { box =>
        us.boxById(box.id) shouldBe Some(box)
      }
    }
  }

  property("fromBoxHolder  + proofsForTransactions") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      val boxes = bh.take(1000)._1
      val tx = AnyoneCanSpendTransaction(boxes.map(_.nonce).toIndexedSeq, IndexedSeq(boxes.map(_.value).sum))
      us.proofsForTransactions(Seq(tx)).isSuccess shouldBe true
    }
  }

  property("proofsForTransactions() to be deterministic") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      val txs = validTransactionsFromBoxHolder(bh)._1

      val (proof1, digest1) = us.proofsForTransactions(txs).get
      val (proof2, digest2) = us.proofsForTransactions(txs).get

      ADProofs.proofDigest(proof1) shouldBe ADProofs.proofDigest(proof2)
      digest1 shouldBe digest2
    }
  }

  property("checkTransactions()") {
    forAll(boxesHolderGen) { bh =>
      val txs = validTransactionsFromBoxHolder(bh)._1

      val boxIds = txs.flatMap(_.boxIdsToOpen)
      boxIds.foreach(id => assert(bh.get(ByteArrayWrapper(id)).isDefined))
      assert(boxIds.distinct.size == boxIds.size)

      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))
      val digest = us.proofsForTransactions(txs).get._2
      us.checkTransactions(txs, digest).isSuccess shouldBe true
    }
  }

  property("applyModifier() - valid full block") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

      val block = validFullBlock(parentOpt = None, us, bh)
      us.applyModifier(block).isSuccess shouldBe true
    }
  }

  property("applyModifier() - 2 valid blocks chain") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

      val block = validFullBlock(parentOpt = None, us, bh)
      val us2 = us.applyModifier(block).get

      val block2 = validFullBlock(parentOpt = Some(block.header), us2, bh)
      val us3 = us2.applyModifier(block2).get

    }
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createUtxoState
      state.applyModifier(b).isFailure shouldBe true
    }
  }

  property("rollback - 1 block back") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))

      val block = validFullBlock(parentOpt = None, us, bh)
      val us2 = us.applyModifier(block).get

      us.rootHash.sameElements(us2.rootHash) shouldBe false

      val us3 = us2.rollbackTo(us.version).get
      us3.rootHash shouldBe us.rootHash
      us3.version shouldBe us.version

      bh.sortedBoxes.take(100).map(_.id).foreach { boxId =>
        us3.boxById(boxId).isDefined shouldBe true
      }

      us3.applyModifier(block).get.rootHash shouldBe us2.rootHash
    }
  }

  property("rollback - n blocks back") {
    forAll(boxesHolderGen, smallInt) { (bh, depth) =>
      whenever(depth > 0 && depth <= 5) {
        val us = createUtxoState(bh)
        bh.sortedBoxes.foreach(box => assert(us.boxById(box.id).isDefined))
        val genesis = validFullBlock(parentOpt = None, us, bh)
        val us2 = us.applyModifier(genesis).get
        us2.rootHash shouldEqual genesis.header.stateRoot


        val (finalState: UtxoState, _) = (0 until depth).foldLeft((us2, genesis)) { (sb, i) =>
          val state = sb._1
          val block = validFullBlock(parentOpt = Some(sb._2.header), state, bh)
          (state.applyModifier(block).get, block)
        }

        finalState.rollbackTo(VersionTag @@ genesis.id).get.rootHash shouldEqual genesis.header.stateRoot
      }
    }
  }
}