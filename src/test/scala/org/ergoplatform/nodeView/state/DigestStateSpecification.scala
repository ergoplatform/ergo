package org.ergoplatform.nodeView.state

import org.ergoplatform.{DataInput, Input}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core._
import scorex.crypto.authds.ADDigest
import sigmastate.interpreter.ProverResult

import scala.util.{Random, Try}

class DigestStateSpecification extends ErgoPropertyTest {

  private val emptyVersion: VersionTag = bytesToVersion(Array.fill(32)(0: Byte))
  private val emptyAdDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)

  property("reopen") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val fb = validFullBlock(parentOpt = None, us, bh)
      val dir2 = createTempDir
      val ds = DigestState.create(Some(us.version), Some(us.rootHash), dir2, stateConstants)
      ds.applyModifier(fb).isSuccess shouldBe true
      ds.close()

      val state = DigestState.create(None, None, dir2, stateConstants)
      state.version shouldEqual fb.header.id
      state.rootHash shouldEqual fb.header.stateRoot
    }
  }

  property("validate() - valid block") {
    var (us, bh) = createUtxoState()
    var ds = createDigestState(us.version, us.rootHash)
    var parentOpt: Option[ErgoFullBlock] = None

    forAll { seed: Int =>
      val blBh = validFullBlockWithBoxHolder(parentOpt, us, bh, new Random(seed))
      val block = blBh._1
      bh = blBh._2
      ds = ds.applyModifier(block).get
      us = us.applyModifier(block).get
      parentOpt = Some(block)
    }
  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createDigestState(emptyVersion, emptyAdDigest)
      state.validate(b).isFailure shouldBe true
    }
  }

  property("applyModifier() - valid block") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val block = validFullBlock(parentOpt = None, us, bh)
      block.blockTransactions.transactions.exists(_.dataInputs.nonEmpty) shouldBe true

      val ds = createDigestState(us.version, us.rootHash)
      ds.applyModifier(block) shouldBe 'success
    }
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createDigestState(emptyVersion, emptyAdDigest)
      state.applyModifier(b).isFailure shouldBe true
    }
  }

  property("rollback & rollback versions") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val block = validFullBlock(parentOpt = None, us, bh)

      val ds = createDigestState(us.version, us.rootHash)

      ds.rollbackVersions.size shouldEqual 1

      val ds2 = ds.applyModifier(block).get

      ds2.rollbackVersions.size shouldEqual 2

      ds2.stateContext.lastHeaders.size shouldEqual 1

      java.util.Arrays.equals(ds2.rootHash, ds.rootHash) shouldBe false

      val ds3 = ds2.rollbackTo(ds.version).get
      ds3.rootHash shouldBe ds.rootHash

      ds3.stateContext.lastHeaders.size shouldEqual 0

      ds3.applyModifier(block).get.rootHash shouldBe ds2.rootHash
    }
  }

  property("validateTransactions() - dataInputs") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      val ds = createDigestState(us.version, us.rootHash)

      // generate 2 independent transactions spending state boxes only
      val headTx = validTransactionsFromBoxes(1, bh.boxes.take(10).values.toSeq, new Random())._1.head
      val nextTx = validTransactionsFromBoxes(1, bh.boxes.takeRight(10).values.toSeq, new Random())._1.head
      headTx.inputs.intersect(nextTx.inputs) shouldBe empty

      // trying to apply transactions with data inputs same as inputs of the next tx
      val dataInputs = nextTx.inputs.filter(i => us.boxById(i.boxId).isDefined).map(i => DataInput(i.boxId))
      val inputs = headTx.outputs.map(b =>  Input(b.id, ProverResult.empty))
      val txWithDataInputs = ErgoTransaction(inputs, dataInputs, headTx.outputCandidates)

      val txs1 = IndexedSeq(headTx, nextTx, txWithDataInputs)
      val (proofBytes1, digest1) = us.proofsForTransactions(txs1).get
      val proof1 = ADProofs(defaultHeaderGen.sample.get.id, proofBytes1)
      Try(ds.validateTransactions(txs1, digest1, proof1, emptyStateContext)) shouldBe 'success

      val txs2 = IndexedSeq(headTx, txWithDataInputs, nextTx)
      val (proofBytes2, digest2) = us.proofsForTransactions(txs2).get
      val proof2 = ADProofs(defaultHeaderGen.sample.get.id, proofBytes2)
      Try(ds.validateTransactions(txs2, digest2, proof2, emptyStateContext)) shouldBe 'success

      val txs3 = IndexedSeq(txWithDataInputs, headTx, nextTx)
      Try(ds.validateTransactions(txs3, digest2, proof2, emptyStateContext)) shouldBe 'failure
    }
  }

}
