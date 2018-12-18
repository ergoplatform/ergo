package org.ergoplatform.modifiers.history

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.state.{Insertion, StateChanges}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.crypto.authds._
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert}
import scorex.crypto.hash.Digest32
import scorex.util._
import sigmastate.Values

class AdProofSpec extends ErgoPropertyTest {
  val KL = 32

  type Digest = ADDigest
  type Proof = SerializedAdProof

  type PrevDigest = Digest
  type NewDigest = Digest

  val emptyModifierId: ModifierId = bytesToId(Array.fill(32)(0.toByte))

  private def createEnv(howMany: Int = 10):
  (Seq[Insertion], PrevDigest, NewDigest, Proof) = {

    val prover = new BatchAVLProver[Digest32, HF](KL, None)
    val zeroBox = ErgoBox(0, Values.TrueLeaf, startHeight, Seq(), Map(), Array.fill(32)(0: Byte).toModifierId)
    prover.performOneOperation(Insert(zeroBox.id, ADValue @@ zeroBox.bytes))
    prover.generateProof()

    val prevDigest = prover.digest
    val boxes = (1 to howMany) map { i => ErgoBox(1, Values.TrueLeaf, boxId = i.toShort, creationHeight = startHeight) }
    boxes.foreach(box => prover.performOneOperation(Insert(box.id, ADValue @@ box.bytes)))
    val pf = prover.generateProof()

    val newDigest = prover.digest
    val operations: Seq[Insertion] = boxes.map(box => Insertion(box))
    (operations, prevDigest, newDigest, pf)
  }

  property("verify should be success in simple case") {
    forAll(Gen.choose(0, 1000)) { s =>
      whenever(s >= 0) {
        val (operations, prevDigest, newDigest, pf) = createEnv(s)
        val proof = ADProofs(emptyModifierId, pf)
        proof.verify(StateChanges(Seq(), operations), prevDigest, newDigest) shouldBe 'success
      }
    }
  }

  property("verify should be failed if first operation is missed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    proof.verify(StateChanges(Seq(), operations.tail), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if last operation is missed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    proof.verify(StateChanges(Seq(), operations.init), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are more operations than expected") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    val moreInsertions = operations :+ Insertion(ErgoBox(10, Values.TrueLeaf, creationHeight = startHeight))
    proof.verify(StateChanges(Seq(), moreInsertions), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are illegal operation") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    val differentInsertions = operations.init :+ Insertion(ErgoBox(10, Values.TrueLeaf, creationHeight = startHeight))
    proof.verify(StateChanges(Seq(), differentInsertions), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are operations in different order") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    val operationsInDifferentOrder = operations.last +: operations.init
    proof.verify(StateChanges(Seq(), operationsInDifferentOrder), prevDigest, newDigest) shouldBe 'failure
  }

  //todo: what to do with that?
  ignore("verify should be failed if there are more proof bytes that needed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, SerializedAdProof @@ (pf :+ 't'.toByte))
    proof.verify(StateChanges(Seq(), operations), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are less proof bytes that needed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, SerializedAdProof @@ pf.init)
    proof.verify(StateChanges(Seq(), operations), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are different proof bytes") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    pf.update(4, 6.toByte)
    val proof = ADProofs(emptyModifierId, pf)
    proof.verify(StateChanges(Seq(), operations), prevDigest, newDigest) shouldBe 'failure
  }

  property("proof is deterministic") {
    val pf1 = createEnv()._4
    val pf2 = createEnv()._4
    ADProofs.proofDigest(pf1) shouldBe ADProofs.proofDigest(pf2)
  }

}
