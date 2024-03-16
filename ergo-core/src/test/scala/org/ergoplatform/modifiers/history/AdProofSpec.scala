package org.ergoplatform.modifiers.history

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.state.StateChanges
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.crypto.authds._
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert}
import scorex.crypto.hash.Digest32
import scorex.util._
import sigmastate.helpers.TestingHelpers._

class AdProofSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants.startHeight

  val KL = 32

  type Digest = ADDigest
  type Proof = SerializedAdProof

  type PrevDigest = Digest
  type NewDigest = Digest

  val emptyModifierId: ModifierId = bytesToId(Array.fill(32)(0.toByte))

  private def insert(box: ErgoBox) = Insert(box.id, ADValue @@ box.bytes)

  private def createEnv(howMany: Int = 10):
  (IndexedSeq[Insert], PrevDigest, NewDigest, Proof) = {

    val prover = new BatchAVLProver[Digest32, HF](KL, None)
    val zeroBox = testBox(0, Constants.TrueLeaf, startHeight, Seq(), Map(), Array.fill(32)(0: Byte).toModifierId)
    prover.performOneOperation(Insert(zeroBox.id, ADValue @@ zeroBox.bytes))
    prover.generateProof()

    val prevDigest = prover.digest
    val boxes = (1 to howMany) map { i => testBox(1, Constants.TrueLeaf, startHeight, boxIndex = i.toShort) }
    boxes.foreach(box => prover.performOneOperation(Insert(box.id, ADValue @@ box.bytes)))
    val pf = prover.generateProof()

    val newDigest = prover.digest
    val operations: IndexedSeq[Insert] = boxes.map(box => Insert(box.id, ADValue @@ box.bytes))
    (operations, prevDigest, newDigest, pf)
  }

  property("verify should be success in simple case") {
    forAll(Gen.choose(0, 1000)) { s =>
      whenever(s >= 0) {
        val (operations, prevDigest, newDigest, pf) = createEnv(s)
        val proof = ADProofs(emptyModifierId, pf)
        proof.verify(StateChanges(IndexedSeq.empty, operations, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'success
      }
    }
  }

  property("verify should be failed if first operation is missed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    proof.verify(StateChanges(IndexedSeq.empty, operations.tail, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if last operation is missed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    proof.verify(StateChanges(IndexedSeq.empty, operations.init, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are more operations than expected") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    val moreInsertions = operations :+ insert(testBox(10, Constants.TrueLeaf, creationHeight = startHeight))
    proof.verify(StateChanges(IndexedSeq.empty, moreInsertions, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are illegal operation") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    val differentInsertions = operations.init :+ insert(testBox(10, Constants.TrueLeaf, creationHeight = startHeight))
    proof.verify(StateChanges(IndexedSeq.empty, differentInsertions, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are operations in different order") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    val operationsInDifferentOrder = operations.last +: operations.init
    proof.verify(StateChanges(IndexedSeq.empty, operationsInDifferentOrder, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'failure
  }

  //todo: what to do with that?
  ignore("verify should be failed if there are more proof bytes that needed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, SerializedAdProof @@ (pf :+ 't'.toByte))
    proof.verify(StateChanges(IndexedSeq.empty, operations, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are less proof bytes that needed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, SerializedAdProof @@ pf.init)
    proof.verify(StateChanges(IndexedSeq.empty, operations, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are different proof bytes") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    pf.update(4, 6.toByte)
    val proof = ADProofs(emptyModifierId, pf)
    proof.verify(StateChanges(IndexedSeq.empty, operations, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'failure
  }

  property("proof is deterministic") {
    val pf1 = createEnv()._4
    val pf2 = createEnv()._4
    ADProofs.proofDigest(pf1) shouldBe ADProofs.proofDigest(pf2)
  }

}
