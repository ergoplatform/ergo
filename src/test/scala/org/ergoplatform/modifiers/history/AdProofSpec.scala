package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.core.ModifierId
import scorex.core.transaction.state.{BoxStateChanges, Insertion}
import scorex.crypto.authds._
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert}
import scorex.crypto.hash.Digest32

class AdProofSpec extends ErgoPropertyTest {
  val KL = 32

  type Digest = ADDigest
  type Proof = SerializedAdProof

  type PrevDigest = Digest
  type NewDigest = Digest

  val emptyModifierId = ModifierId @@ Array.fill(32)(0.toByte)

  private def createEnv(howMany: Int = 10):
  (Seq[Insertion[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox]], PrevDigest, NewDigest, Proof) = {

    val prover = new BatchAVLProver[Digest32, HF](KL, Some(ErgoState.BoxSize))
    val zeroBox = AnyoneCanSpendNoncedBox(0, 0L)
    prover.performOneOperation(Insert(zeroBox.id, zeroBox.bytes))
    prover.generateProof()

    val prevDigest = prover.digest
    val boxes = (1 to howMany) map { i => AnyoneCanSpendNoncedBox(i, 1L) }
    boxes.foreach(box => prover.performOneOperation(Insert(box.id, box.bytes)))
    val pf = prover.generateProof()

    val newDigest = prover.digest
    val operations: Seq[Insertion[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox]] =
      boxes.map(box => Insertion[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](box))
    (operations, prevDigest, newDigest, pf)
  }

  property("verify should be success in simple case") {
    forAll(Gen.choose(0, 1000)) { s =>
      whenever(s >= 0) {
        val (operations, prevDigest, newDigest, pf) = createEnv(s)
        val proof = ADProofs(emptyModifierId, pf)
        proof.verify(BoxStateChanges(operations), prevDigest, newDigest) shouldBe 'success
      }
    }
  }

  property("verify should be failed if first operation is missed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    proof.verify(BoxStateChanges(operations.tail), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if last operation is missed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    proof.verify(BoxStateChanges(operations.init), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are more operations than expected") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    val moreInsertions = operations :+
      Insertion[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](AnyoneCanSpendNoncedBox(11L, 1L))
    proof.verify(BoxStateChanges(moreInsertions), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are illegal operation") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    val differentInsertions = operations.init :+
      Insertion[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](AnyoneCanSpendNoncedBox(11L, 1L))
    proof.verify(BoxStateChanges(differentInsertions), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are operations in different order") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    val operationsInDifferentOrder = operations.last +: operations.init
    proof.verify(BoxStateChanges(operationsInDifferentOrder), prevDigest, newDigest) shouldBe 'failure
  }

  //todo: what to do with that?
  ignore("verify should be failed if there are more proof bytes that needed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, SerializedAdProof @@ (pf :+ 't'.toByte))
    proof.verify(BoxStateChanges(operations), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are less proof bytes that needed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, SerializedAdProof @@ pf.init)
    proof.verify(BoxStateChanges(operations), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are different proof bytes") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    pf.update(4, 6.toByte)
    val proof = ADProofs(emptyModifierId, pf)
    proof.verify(BoxStateChanges(operations), prevDigest, newDigest) shouldBe 'failure
  }

  property("proof is deterministic") {
    val (operations1, prevDigest1, newDigest1, pf1) = createEnv()
    val (operations2, prevDigest2, newDigest2, pf2) = createEnv()

    ADProofs.proofDigest(pf1) shouldBe ADProofs.proofDigest(pf2)
  }
}
