package org.ergoplatform.modifiers.history

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.state.StateChanges
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.crypto.authds._
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, BatchAVLVerifier, Insert, Lookup, Operation, Remove}
import scorex.crypto.hash.Digest32
import scorex.util._
import sigmastate.helpers.TestingHelpers._
import org.ergoplatform.settings.Constants.TrueTree

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
    val zeroBox = testBox(0, TrueTree, startHeight, Seq(), Map(), Array.fill(32)(0: Byte).toModifierId)
    prover.performOneOperation(Insert(zeroBox.id, ADValue @@ zeroBox.bytes))
    prover.generateProof()

    val prevDigest = prover.digest
    val boxes = (1 to howMany) map { i => testBox(1, TrueTree, startHeight, boxIndex = i.toShort) }
    boxes.foreach(box => prover.performOneOperation(Insert(box.id, ADValue @@ box.bytes)))
    val pf = prover.generateProof()

    val newDigest = prover.digest
    val operations: IndexedSeq[Insert] = boxes.map(box => Insert(box.id, ADValue @@ box.bytes))
    (operations, prevDigest, newDigest, pf)
  }

  /**
    * Proof for lookups, removals and insertions of boxes on a tree of `treeSize` boxes.
    *
    * @param revealRightmostPath - if set, the proof also reveals the nodes of the path to the rightmost leaf,
    *                            no operation touches them, which the prover leaves as labels otherwise
    */
  private def createMixedEnv(treeSize: Int,
                             lookups: Int,
                             removals: Int,
                             insertions: Int,
                             revealRightmostPath: Boolean = false,
                             seed: Long = 0L): (StateChanges, PrevDigest, NewDigest, Proof) = {
    def box(i: Int): ErgoBox = testBox(1, TrueTree, startHeight, boxIndex = i.toShort)

    val prover = new BatchAVLProver[Digest32, HF](KL, None)
    (1 to treeSize).foreach(i => prover.performOneOperation(insert(box(i))).get)
    prover.generateProof()
    val prevDigest = prover.digest

    val existing = new scala.util.Random(seed).shuffle((1 to treeSize).toVector)
    val absentLookups = (treeSize + insertions + 1) to (treeSize + insertions + lookups / 2)
    val changes = StateChanges(
      toRemove = existing.take(removals).map(i => Remove(box(i).id)),
      toAppend = ((treeSize + 1) to (treeSize + insertions)).map(i => insert(box(i))),
      toLookup = (existing.drop(removals).take(lookups) ++ absentLookups).map(i => Lookup(box(i).id))
    )
    changes.operations.foreach(prover.performOneOperation(_).get)

    if (revealRightmostPath) {
      prover.treeWalk[Unit, Unit](
        (node, _) => {
          if (!node.isNew) node.visited = true
          (node.right, ())
        },
        (leaf, _) => if (!leaf.isNew) leaf.visited = true,
        ()
      )
    }
    val pf = prover.generateProof()
    (changes, prevDigest, prover.digest, pf)
  }

  // whether scrypto's own verifier replays the operations on the proof to the new digest
  private def scryptoAccepts(operations: Seq[Operation],
                             prevDigest: PrevDigest,
                             newDigest: NewDigest,
                             proof: Proof): Boolean = {
    val verifier = new BatchAVLVerifier[Digest32, HF](prevDigest, proof, KL, None,
      maxNumOperations = Some(operations.size))
    operations.forall(verifier.performOneOperation(_).isSuccess) &&
      verifier.digest.exists(digest => java.util.Arrays.equals(digest, newDigest))
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
    val moreInsertions = operations :+ insert(testBox(10, TrueTree, creationHeight = startHeight))
    proof.verify(StateChanges(IndexedSeq.empty, moreInsertions, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are illegal operation") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    val differentInsertions = operations.init :+ insert(testBox(10, TrueTree, creationHeight = startHeight))
    proof.verify(StateChanges(IndexedSeq.empty, differentInsertions, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'failure
  }

  property("verify should be failed if there are operations in different order") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val proof = ADProofs(emptyModifierId, pf)
    val operationsInDifferentOrder = operations.last +: operations.init
    proof.verify(StateChanges(IndexedSeq.empty, operationsInDifferentOrder, IndexedSeq.empty), prevDigest, newDigest) shouldBe 'failure
  }

  // Digest-mode peers must accept the same proofs as utxo-mode peers, which regenerate the proof and compare
  // its hash with header.ADProofsRoot. Otherwise a miner can pad the proof, commit to the hash of the padded
  // one in the header, and split digest-mode peers off from the others.
  property("verify should be failed if there are more proof bytes that needed") {
    val (operations, prevDigest, newDigest, pf) = createEnv()
    val changes = StateChanges(IndexedSeq.empty, operations, IndexedSeq.empty)

    Seq(Array('t'.toByte), Array(0.toByte), Array.fill(100)(1.toByte)).foreach { extraBytes =>
      val proof = ADProofs(emptyModifierId, SerializedAdProof @@ (pf ++ extraBytes))
      val result = proof.verify(changes, prevDigest, newDigest)
      result shouldBe 'failure
      result.failed.get.getMessage should include("bytes, but")
    }
  }

  property("verify should be failed if unused bits after the last direction are set") {
    var checkedPaddingBits = 0
    (1 to 40).foreach { howMany =>
      val (operations, prevDigest, newDigest, pf) = createEnv(howMany)
      val changes = StateChanges(IndexedSeq.empty, operations, IndexedSeq.empty)
      (0 to 7).foreach { bit =>
        val flipped = pf.clone()
        flipped(flipped.length - 1) = (flipped.last ^ (1 << bit)).toByte
        // the bit is unused, if the flip does not change what scrypto's verifier makes of the proof
        if (scryptoAccepts(operations, prevDigest, newDigest, SerializedAdProof @@ flipped)) {
          checkedPaddingBits += 1
          val result = ADProofs(emptyModifierId, SerializedAdProof @@ flipped)
            .verify(changes, prevDigest, newDigest)
          result shouldBe 'failure
          result.failed.get.getMessage should include("unused bits")
        }
      }
    }
    checkedPaddingBits should be > 0
  }

  property("verify should be failed if the proof reveals nodes no operation touches") {
    val (changes, prevDigest, newDigest, canonicalProof) =
      createMixedEnv(treeSize = 200, lookups = 3, removals = 2, insertions = 3, revealRightmostPath = false)
    val (_, _, _, paddedProof) =
      createMixedEnv(treeSize = 200, lookups = 3, removals = 2, insertions = 3, revealRightmostPath = true)

    // the padded proof differs in the packed tree only: it has the same directions
    paddedProof.length should be > canonicalProof.length
    scryptoAccepts(changes.operations, prevDigest, newDigest, paddedProof) shouldBe true

    ADProofs(emptyModifierId, canonicalProof).verify(changes, prevDigest, newDigest) shouldBe 'success
    val result = ADProofs(emptyModifierId, paddedProof).verify(changes, prevDigest, newDigest)
    result shouldBe 'failure
    result.failed.get.getMessage should include("no operation touches")
  }

  property("verify should be success for lookups, removals and insertions of random boxes") {
    forAll(Gen.choose(1, 300), Gen.choose(0, 10), Gen.choose(0, 10), Gen.choose(0, 10), Gen.long) {
      (treeSize, lookups, removals, insertions, seed) =>
        val (changes, prevDigest, newDigest, pf) =
          createMixedEnv(treeSize, lookups, removals, insertions, seed = seed)
        val proof = ADProofs(emptyModifierId, pf)
        proof.verify(changes, prevDigest, newDigest) shouldBe 'success
        ADProofs(emptyModifierId, SerializedAdProof @@ (pf :+ 0.toByte))
          .verify(changes, prevDigest, newDigest) shouldBe 'failure
    }
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
