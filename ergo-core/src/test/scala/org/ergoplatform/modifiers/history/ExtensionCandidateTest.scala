package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen

class ExtensionCandidateTest extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.CoreObjectGenerators.modifierIdGen

  type KV = (Array[Byte], Array[Byte])

  property("proofFor should return a valid proof for an existing value") {
    forAll { explodedFields: (Seq[KV], KV, Seq[KV]) =>
      val (left, middle, right) = explodedFields
      val fields = left ++ (middle +: right)

      val ext = ExtensionCandidate(fields)
      val proof = ext.proofFor(middle._1.clone)
      proof shouldBe defined
      val nakedLeaf = proof.get.leafData
      val numBytesKey = nakedLeaf.head
      val key = nakedLeaf.tail.take(numBytesKey)
      key shouldBe middle._1
      proof.get.valid(ext.digest) shouldBe true
    }
  }

  property("batchProofFor should return a valid proof for a set of existing values") {
    val modifierIds = Gen.listOf(modifierIdGen)
    forAll(modifierIds) { modifiers =>
      whenever(modifiers.nonEmpty) {

        val fields = NipopowAlgos.packInterlinks(modifiers)
        val ext = ExtensionCandidate(fields)
        val proof = ext.batchProofFor(fields.map(_._1.clone).toArray: _*)
        proof shouldBe defined
        proof.get.valid(ext.interlinksDigest) shouldBe true
      }
    }
  }

  property("batchProofFor should return None for a empty fields") {
    val fields: Seq[KV] = Seq.empty
    val ext = ExtensionCandidate(fields)
    val proof = ext.batchProofFor(fields.map(_._1.clone).toArray: _*)
    proof shouldBe None
  }

  // Locks the on-chain `extensionRoot` of the (empty-extension) genesis block.
  property("digest of an empty extension equals the legacy genesis extensionRoot") {
    ExtensionCandidate(Seq.empty).digest shouldBe Algos.emptyMerkleTreeRoot
  }

  property("digest stays at the legacy genesis value when two empty extensions are combined") {
    (ExtensionCandidate(Seq.empty) ++ ExtensionCandidate(Seq.empty)).digest shouldBe Algos.emptyMerkleTreeRoot
  }

  // A non-genesis extension always contains interlink fields (added by popow), but the
  // interlinksMerkleTree is still computed by filtering for the InterlinksVectorPrefix.
  // If no field has that prefix, the filtered tree is empty — and after #1077 its
  // root hash is the MerkleTree library's empty value, not Algos.emptyMerkleTreeRoot.
  property("interlinksDigest of an extension without interlink fields is the empty MerkleTree root") {
    val nonInterlinkField: KV = (Array[Byte](0x01, 0x02), Array[Byte](0x03))
    val ext = ExtensionCandidate(Seq(nonInterlinkField))
    ext.interlinksDigest should not equal Algos.emptyMerkleTreeRoot
    ext.interlinksDigest shouldBe ext.interlinksMerkleTree.rootHash
  }
}
