package org.ergoplatform.settings

import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.generators.CoreObjectGenerators.nonEmptyBytesGen
import org.scalacheck.Gen
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.Leaf
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16

class AlgosSpec extends ErgoCorePropertyTest {

  property("merkleTreeRoot delegates to MerkleTree.rootHash for empty input") {
    Algos.merkleTreeRoot(Seq.empty) shouldBe Algos.merkleTree(Seq.empty).rootHash
  }

  property("merkleTreeRoot agrees with MerkleTree.rootHash for non-empty input") {
    forAll(Gen.nonEmptyListOf(nonEmptyBytesGen)) { bytes =>
      val leaves = bytes.map(LeafData @@ _)
      Algos.merkleTreeRoot(leaves) shouldBe Algos.merkleTree(leaves).rootHash
    }
  }

  property("emptyMerkleTreeRoot equals hash of an empty byte array (genesis extensionRoot)") {
    Base16.encode(Algos.emptyMerkleTreeRoot) shouldBe
      "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
  }

  // The tree↔proof contract is what consensus relies on: any leaf in the tree must
  // produce a proof that validates against `merkleTreeRoot`. Catches future library
  // drift that would silently invalidate proofs in production.
  property("MerkleProof produced from a tree validates against merkleTreeRoot") {
    forAll(Gen.nonEmptyListOf(nonEmptyBytesGen)) { bytes =>
      val leaves = bytes.map(LeafData @@ _)
      val tree = Algos.merkleTree(leaves)
      val root = Algos.merkleTreeRoot(leaves)
      leaves.forall { leaf =>
        tree.proofByElement(Leaf[Digest32](leaf)(Algos.hash)).exists(_.valid(root))
      } shouldBe true
    }
  }

}
