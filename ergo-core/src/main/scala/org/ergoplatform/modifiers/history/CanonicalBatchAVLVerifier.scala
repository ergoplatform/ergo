package org.ergoplatform.modifiers.history

import org.ergoplatform.settings.Algos.HF
import scorex.crypto.authds.avltree.batch.{BatchAVLVerifier, InternalNode, InternalVerifierNode, Leaf, Node, Operation}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.utils.Ints

import scala.util.{Failure, Success, Try}

/**
  * `BatchAVLVerifier` which can also tell whether the proof is exactly the one
  * `BatchAVLProver.generateProof` produces for the replayed operations.
  *
  * scrypto's verifier accepts every proof that replays the operations to the expected digest,
  * so the same operations have many accepted proofs: bytes can be appended, the unused high bits of the
  * last byte of directions can be set, and nodes no operation touches can be revealed instead of
  * being left as labels. Each variant hashes differently, and a miner can commit to the hash in
  * `header.ADProofsRoot`. Digest-mode nodes then accept the block, while UTXO-mode nodes regenerate
  * the proof, find another hash and reject it.
  *
  * The form is pinned from what the replay itself reports, without touching scrypto internals:
  *  - `nextDirectionIsLeft` is called once per direction bit that the proof is read for,
  *  - `onNodeVisit` is called for every node of the proof that an operation touches, which are the nodes
  *    the prover reveals, as both run the same code,
  *  - the length of the packed tree is found by walking it in the format `generateProof` writes.
  */
private[history] final class CanonicalBatchAVLVerifier(startingDigest: ADDigest,
                                                       proofBytes: SerializedAdProof,
                                                       numOperations: Int)
  extends BatchAVLVerifier[Digest32, HF](startingDigest, proofBytes, ADProofs.KL, None,
    maxNumOperations = Some(numOperations)) {

  // number of direction bits read from the proof by the operations performed so far
  private var directionBits = 0
  // number of nodes read from the proof which the operations performed so far have touched
  private var touchedProofNodes = 0

  override protected def nextDirectionIsLeft(key: ADKey, r: InternalNode[Digest32]): Boolean = {
    directionBits += 1
    super.nextDirectionIsLeft(key, r)
  }

  override protected def addNode(r: Leaf[Digest32],
                                 key: ADKey,
                                 v: ADValue): InternalVerifierNode[Digest32] = {
    val node = super.addNode(r, key, v)
    // the nodes created by an operation are not in the proof, so a later operation
    // touching them must not be counted
    node.visited = true
    node.left.visited = true
    node.right.visited = true
    node
  }

  override protected def onNodeVisit(n: Node[Digest32],
                                     operation: Operation,
                                     isRotate: Boolean): Unit = {
    if (!n.visited) touchedProofNodes += 1
    super.onNodeVisit(n, operation, isRotate)
  }

  /**
    * To be called once all the operations are performed successfully, and the digest is the expected one.
    *
    * @return Success if the proof is exactly the one the prover generates for these operations
    */
  def checkCanonicalForm(): Try[Unit] = {
    val (packedTreeLength, proofNodes) = walkPackedTree()
    val directionBytes = (directionBits + 7) / 8
    val expectedLength = packedTreeLength + directionBytes
    // bits of the last direction byte past the last direction bit
    val unusedBitsMask = (0xff << (directionBits & 7)) & 0xff

    if (proofBytes.length != expectedLength) {
      val msg = s"ADProof has ${proofBytes.length} bytes, but ${expectedLength} are needed"
      Failure(new IllegalArgumentException(msg))
    } else if ((directionBits & 7) != 0 && (proofBytes.last & unusedBitsMask) != 0) {
      Failure(new IllegalArgumentException("ADProof has unused bits set after the last direction"))
    } else if (proofNodes != touchedProofNodes) {
      val msg = s"ADProof reveals ${proofNodes - touchedProofNodes} node(s) no operation touches"
      Failure(new IllegalArgumentException(msg))
    } else {
      Success(())
    }
  }

  /**
    * Walks the packed tree at the start of the proof the way the verifier does when it reads it, see
    * `BatchAVLVerifier.reconstructedTree`.
    *
    * @return length of the packed tree, including the end of tree marker, and the number of nodes
    *         the proof reveals, that is, of its leaves and internal nodes without the label-only ones
    */
  private def walkPackedTree(): (Int, Int) = {
    var i = 0
    var revealedNodes = 0
    // the key of a leaf following a leaf is not written, as it is the next leaf key of the previous one
    var previousIsLeaf = false
    while (proofBytes(i) != EndOfTreeInPackagedProof) {
      val tag = proofBytes(i)
      i += 1
      tag match {
        case LabelInPackagedProof =>
          i += labelLength
          previousIsLeaf = false
        case LeafInPackagedProof =>
          if (!previousIsLeaf) i += keyLength
          i += keyLength
          val valueLength = Ints.fromByteArray(proofBytes.slice(i, i + 4))
          i += 4 + valueLength
          revealedNodes += 1
          previousIsLeaf = true
        case _ => // balance of an internal node
          revealedNodes += 1
      }
    }
    (i + 1, revealedNodes)
  }

}
