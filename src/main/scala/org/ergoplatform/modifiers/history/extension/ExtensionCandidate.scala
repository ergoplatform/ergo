package org.ergoplatform.modifiers.history.extension

import org.ergoplatform.settings.Algos
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.{BatchMerkleProof, Leaf, MerkleProof, MerkleTree}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId

import scala.collection.mutable
/**
  * Extension block section with header id not provided
  *
  * Useful when a header is not formed yet
  *
  * @param fields - key-value data extension section holds.
  *               Keys must be of 2 bytes length, they must be unique.
  *               Values must be no more than 64 bytes long.
  *               Data must be 32,768 bytes max.
  */
class ExtensionCandidate(val fields: Seq[(Array[Byte], Array[Byte])]) {
  lazy val merkleTree: MerkleTree[Digest32] = Extension.merkleTree(fields)

  lazy val digest: Digest32 = Algos.merkleTreeRoot(merkleTree)

  def toExtension(headerId: ModifierId): Extension = Extension(headerId, fields)

  def ++(that: ExtensionCandidate): ExtensionCandidate = ExtensionCandidate(fields ++ that.fields)

  def proofFor(key: Array[Byte]): Option[MerkleProof[Digest32]] =
    fields.find(_._1 sameElements key)
      .map(Extension.kvToLeaf)
      .flatMap(kv => merkleTree.proofByElement(Leaf[Digest32](LeafData @@ kv)(Algos.hash)))

  def batchProofFor(keys: Array[Byte]*): Option[BatchMerkleProof[Digest32]] = {
    val indices = keys
        .map(key => fields.find(_._1 sameElements key)
        .map(Extension.kvToLeaf)
        .map(kv => Leaf[Digest32](LeafData @@ kv)(Algos.hash).hash)
        .flatMap(leafData => merkleTree.elementsHashIndex.get(new mutable.WrappedArray.ofByte(leafData)))
      ).toSeq.flatten
    merkleTree.proofByIndices(indices)(Algos.hash)
  }
}

object ExtensionCandidate {
  def apply(fields: Seq[(Array[Byte], Array[Byte])]): ExtensionCandidate = new ExtensionCandidate(fields)
}
