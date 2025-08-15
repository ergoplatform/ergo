package org.ergoplatform.settings

import org.ergoplatform.utils
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.Digest32
import scorex.util.encode.BytesEncoder


object Algos extends ErgoAlgos with utils.ScorexEncoding {

  override implicit val encoder: BytesEncoder = utils.ScorexEncoder

  lazy val emptyMerkleTreeRoot: Digest32 = Algos.hash(LeafData @@ Array[Byte]())

  /**
    * This method might be useful and reimplemented, if encoding of ModifierId and VersionTag
    * is different form default bytes encoding, e.g. this method should be reimplemented together
    * with encode() and decode methods
    */
  @inline def encode(id: String): String = id

  /**
    * A method to build a Merkle tree over binary objects (leafs of the tree)
    * @param elements - Merkle tree leafs (byte arrays of arbitrary size)
    * @return a Merkle tree built over the elements
    */
  def merkleTree(elements: Seq[LeafData]): MerkleTree[Digest32] = MerkleTree(elements)(hash)

  /**
    * A method which is building a Merkle tree over binary objects and returns a digest
    * (256-bits long root hash) of the tree
    *
    * !!! If input sequence is empty, then the function returns a special value (hash of empty byte array), which is
    *  equal to 0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8 and different from "rootHash" property
    *  of a Merkle tree instance (merkleTree(elements).rootHash) which is equal to another special value
    *  0000000000000000000000000000000000000000000000000000000000000000
    *
    *  See https://github.com/ergoplatform/ergo/issues/1077
    *
    * @param elements - tree leafs
    * @return 256-bits (32-bytes) long digest of the tree
    */
  def merkleTreeRoot(elements: Seq[LeafData]): Digest32 =
    if (elements.isEmpty) emptyMerkleTreeRoot else merkleTree(elements).rootHash

  def merkleTreeRoot(tree: MerkleTree[Digest32]): Digest32 =
    if (tree.length == 0) emptyMerkleTreeRoot else tree.rootHash

}
