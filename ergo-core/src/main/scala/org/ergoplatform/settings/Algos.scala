package org.ergoplatform.settings

import org.ergoplatform.utils
import org.ergoplatform.utils.ScorexEncoder
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.Digest32
import scorex.util._


object Algos extends ErgoAlgos with utils.ScorexEncoding {

  // ErgoAlgos in sigmastate extends scorex.util.ScorexEncoding where encoder is BytesEncoder
  // but here we use scorex.core.utils.ScorexEncoding where encoder is ScorexEncoder
  // After ScorexEncoder is moved (there is even a todo for that) from scorex.core to scorex.util
  //  we can fix this ugliness.
  override implicit val encoder: ScorexEncoder = utils.ScorexEncoder.default

  /**
    * Hash of an empty byte array. Locked in as the on-chain `extensionRoot` of the
    * mainnet genesis block, whose extension is empty; preserved at the one call site
    * (`ExtensionCandidate.digest`) so genesis consensus is not changed.
    */
  lazy val emptyMerkleTreeRoot: Digest32 = Algos.hash(LeafData @@ Array[Byte]())

  @inline def encode(id: ModifierId): String = encoder.encode(id)

  /**
    * Build a Merkle tree over binary objects (leaves of the tree).
    */
  def merkleTree(elements: Seq[LeafData]): MerkleTree[Digest32] = MerkleTree(elements)(hash)

  /**
    * Root hash of the Merkle tree built over the given leaves.
    */
  def merkleTreeRoot(elements: Seq[LeafData]): Digest32 = merkleTree(elements).rootHash

}
