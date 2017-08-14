package org.ergoplatform.settings

import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.Blake2b256

object Algos {

  def blockIdDifficulty(id: Array[Byte]): BigInt = {
    val blockTarget = BigInt(1, id)
    assert(blockTarget <= Constants.MaxTarget, s"Block $blockTarget target is bigger than max ${Constants.MaxTarget}")
    Constants.MaxTarget / blockTarget
  }

  val hash = Blake2b256
  //TODO replace to concrete mining algorithm
  val miningHash = Blake2b256

  val initialDifficulty = 1

  def merkleTreeRoot(elements: Seq[Array[Byte]]): Array[Byte] =
    if (elements.isEmpty) emptyMerkleTreeRoot else MerkleTree(elements)(hash).rootHash

  lazy val emptyMerkleTreeRoot: Array[Byte] = Algos.hash(Array[Byte]())
}
