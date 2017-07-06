package org.ergoplatform.settings

import scorex.core.block.Block._
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.{Blake2b256, CommutativeHash}

object Algos {

  def blockIdDifficulty(id: Array[Version]): BigInt = {
    val blockTarget = BigInt(1, id)
    Constants.MaxTarget / blockTarget
  }

  val hash = new CommutativeHash(Blake2b256)
  //TODO replace to concrete mining algorithm
  val miningHash = Blake2b256

  def merkleTreeRoot(elements: Seq[Array[Byte]]): Array[Byte] = if (elements.isEmpty) EmptyMerkleTreeRoot
  else MerkleTree(elements)(hash).rootHash

  lazy val EmptyMerkleTreeRoot: Array[Byte] = Algos.hash(Array[Byte]())

}
