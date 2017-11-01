package org.ergoplatform.settings

import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.util.Try

object Algos {

  def encode(bytes: Array[Byte]): String = encoder.encode(bytes)

  def decode(str: String): Try[Array[Byte]] = encoder.decode(str)

  def blockIdDifficulty(id: Array[Byte]): BigInt = {
    val blockTarget = BigInt(1, id)
    require(blockTarget <= Constants.MaxTarget, s"Block $blockTarget target is bigger than max ${Constants.MaxTarget}")
    Constants.MaxTarget / blockTarget
  }

  val hash = Blake2b256
  val encoder = Base58

  val initialDifficulty = 1

  def merkleTreeRoot(elements: Seq[LeafData]): Digest32 =
    if (elements.isEmpty) emptyMerkleTreeRoot else MerkleTree(elements)(hash).rootHash

  lazy val emptyMerkleTreeRoot: Digest32 = Algos.hash(LeafData @@ Array[Byte]())
}
