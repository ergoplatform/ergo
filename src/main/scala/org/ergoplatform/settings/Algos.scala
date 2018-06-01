package org.ergoplatform.settings

import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.encode.Base16
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.util.Try

object Algos {

  type HF = Blake2b256.type

  def encode(bytes: Array[Byte]): String = encoder.encode(bytes)

  //TODO: move this to [[Base16]] class
  def encode(bytes: Seq[Byte]): String = bytes.map("%02x".format(_)).mkString

  def decode(str: String): Try[Array[Byte]] = encoder.decode(str)

  def blockIdDifficulty(id: Array[Byte]): BigInt = {
    val blockTarget = BigInt(1, id).ensuring(_ <= Constants.MaxTarget)
    Constants.MaxTarget / blockTarget
  }

  val hash: HF = Blake2b256
  val encoder = Base16

  val initialDifficulty = 1

  def merkleTreeRoot(elements: Seq[LeafData]): Digest32 =
    if (elements.isEmpty) emptyMerkleTreeRoot else MerkleTree(elements)(hash).rootHash

  lazy val emptyMerkleTreeRoot: Digest32 = Algos.hash(LeafData @@ Array[Byte]())
}
