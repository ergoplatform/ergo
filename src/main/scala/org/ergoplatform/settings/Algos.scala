package org.ergoplatform.settings

import io.iohk.iodb.ByteArrayWrapper
import scorex.core._
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.util.Try

object Algos extends ScorexEncoding {

  type HF = Blake2b256.type

  def versionToBAW(id: VersionTag): ByteArrayWrapper = ByteArrayWrapper(versionToBytes(id))

  def idToBAW(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(idToBytes(id))

  // TODO may be redundant if id is already in encoded form
  @inline
  def encode(id: ModifierId): String = encoder.encode(id)

  def encode(bytes: Array[Byte]): String = encoder.encode(bytes)

  def decode(str: String): Try[Array[Byte]] = encoder.decode(str)

  def blockIdDifficulty(id: Array[Byte]): BigInt = {
    val blockTarget = BigInt(1, id).ensuring(_ <= Constants.MaxTarget)
    Constants.MaxTarget / blockTarget
  }

  val hash: HF = Blake2b256

  val initialDifficulty = 1

  def merkleTreeRoot(elements: Seq[LeafData]): Digest32 =
    if (elements.isEmpty) emptyMerkleTreeRoot else MerkleTree(elements)(hash).rootHash

  lazy val emptyMerkleTreeRoot: Digest32 = Algos.hash(LeafData @@ Array[Byte]())
}
