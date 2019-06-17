package org.ergoplatform.settings

import io.iohk.iodb.ByteArrayWrapper
import scorex.core.utils.ScorexEncoding
import scorex.core.{VersionTag, versionToBytes}
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util._

import scala.util.Try

object Algos extends ScorexEncoding {

  type HF = Blake2b256.type

  val hash: HF = Blake2b256

  lazy val emptyMerkleTreeRoot: Digest32 = Algos.hash(LeafData @@ Array[Byte]())

  @inline def versionToBAW(id: VersionTag): ByteArrayWrapper = ByteArrayWrapper(versionToBytes(id))

  @inline def idToBAW(id: ModifierId): ByteArrayWrapper = ByteArrayWrapper(idToBytes(id))

  @inline def encode(id: ModifierId): String = encoder.encode(id)

  @inline def encode(bytes: Array[Byte]): String = encoder.encode(bytes)

  @inline def decode(str: String): Try[Array[Byte]] = encoder.decode(str)

  @inline def decodeUnsafe(str: String): Array[Byte] = decode(str).get

  def blockIdDifficulty(id: Array[Byte]): BigInt = {
    val blockTarget = BigInt(1, id).ensuring(_ <= Constants.MaxTarget)
    Constants.MaxTarget / blockTarget
  }

  def merkleTreeRoot(elements: Seq[LeafData]): Digest32 =
    if (elements.isEmpty) emptyMerkleTreeRoot else MerkleTree(elements)(hash).rootHash

  /**
    * Add longs, returning Long.Max value if there was any long overflow
    */
  @inline def addExact(a: Long, b: Long): Long = {
    val sum = a + b
    if (sum < 0) Long.MaxValue else sum
  }

  @inline def addExact(a: Long, b: Long, c: Long): Long = addExact(addExact(a, b), c)


  /**
    * Multiply longs, returning Long.Max value if there was any long overflow
    */
  @inline def multiplyExact(e1: Long, e2: Long): Long = {
    try {
      Math.multiplyExact(e1, e2)
    } catch {
      case _: Throwable => Long.MaxValue
    }
  }

}
