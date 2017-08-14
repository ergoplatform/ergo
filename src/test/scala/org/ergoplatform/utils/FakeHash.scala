package org.ergoplatform.utils

import com.google.common.primitives.Ints
import scorex.crypto.hash.CryptographicHash32

import scala.util.Random

/**
  * Hash-like function which generates a random(and deterministic) 256-bits value which starts with defined
  * number of zero bits
  *
  * This is not a cryptographic hash function!
  *
  * @param minZeroBits - number of zero bits
  */
//todo: do we need this at all?
class FakeHash(minZeroBits: Int) extends CryptographicHash32 {
  override def hash(input: Message): Digest = {
    val seedBytesUnsafe = if(input.length <= 4) input else input.take(4)
    val seedBytes = Array.fill(4 - seedBytesUnsafe.length)(0 : Byte) ++ seedBytesUnsafe

    val seed = Ints.fromByteArray(seedBytes)
    val data = BigInt(256 - minZeroBits, new Random(seed)).toByteArray
    val zeroBytesCnt = 32 - data.length
    Array.fill(zeroBytesCnt)(0:Byte) ++ data
  }
}