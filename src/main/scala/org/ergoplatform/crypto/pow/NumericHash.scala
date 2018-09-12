package org.ergoplatform.crypto.pow

import org.bouncycastle.util.BigIntegers
import scorex.crypto.hash.Blake2b256

import scala.annotation.tailrec

/**
  * One way cryptographic hash function, that produces numbers in [0,p) range
  */
class NumericHash(val p: BigInt) {
  assert(p.bigInteger.bitLength() <= 256, "We use 256 bit hash here")
  // biggest number < 2^256 that is divisible by p without remainder
  private val validRange: BigInt = (BigInt(2).pow(256) / p) * p

  @tailrec
  final def hash(input: Array[Byte]): BigInt = {
    val hashed = Blake2b256(input)
    val bi = BigInt(BigIntegers.fromUnsignedByteArray(hashed))
    if (bi <= validRange) {
      bi.mod(p)
    } else {
      hash(hashed)
    }
  }
}
