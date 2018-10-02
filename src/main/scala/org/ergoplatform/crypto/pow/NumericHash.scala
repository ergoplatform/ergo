package org.ergoplatform.crypto.pow

import org.bouncycastle.util.BigIntegers
import scorex.core.utils.ScorexEncoding
import scorex.crypto.hash.Blake2b256
import scorex.util.ScorexLogging

import scala.annotation.tailrec

/**
  * One way cryptographic hash function, that produces numbers in [0,p) range.
  * Calculates Blake2b256 hash of a provided input and checks, whether result is
  * in range from 0 to a maximum number divisible by p without remainder.
  * If yes return the result mod p, otherwise make one more iteration using hash as an input.
  * This is done to ensure uniform distribution of the resulting numbers.
  */
class NumericHash(val p: BigInt) extends ScorexLogging with ScorexEncoding {
  assert(p.bigInteger.bitLength() <= 256, "We use 256 bit hash here")
  // biggest number <= 2^256 that is divisible by p without remainder
  val validRange: BigInt = (BigInt(2).pow(256) / p) * p

  @tailrec
  final def hash(input: Array[Byte]): BigInt = {
    val hashed = Blake2b256(input)
    val bi = BigInt(BigIntegers.fromUnsignedByteArray(hashed))
    if (bi < validRange) {
      bi.mod(p)
    } else {
      log.debug(s"Calculate one more hash for ${encoder.encode(input)} and p=$p")
      hash(hashed)
    }
  }
}
