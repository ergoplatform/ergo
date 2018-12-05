package org.ergoplatform.mining

import org.bouncycastle.util.BigIntegers
import scorex.crypto.hash.{Blake2b256, Blake2b512}
import scorex.util.{ScorexEncoding, ScorexLogging}

import scala.annotation.tailrec

/**
  * One way cryptographic hash function, that produces numbers in [0,q) range.
  * Calculates Blake2b256 hash of a provided input and checks, whether result is
  * in range from 0 to a maximum number divisible by q without remainder.
  * If yes return the result mod q, otherwise make one more iteration using hash as an input.
  * This is done to ensure uniform distribution of the resulting numbers.
  */
class NumericHash(val q: BigInt) extends ScorexLogging with ScorexEncoding {
  assert(q.bigInteger.bitLength() <= 256, "We use 256 bit hash here")
  // biggest number <= 2^256 that is divisible by p without remainder
  val validRange: BigInt = (BigInt(2).pow(256) / q) * q

  @tailrec
  final def hash(input: Array[Byte]): BigInt = {
    val hashed = Blake2b256(input)
    val bi = BigInt(BigIntegers.fromUnsignedByteArray(hashed))
    if (bi < validRange) {
      bi.mod(q)
    } else {
      log.trace(s"Calculate one more hash for ${encoder.encode(input)} and q=$q")
      hash(hashed)
    }
  }
}
