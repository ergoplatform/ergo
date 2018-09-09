package org.ergoplatform.crypto.pow

import org.bouncycastle.util.BigIntegers
import scorex.crypto.hash.Blake2b256

/**
  * Hash that returns a number from [0,p)
  */
trait SuHash {
  val p: BigInt
  def hash(input: Array[Byte]): BigInt
}

/**
  * TODO ensure correct distribution of outputs
  */
class DefaultSuHash(val p: BigInt) extends SuHash {
  override def hash(input: Array[Byte]): BigInt = BigIntegers.fromUnsignedByteArray(Blake2b256(input)).mod(p)
}
