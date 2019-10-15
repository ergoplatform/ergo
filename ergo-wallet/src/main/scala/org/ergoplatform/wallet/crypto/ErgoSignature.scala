package org.ergoplatform.wallet.crypto

import org.bouncycastle.util.BigIntegers
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigmastate.interpreter.CryptoConstants
import sigmastate.serialization.GroupElementSerializer

/**
  * Schnorr signature scheme implementation.
  */
object ErgoSignature {

  import CryptoConstants._

  /**
    * Signs given `msg` using given `sk`.
    *
    * @return signature bytes
    */
  def sign(msg: Array[Byte], sk: BigInt): Array[Byte] = {
    val y = genSecret
    val pk = dlogGroup.exponentiate(dlogGroup.generator, sk.bigInteger)
    val w = dlogGroup.exponentiate(dlogGroup.generator, y.bigInteger)
    val s = genCommitment(pk, w) ++ msg
    val c = BigInt(BigIntegers.fromUnsignedByteArray(hf(s)))
    val z = (sk * c + y) % groupOrder
    BigIntegers.asUnsignedByteArray(24, c.bigInteger) ++ BigIntegers.asUnsignedByteArray(32, z.bigInteger)
  }

  /**
    * Checks whether a given `signature` corresponds to a given `msg` and `pk`.
    *
    * @return `true` is the signature is valid, `false` otherwise
    */
  def verify(msg: Array[Byte], signature: Array[Byte], pk: EcPointType): Boolean = {
    val cBytes = signature.take(24)
    val c = BigInt(BigIntegers.fromUnsignedByteArray(cBytes))
    val z = BigInt(BigIntegers.fromUnsignedByteArray(signature.takeRight(32)))
    val a1 = dlogGroup.exponentiate(dlogGroup.generator, z.bigInteger)
    val a2 = dlogGroup.exponentiate(pk, c.bigInteger.negate())
    val w = dlogGroup.multiplyGroupElements(a1, a2)
    val s = genCommitment(pk, w) ++ msg
    java.util.Arrays.equals(hf(s), cBytes)
  }

  private[crypto] def genSecret: BigInt = {
    val y = BigInt(BigIntegers.fromUnsignedByteArray(secureRandomBytes(32)))
    if (y == 0 || y >= groupOrder) genSecret else y
  }

  private def hf(x: Array[Byte]): Array[Byte] = Blake2b256.hash(x).take(24)

  // Assembles a commitment of equivalent to `SigmaTree` form
  private def genCommitment(pk: EcPointType, w: EcPointType): Array[Byte] = {
    val prefix = Base16.decode("010027100108cd").get
    val postfix = Base16.decode("73000021").get
    val pkBytes = GroupElementSerializer.toBytes(pk)
    val wBytes = GroupElementSerializer.toBytes(w)
    prefix ++ pkBytes ++ postfix ++ wBytes
  }

}
