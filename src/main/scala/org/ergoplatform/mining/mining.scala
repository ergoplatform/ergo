package org.ergoplatform

import org.bouncycastle.math.ec.ECPoint
import scapi.sigma.BcDlogFp
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType

package object mining {

  type PrivateKey = BigInt

  val PublicKeyLength: Byte = 33
  val group: BcDlogFp[EcPointType] = CryptoConstants.dlogGroup
  val q: BigInt = group.q
  private val hashFn: NumericHash = new NumericHash(q)

  def hash(in: Array[Byte]): BigInt = hashFn.hash(in)

  def genPk(s: PrivateKey): ECPoint = group.exponentiate(group.generator, s.bigInteger)

  def randomSecret(): PrivateKey = hash(scorex.utils.Random.randomBytes(32))

  def lg(x: Int): Int = (Math.log(x) / Math.log(2)).toInt.ensuring(s => Math.pow(2, s) == x)

  def pkToBytes(pk: ECPoint): Array[Byte] = pk.getEncoded(true).ensuring(_.size == PublicKeyLength)

  def pkFromBytes(bytes: Array[Byte]): ECPoint = group.curve.decodePoint(bytes)

}
