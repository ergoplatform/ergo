package org.ergoplatform

import sigmastate.basics.BcDlogFp
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.serialization.{GroupElementSerializer, Serializer}

package object mining {

  type PrivateKey = BigInt

  val PublicKeyLength: Byte = 33

  val group: BcDlogFp[EcPointType] = CryptoConstants.dlogGroup

  val q: BigInt = group.q

  private val hashFn: NumericHash = new NumericHash(q)

  def hash(in: Array[Byte]): BigInt = hashFn.hash(in)

  def genPk(s: PrivateKey): EcPointType = group.exponentiate(group.generator, s.bigInteger)

  def randomSecret(): PrivateKey = DLogProverInput.random().w

  def pkToBytes(pk: EcPointType): Array[Byte] = GroupElementSerializer.toBytes(pk)

  def pkFromBytes(bytes: Array[Byte]): EcPointType = GroupElementSerializer.parseBody(Serializer.startReader(bytes))

}
