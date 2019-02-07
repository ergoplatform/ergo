package org.ergoplatform

import scorex.util.encode.Base16
import sigmastate.basics.BcDlogGroup
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.serialization.{GroupElementSerializer, Serializer}

package object mining {

  type PrivateKey = BigInt

  val PublicKeyLength: Byte = 33

  val group: BcDlogGroup[EcPointType] = CryptoConstants.dlogGroup

  val q: BigInt = group.order

  private val hashFn: NumericHash = new NumericHash(q)

  def hash(in: Array[Byte]): BigInt = hashFn.hash(in)

  def genPk(s: PrivateKey): EcPointType = group.exponentiate(group.generator, s.bigInteger)

  def randomSecret(): PrivateKey = DLogProverInput.random().w

  def groupElemToBytes(pk: EcPointType): Array[Byte] = GroupElementSerializer.toBytes(pk)

  def groupElemFromBytes(bytes: Array[Byte]): EcPointType = GroupElementSerializer.parseBody(Serializer.startReader(bytes))
}