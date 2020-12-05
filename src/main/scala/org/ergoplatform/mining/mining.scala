package org.ergoplatform

import org.bouncycastle.util.BigIntegers
import scorex.crypto.hash.Blake2b256
import sigmastate.basics.BcDlogGroup
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.serialization.{GroupElementSerializer, SigmaSerializer}

package object mining {

  type PrivateKey = BigInt

  val PublicKeyLength: Byte = 33

  val group: BcDlogGroup[EcPointType] = CryptoConstants.dlogGroup

  val q: BigInt = group.order

  private val hashFn: NumericHash = new NumericHash(q)

  /**
    * Hash function which output is in Zq. Used in Autolykos v.1
    * @param in - input (bitstring)
    * @return - output(in Zq)
    */
  def hashModQ(in: Array[Byte]): BigInt = hashFn.hash(in)

  /**
    * Convert byte array to unsigned integer
    * @param in - byte array
    * @return - unsigned integer
    */
  def toBigInt(in: Array[Byte]): BigInt = BigInt(BigIntegers.fromUnsignedByteArray(in))

  /**
    * Blake2b256 hash function invocation
    * @param in - input bitstring
    * @return - 256 bits (32 bytes) array
    */
  def hash(in: Array[Byte]): Array[Byte] = Blake2b256.hash(in)

  def genPk(s: PrivateKey): EcPointType = group.exponentiate(group.generator, s.bigInteger)

  def randomSecret(): PrivateKey = DLogProverInput.random().w

  def groupElemToBytes(ge: EcPointType): Array[Byte] = GroupElementSerializer.toBytes(ge)

  def groupElemFromBytes(bytes: Array[Byte]): EcPointType = GroupElementSerializer.parse(SigmaSerializer.startReader(bytes))

}
