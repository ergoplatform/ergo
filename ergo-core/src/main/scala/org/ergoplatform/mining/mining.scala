package org.ergoplatform

import org.bouncycastle.util.BigIntegers
import scorex.crypto.hash.Blake2b256
import sigmastate.crypto.CryptoConstants.EcPointType
import sigmastate.crypto.{BcDlogGroup, CryptoConstants}
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.serialization.{GroupElementSerializer, SigmaSerializer}

package object mining {

  type PrivateKey = BigInt

  val PublicKeyLength: Int = 33

  val group: BcDlogGroup = CryptoConstants.dlogGroup

  // Group order, used in Autolykos V.1 for non-outsourceability,
  // and also to obtain target in both Autolykos v1 and v2
  val q: BigInt = group.order

  private val modQHashFn: ModQHash = new ModQHash(q)

  /**
    * Hash function which output is in Zq. Used in Autolykos v.1
    * @param in - input (bit-string)
    * @return - output(in Zq)
    */
  def hashModQ(in: Array[Byte]): BigInt = modQHashFn.hash(in)

  /**
    * Convert byte array to unsigned integer
    * @param in - byte array
    * @return - unsigned integer
    */
  def toBigInt(in: Array[Byte]): BigInt = BigInt(BigIntegers.fromUnsignedByteArray(in))

  /**
    * Blake2b256 hash function invocation
    * @param in - input bit-string
    * @return - 256 bits (32 bytes) array
    */
  def hash(in: Array[Byte]): Array[Byte] = Blake2b256.hash(in)

  def genPk(s: PrivateKey): EcPointType = group.exponentiate(group.generator, s.bigInteger)

  def randomSecret(): PrivateKey = DLogProverInput.random().w

  def groupElemToBytes(ge: EcPointType): Array[Byte] = GroupElementSerializer.toBytes(ge)

  def groupElemFromBytes(bytes: Array[Byte]): EcPointType = GroupElementSerializer.parse(SigmaSerializer.startReader(bytes))

}
