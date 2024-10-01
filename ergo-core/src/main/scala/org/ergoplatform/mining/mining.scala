package org.ergoplatform

import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining.AutolykosSolution
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import scorex.crypto.hash.Blake2b256
import sigma.crypto.{BcDlogGroup, CryptoConstants, EcPointType}
import sigma.serialization.{GroupElementSerializer, SigmaSerializer}
import sigmastate.crypto.DLogProtocol.DLogProverInput

sealed trait ProveBlockResult

case object NothingFound extends ProveBlockResult

case class OrderingBlockFound(fb: ErgoFullBlock) extends ProveBlockResult

case class OrderingBlockHeaderFound(h: Header) extends ProveBlockResult

case class InputBlockFound(fb: ErgoFullBlock) extends ProveBlockResult

case class InputBlockHeaderFound(h: Header) extends ProveBlockResult

sealed trait BlockSolutionSearchResult

case object NoSolutionFound extends BlockSolutionSearchResult

sealed trait SolutionFound extends BlockSolutionSearchResult {
  val as: AutolykosSolution
}

case class InputSolutionFound(override val as: AutolykosSolution) extends SolutionFound

case class OrderingSolutionFound(override val as: AutolykosSolution) extends SolutionFound

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
