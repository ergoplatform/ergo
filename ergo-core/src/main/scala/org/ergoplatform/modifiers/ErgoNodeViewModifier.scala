package org.ergoplatform.modifiers

import org.ergoplatform.core.BytesSerializable
import org.ergoplatform.utils.ScorexEncoding
import scorex.util.{ModifierId, bytesToId}
import scorex.utils.Ints

/**
  * Basic trait for entities which are modifying internal blockchain view of the node, such as
  * unconfirmed transactions, block sections
  */
trait ErgoNodeViewModifier extends BytesSerializable with ScorexEncoding {

  /**
    * @return cryptographically strong unique identifier of an object
    */
  def serializedId: Array[Byte]

  /**
    * identifier which can is friendly to data structures such as maps etc
    */
  lazy val id: ModifierId = bytesToId(serializedId)

  /**
    * Type of node view modifier (transaction, header etc)
    */
  val modifierTypeId: NetworkObjectTypeId.Value

  /**
    * Size of binary representation provided during object construction (to avoid serialization just to get the value)
    */
  val sizeOpt: Option[Int]

  /**
    * Cached size of binary representation
    */
  lazy val size: Int = sizeOpt.getOrElse(bytes.length)

  /**
    * @return readable representation of `id`, as `id` is a hex-encoded string now, just identity functions is used
    */
  def encodedId: String = id

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ErgoNodeViewModifier => (that.id == id) && (that.modifierTypeId == modifierTypeId)
    case _ => false
  }

  override def hashCode(): Int = {
    Ints.fromByteArray(serializedId)
  }

}

object ErgoNodeViewModifier {
  /**
    * It is assumed that all the modifiers (offchain transactions, blocks, blockheaders etc)
    * have identifiers of the some length fixed with the ModifierIdSize constant
    */
  val ModifierIdSize: Int = 32 // in bytes
}
