package org.ergoplatform.modifiers

import org.ergoplatform.core.BytesSerializable
import org.ergoplatform.utils.ScorexEncoding
import scorex.util.{ModifierId, bytesToId}
import scorex.utils.Ints

trait ErgoNodeViewModifier extends BytesSerializable with ScorexEncoding {

  lazy val id: ModifierId = bytesToId(serializedId)

  val modifierTypeId: NetworkObjectTypeId.Value

  val sizeOpt: Option[Int]

  lazy val size: Int = sizeOpt.getOrElse(bytes.length)

  def serializedId: Array[Byte]

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ErgoNodeViewModifier => (that.id == id) && (that.modifierTypeId == modifierTypeId)
    case _ => false
  }

  override def hashCode(): Int = {
    Ints.fromByteArray(serializedId)
  }

  /**
    * @return readable representation of `id`, as `id` is a hex-encoded string now, just identity functions is used
    */
  def encodedId: String = id
}


/**
  * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
  * have identifiers of the some length fixed with the ModifierIdSize constant
  */
object ErgoNodeViewModifier {
  val ModifierIdSize: Int = 32 // in bytes
}
