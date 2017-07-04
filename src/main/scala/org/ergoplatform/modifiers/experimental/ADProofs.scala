package org.ergoplatform.modifiers.experimental

import io.circe.Json
import org.ergoplatform.settings.Constants
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

case class ADProofs(proofBytes: Array[Byte]) extends ErgoModifier {
  override val modifierTypeId: ModifierTypeId = ADProofs.ModifierTypeId

  override def id: ModifierId = Constants.hash(proofBytes)

  override type M = ADProofs

  override def serializer: Serializer[ADProofs] = ???

  override def json: Json = ???

  def validate(startingDigest: Array[Byte]): Boolean = {
    //TODO validate proof relative to starting digest
    ???
  }

}

object ADProofs {
  val ModifierTypeId: Byte = 104: Byte
}
