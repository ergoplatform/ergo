package org.ergoplatform.modifiers.experimental

import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

class ADProofs extends ErgoModifier {
  override val modifierTypeId: ModifierTypeId = ADProofs.ModifierTypeId

  override def id: ModifierId = ???

  override type M = ADProofs

  override def serializer: Serializer[ADProofs] = ???

  override def json: Json = ???
}

object ADProofs {
  val ModifierTypeId: Byte = 104: Byte
}
