package org.ergoplatform.modifiers.experimental

import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

class PoPoWProof extends ErgoModifier {
  override val modifierTypeId: ModifierTypeId = PoPoWProof.ModifierTypeId

  override def id: ModifierId = ???

  override type M = PoPoWProof

  override def serializer: Serializer[PoPoWProof] = ???

  override def json: Json = ???
}

object PoPoWProof {
  val ModifierTypeId: Byte = 105: Byte
}

