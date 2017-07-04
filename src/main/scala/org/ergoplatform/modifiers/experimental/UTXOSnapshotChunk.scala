package org.ergoplatform.modifiers.experimental

import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

class UTXOSnapshotChunk extends ErgoModifier {
  override val modifierTypeId: ModifierTypeId = UTXOSnapshotChunk.ModifierTypeId

  override def id: ModifierId = ???

  override type M = UTXOSnapshotChunk

  override def serializer: Serializer[UTXOSnapshotChunk] = ???

  override def json: Json = ???
}

object UTXOSnapshotChunk {
  val ModifierTypeId: Byte = 107: Byte
}
