package org.ergoplatform.modifiers.experimental

import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

class UTXOSnapshotChunk extends ErgoModifier {
  override val modifierTypeId: ModifierTypeId = UTXOSnapshotChunk.ModifierTypeId

  override lazy val id: ModifierId = ???

  override type M = UTXOSnapshotChunk

  override lazy val serializer: Serializer[UTXOSnapshotChunk] = ???

  override lazy val json: Json = ???
}

object UTXOSnapshotChunk {
  val ModifierTypeId: Byte = 107: Byte
}
