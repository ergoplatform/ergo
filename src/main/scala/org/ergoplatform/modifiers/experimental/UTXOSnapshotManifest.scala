package org.ergoplatform.modifiers.experimental

import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

class UTXOSnapshotManifest extends ErgoModifier {
  override val modifierTypeId: ModifierTypeId = UTXOSnapshotManifest.ModifierTypeId

  override lazy val id: ModifierId = ???

  override type M = UTXOSnapshotManifest

  override lazy val serializer: Serializer[UTXOSnapshotManifest] = ???

  override lazy val json: Json = ???
}

object UTXOSnapshotManifest {
  val ModifierTypeId: Byte = 106: Byte
}

