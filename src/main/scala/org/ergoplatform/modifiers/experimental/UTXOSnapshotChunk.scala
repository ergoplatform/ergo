package org.ergoplatform.modifiers.experimental

import io.circe.Json
import org.ergoplatform.nodeView.state.StateElement
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

case class UTXOSnapshotChunk(stateElements: Seq[StateElement], index: Short) extends ErgoModifier {
  override val modifierTypeId: ModifierTypeId = UTXOSnapshotChunk.ModifierTypeId

  override lazy val id: ModifierId = ???

  override type M = UTXOSnapshotChunk

  override lazy val serializer: Serializer[UTXOSnapshotChunk] = ???

  override lazy val json: Json = ???

  lazy val rootHash: Array[Byte] = Algos.merkleTreeRoot(stateElements.map(_.bytes))
}

object UTXOSnapshotChunk {
  val ModifierTypeId: Byte = 107: Byte
}
