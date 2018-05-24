package org.ergoplatform.modifiers.state

import org.ergoplatform.ErgoTransaction
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk.StateElement
import org.ergoplatform.settings.Algos
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32
import scorex.utils.Random

case class UTXOSnapshotChunk(stateElements: Seq[StateElement], index: Short) extends ErgoPersistentModifier {
  override val modifierTypeId: ModifierTypeId = UTXOSnapshotChunk.modifierTypeId

  //TODO implement correctly
  override lazy val id: ModifierId = ModifierId @@ Random.randomBytes(32)

  override type M = UTXOSnapshotChunk

  override lazy val serializer: Serializer[UTXOSnapshotChunk] = ???

  lazy val rootHash: Digest32 = Algos.merkleTreeRoot(stateElements.map(LeafData @@ _.bytes))
}

object UTXOSnapshotChunk {
  type StateElement = ErgoTransaction

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (107: Byte)
}
