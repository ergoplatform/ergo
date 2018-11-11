package org.ergoplatform.modifiers.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId}

case class UtxoSnapshot(manifest: UtxoSnapshotManifest, chunks: IndexedSeq[UtxoSnapshotChunk])
  extends ErgoPersistentModifier {

  override type M = UtxoSnapshot

  override lazy val id: ModifierId = manifest.id

  override val modifierTypeId: ModifierTypeId = UtxoSnapshot.modifierTypeId

  override def serializedId: Array[Byte] = manifest.serializedId

  override def parentId: ModifierId = ???

  override val sizeOpt: Option[Int] = None

  override lazy val serializer: Serializer[M] =
    throw new Exception("Serialization for UtxoSnapshot is not (and will not be) implemented")
}

object UtxoSnapshot {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (108: Byte)

  def rootHashToSerializedId(rootHash: Digest32): Array[Byte] = Algos.hash(rootHash)

  def rootHashToId(rootHash: Digest32): ModifierId = bytesToId(rootHashToSerializedId(rootHash))

}
