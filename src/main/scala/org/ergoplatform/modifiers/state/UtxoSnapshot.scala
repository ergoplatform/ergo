package org.ergoplatform.modifiers.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.util.ModifierId

case class UtxoSnapshot(manifest: UtxoSnapshotManifest,
                        chunks: Seq[UtxoSnapshotChunk],
                        lastHeaders: Seq[Header])
  extends ErgoPersistentModifier {

  override type M = UtxoSnapshot

  override lazy val id: ModifierId = manifest.id

  override val modifierTypeId: ModifierTypeId = UtxoSnapshot.modifierTypeId

  override def serializedId: Array[Byte] = manifest.serializedId

  override val sizeOpt: Option[Int] = None

  override lazy val serializer: Serializer[M] =
    throw new Exception("Serialization for UtxoSnapshot is not (and will not be) implemented")

  override def parentId: ModifierId = ???

}

object UtxoSnapshot {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (108: Byte)
}
