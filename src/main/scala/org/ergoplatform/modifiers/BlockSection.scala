package org.ergoplatform.modifiers

import org.ergoplatform.settings.Algos
import scorex.core.{ModifierId, ModifierTypeId, PersistentNodeViewModifier}

/**
  * An interface for Ergo block section which contains corresponding header id and a digest of its payload.
  */
trait BlockSection extends ErgoPersistentModifier {

  override lazy val id: ModifierId = BlockSection.computeId(modifierTypeId, headerId, digest)

  def digest: Array[Byte]
  def headerId: ModifierId
  override def parentId: ModifierId = headerId

}

object BlockSection {
  def computeId(modifierType: ModifierTypeId, headerId: Array[Byte], digest: Array[Byte]): ModifierId =
    ModifierId @@ Algos.hash.prefixedHash(modifierType, headerId, digest)
}
