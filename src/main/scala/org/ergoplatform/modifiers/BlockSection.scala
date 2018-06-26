package org.ergoplatform.modifiers

import org.ergoplatform.settings.Algos
import scorex.core.{ModifierId, ModifierTypeId, PersistentNodeViewModifier}

/**
  * Section of Ergo block, that contains header id and digest of it's payload
  */
trait BlockSection extends ErgoPersistentModifier {

  override lazy val id: ModifierId = BlockSection.computeId(modifierTypeId, headerId, digest)

  def digest: Array[Byte]
  def headerId: ModifierId
}

object BlockSection {
  def computeId(modifierType: ModifierTypeId, headerId: Array[Byte], digest: Array[Byte]): ModifierId =
    ModifierId @@ Algos.hash.prefixedHash(modifierType, headerId, digest)
}
