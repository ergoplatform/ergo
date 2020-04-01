package org.ergoplatform.modifiers

import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}

/**
  * An interface for Ergo block section which contains corresponding header id and a digest of its payload.
  */
trait BlockSection extends ErgoPersistentModifier {

  override lazy val serializedId: Array[Byte] = BlockSection.computeIdBytes(modifierTypeId, headerId, digest)

  override lazy val id: ModifierId = bytesToId(serializedId)

  def digest: Digest32

  def headerId: ModifierId

  override def parentId: ModifierId = headerId
}

object BlockSection {
  def computeId(modifierType: ModifierTypeId, headerId: ModifierId, digest: Array[Byte]): ModifierId =
    bytesToId(computeIdBytes(modifierType, headerId, digest))

  def computeIdBytes(modifierType: ModifierTypeId, headerId: ModifierId, digest: Array[Byte]): Array[Byte] =
      Algos.hash.prefixedHash(modifierType, idToBytes(headerId), digest)
}
