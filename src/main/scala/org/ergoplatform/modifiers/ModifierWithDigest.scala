package org.ergoplatform.modifiers

import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier._
import scorex.core.PersistentNodeViewModifier

trait ModifierWithDigest extends PersistentNodeViewModifier[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction] {

  override lazy val id: ModifierId = ModifierWithDigest.computeId(modifierTypeId, headerId, digest)

  def digest: Array[Byte]

  def headerId: Array[Byte]
}

object ModifierWithDigest {
  def computeId(modifierType: ModifierTypeId, headerId: Array[Byte], digest: Array[Byte]): Array[Byte] =
    Algos.hash.prefixedHash(modifierType, headerId, digest)
}