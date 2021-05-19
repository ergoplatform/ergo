package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.popow.NipopowProof
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId}

/**
  * Network message carrying NiPoPoW proof for a block.
  *
  * Not really implemented yet. Will be used for bootstrapping.
  *
  * @param proof   - NiPoPoW proof
  * @param sizeOpt - optionally, serialized network message size
  */
case class PoPowProofModifier(proof: NipopowProof, override val sizeOpt: Option[Int] = None)
  extends Comparable[PoPowProofModifier] with Ordered[PoPowProofModifier] with ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = PoPowProofModifier.modifierTypeId

  override def parentId: ModifierId = ???

  override def serializedId: Array[Byte] = Algos.hash(bytes)

  override lazy val id: ModifierId = bytesToId(serializedId)

  override type M = PoPowProofModifier

  override lazy val serializer: ScorexSerializer[PoPowProofModifier] = throw new Error("PoPow proofs serialization not supported")

  override def compare(that: PoPowProofModifier): Int = ???

}

object PoPowProofModifier {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (105: Byte)

}
