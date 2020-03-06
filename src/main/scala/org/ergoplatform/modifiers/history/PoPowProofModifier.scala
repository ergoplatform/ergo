package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.popow.PoPowProof
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId}

case class PoPowProofModifier(proof: PoPowProof,
                              override val sizeOpt: Option[Int] = None)
                             (implicit powScheme: AutolykosPowScheme) extends Comparable[PoPowProofModifier] with Ordered[PoPowProofModifier]
  with ErgoPersistentModifier {

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