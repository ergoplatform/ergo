package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId}

case class PoPoWProof(m: Byte,
                      k: Byte,
                      i: Byte,
                      innerchain: Seq[Header],
                      suffix: Seq[Header],
                      override val sizeOpt: Option[Int] = None)
                     (implicit powScheme: AutolykosPowScheme) extends Comparable[PoPoWProof] with Ordered[PoPoWProof]
  with ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = PoPoWProof.modifierTypeId

  override def parentId: ModifierId = ???

  override def serializedId: Array[Byte] = Algos.hash(bytes)

  override lazy val id: ModifierId = bytesToId(serializedId)

  override type M = PoPoWProof

  override lazy val serializer: ScorexSerializer[PoPoWProof] = throw new Error("PoPow proofs serialization not supported")

  override def compare(that: PoPoWProof): Int = ???

}

object PoPoWProof {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (105: Byte)
}
