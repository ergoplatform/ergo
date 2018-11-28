package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.util.ModifierId

import scala.util.Try

case class NiPoPowProof(m: Int, k: Int, prefix: Seq[Header], suffix: Seq[Header])
  extends ErgoPersistentModifier with ModifierValidator {

  override type M = NiPoPowProof

  override val modifierTypeId: ModifierTypeId = NiPoPowProof.modifierTypeId

  override val sizeOpt: Option[Int] = None

  override def serializedId: Array[Byte] = ???

  override def serializer: Serializer[NiPoPowProof] = ???

  override def parentId: ModifierId = ???

  def maxLevel: Int = ???

  def headersOfLevel(l: Int): Seq[Header] = ???

  def validate: Try[Unit] = {
    failFast
      .demand(suffix.lengthCompare(k) == 0, "Invalid suffix length")
      .demand(prefix.groupBy(NiPoPowProof.levelOf).forall(_._2.lengthCompare(m) == 0), "Invalid prefix length")
      .result
      .toTry
  }
}

object NiPoPowProof {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (110: Byte)

  def levelOf(header: Header): Int = ???
}
