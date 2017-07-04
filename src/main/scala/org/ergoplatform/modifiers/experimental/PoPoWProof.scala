package org.ergoplatform.modifiers.experimental

import io.circe.Json
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

import scala.util.Try

case class PoPoWProof(m: Int,
                      k: Int,
                      innerchain: Seq[Header],
                      suffix: Seq[Header]) extends Comparable[PoPoWProof] with Ordered[PoPoWProof] with HistoryModifier {

  override val modifierTypeId: ModifierTypeId = PoPoWProof.ModifierTypeId

  override lazy val id: ModifierId = ???

  override type M = PoPoWProof

  override lazy val serializer: Serializer[PoPoWProof] = ???

  override lazy val json: Json = ???

  override def compare(that: PoPoWProof): Int = ???

  lazy val validate: Try[Unit] = ???
}

object PoPoWProof {
  val ModifierTypeId: Byte = 105: Byte
}

