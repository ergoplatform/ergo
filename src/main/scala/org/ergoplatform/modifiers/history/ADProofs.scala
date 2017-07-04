package org.ergoplatform.modifiers.history

import io.circe.Json
import org.ergoplatform.settings.Constants
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

import scala.util.Try

case class ADProofs(proofBytes: Array[Byte]) extends HistoryModifier {
  override val modifierTypeId: ModifierTypeId = ADProofs.ModifierTypeId

  override lazy val id: ModifierId = Constants.hash(proofBytes)

  override type M = ADProofs

  override lazy val serializer: Serializer[ADProofs] = ???

  override lazy val json: Json = ???

}

object ADProofs {
  val ModifierTypeId: Byte = 104: Byte

  def validate(proof: ADProofs, startingDigest: Array[Byte]): Try[Unit] = {
    //TODO validate proof relative to starting digest
    ???
  }

}
