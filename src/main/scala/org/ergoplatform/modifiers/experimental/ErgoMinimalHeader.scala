package org.ergoplatform.modifiers.experimental

import io.circe.Json
import org.ergoplatform.settings.Constants
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

case class ErgoMinimalHeader(payloadRootHash: Array[Byte],
                             nonce: Int) extends NodeViewModifier {

  override val modifierTypeId: ModifierTypeId = ErgoMinimalHeader.ModifierTypeId

  override lazy val id: ModifierId = Constants.hash(bytes)

  override lazy val json: Json = ???


  override type M = ErgoMinimalHeader

  override lazy val serializer: Serializer[ErgoMinimalHeader] = ???

}

object ErgoMinimalHeader {
  val ModifierTypeId: Byte = 10: Byte
}
