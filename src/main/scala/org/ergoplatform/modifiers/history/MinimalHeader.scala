package org.ergoplatform.modifiers.history

import com.google.common.primitives.Ints
import io.circe.Json
import org.ergoplatform.modifiers.ErgoModifier
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

import scala.util.Try

case class MinimalHeader(payloadRootHash: Array[Byte],
                         nonce: Int) extends ErgoModifier {

  override val modifierTypeId: ModifierTypeId = MinimalHeader.ModifierTypeId

  override lazy val id: ModifierId = Algos.hash(bytes)

  override lazy val json: Json = ???

  override type M = MinimalHeader

  override lazy val serializer: Serializer[MinimalHeader] = MinimalHeaderSerializer

}

object MinimalHeader {
  val ModifierTypeId: Byte = 100: Byte
}

object MinimalHeaderSerializer extends Serializer[MinimalHeader] {
  override def toBytes(obj: MinimalHeader): Array[Byte] = obj.payloadRootHash ++ Ints.toByteArray(obj.nonce)

  override def parseBytes(bytes: Array[Byte]): Try[MinimalHeader] = Try {
    MinimalHeader(bytes.slice(0, 32), Ints.fromByteArray(bytes.slice(32, 36)))
  }
}

