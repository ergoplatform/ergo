package org.ergoplatform.modifiers.history

import com.google.common.primitives.Bytes
import io.circe.Json
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

import scala.util.Try

case class ADProofs(headerId: ModifierId, proofBytes: Array[Byte]) extends HistoryModifier {
  override val modifierTypeId: ModifierTypeId = ADProofs.ModifierTypeId

  override lazy val id: ModifierId = ADProofs.idFromProof(proofBytes)

  override type M = ADProofs

  override lazy val serializer: Serializer[ADProofs] = ADProofsSerializer

  override lazy val json: Json = ???

  override def toString: String = s"ADProofs(${Base58.encode(id)},${Base58.encode(headerId)},${Base58.encode(proofBytes)})"
}

object ADProofs {
  val ModifierTypeId: Byte = 104: Byte

  def idFromProof(proofBytes: Array[Byte]): Array[Byte] = Algos.hash.prefixedHash(ModifierTypeId, proofBytes)
}

object ADProofsSerializer extends Serializer[ADProofs] {
  override def toBytes(obj: ADProofs): Array[ModifierTypeId] = Bytes.concat(obj.headerId, obj.proofBytes)

  override def parseBytes(bytes: Array[ModifierTypeId]): Try[ADProofs] = Try {
    ADProofs(bytes.take(Constants.ModifierIdSize), bytes.slice(Constants.ModifierIdSize, bytes.length))
  }
}
