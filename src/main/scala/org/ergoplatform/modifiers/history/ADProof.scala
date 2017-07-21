package org.ergoplatform.modifiers.history

import com.google.common.primitives.Bytes
import io.circe.Json
import org.ergoplatform.modifiers.history.ADProof.ModifierTypeId
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

import scala.util.Try

case class ADProof(headerId: ModifierId, proofBytes: Array[Byte]) extends HistoryModifier {
  override val modifierTypeId: ModifierTypeId = ADProof.ModifierTypeId

  override lazy val id: ModifierId = Algos.hash.prefixedHash(ModifierTypeId, headerId, proofBytes)

  override type M = ADProof

  override lazy val serializer: Serializer[ADProof] = ADProofSerializer

  override lazy val json: Json = ???

  override def toString: String = s"ADProofs(${Base58.encode(id)},${Base58.encode(headerId)},${Base58.encode(proofBytes)})"
}

object ADProof {
  type ProofRepresentation = Array[Byte]

  val ModifierTypeId: Byte = 104: Byte

  def proofDigest(proofBytes: Array[Byte]): Array[Byte] = Algos.hash.prefixedHash(ModifierTypeId, proofBytes)
}

object ADProofSerializer extends Serializer[ADProof] {
  override def toBytes(obj: ADProof): Array[Byte] = Bytes.concat(obj.headerId, obj.proofBytes)

  override def parseBytes(bytes: Array[Byte]): Try[ADProof] = Try {
    ADProof(bytes.take(Constants.ModifierIdSize), bytes.slice(Constants.ModifierIdSize, bytes.length))
  }
}
