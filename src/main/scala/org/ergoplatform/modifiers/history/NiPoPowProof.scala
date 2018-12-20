package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.util.ModifierId

import scala.util.Try

case class NiPoPowProof(prefix: NiPoPowProofPrefix, suffix: NiPoPowProofSuffix)
  extends ErgoPersistentModifier with ModifierValidator {

  override type M = NiPoPowProof

  override val modifierTypeId: ModifierTypeId = NiPoPowProof.TypeId

  override val sizeOpt: Option[Int] = None

  override def serializedId: Array[Byte] = prefix.serializedId

  override def serializer: Serializer[M] =
    throw new Exception("Serialization for NiPoPowProof is not supported")

  override def parentId: ModifierId = prefix.parentId

  def chain: Seq[Header] = prefix.chain ++ suffix.chain

  def validate: Try[Unit] = prefix.validate.flatMap(_ => suffix.validate)

}

object NiPoPowProof {

  val TypeId: ModifierTypeId = ModifierTypeId @@ (110: Byte)

  def apply(m: Int, k: Int, prefixChain: Seq[Header], suffixChain: Seq[Header]): NiPoPowProof = {
    val suffix = NiPoPowProofSuffix(k, suffixChain)
    val prefix = NiPoPowProofPrefix(m, prefixChain, suffix.id)
    new NiPoPowProof(prefix, suffix)
  }

}

object NiPoPowProofSerializer extends Serializer[NiPoPowProof] {

  override def toBytes(obj: NiPoPowProof): Array[Byte] = {
    Bytes.concat(
      Ints.toByteArray(obj.prefix.bytes.length),
      Ints.toByteArray(obj.suffix.bytes.length),
      obj.prefix.bytes,
      obj.suffix.bytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[NiPoPowProof] = {
    val prefixLen = Ints.fromByteArray(bytes.take(4))
    val suffixLen = Ints.fromByteArray(bytes.slice(4, 8))
    NiPoPowProofPrefixSerializer.parseBytes(bytes.slice(8, prefixLen + 8)).flatMap { prefix =>
      NiPoPowProofSuffixSerializer.parseBytes(bytes.slice(prefixLen + 8, prefixLen + 8 + suffixLen))
        .map(NiPoPowProof(prefix, _))
    }
  }

}
