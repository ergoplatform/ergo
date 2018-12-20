package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.util.ModifierId

import scala.util.Try

case class NiPoPowProofSuffix(k: Int,
                              chain: Seq[Header],
                              sizeOpt: Option[Int] = None)
  extends ErgoPersistentModifier with ModifierValidator {

  override type M = NiPoPowProofSuffix

  override val modifierTypeId: ModifierTypeId = NiPoPowProof.TypeId

  override def serializedId: Array[Byte] = Algos.hash(bytes)

  override def serializer: Serializer[M] = NiPoPowProofSuffixSerializer

  override def parentId: ModifierId = chain.head.id

  def validate: Try[Unit] = {
    failFast
      .demand(chain.lengthCompare(k) == 0, "Invalid suffix length")
      .result
      .toTry
  }

}

object NiPoPowProofSuffix {
  val TypeId: ModifierTypeId = ModifierTypeId @@ (112: Byte)
}

object NiPoPowProofSuffixSerializer extends Serializer[NiPoPowProofSuffix] {

  override def toBytes(obj: NiPoPowProofSuffix): Array[Byte] = {
    Bytes.concat(
      Ints.toByteArray(obj.k),
      Ints.toByteArray(obj.chain.size),
      Bytes.concat(obj.chain.map(h => Ints.toByteArray(h.bytes.length) ++ h.bytes): _*)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[NiPoPowProofSuffix] = Try {
    import cats.implicits._
    val k = Ints.fromByteArray(bytes.take(4))
    val suffixSize = Ints.fromByteArray(bytes.slice(4, 8))
    val (suffixTryList, _) = (0 until suffixSize)
      .foldLeft((List.empty[Try[Header]], bytes.drop(12))) {
        case ((acc, leftBytes), _) =>
          val headerLen = Ints.fromByteArray(leftBytes.take(4))
          val headerTry = HeaderSerializer.parseBytes(leftBytes.slice(4, 4 + headerLen))
          (acc :+ headerTry, leftBytes.drop(4 + headerLen))
      }
    val suffixTry: Try[List[Header]] = suffixTryList.sequence
    suffixTry.map(suffix => NiPoPowProofSuffix(k, suffix, Some(bytes.length)))
  }.flatten

}
