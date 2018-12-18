package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try

case class NiPoPowProofPrefix(m: Int,
                              chain: Seq[Header],
                              suffixId: ModifierId,
                              sizeOpt: Option[Int] = None)
  extends ErgoPersistentModifier with ModifierValidator {

  import NiPoPowAlgos._

  override type M = NiPoPowProofPrefix

  override val modifierTypeId: ModifierTypeId = NiPoPowProof.TypeId

  override def serializedId: Array[Byte] = Algos.hash(bytes)

  override def serializer: Serializer[M] = NiPoPowProofPrefixSerializer

  override def parentId: ModifierId = chain.head.id

  def chainOfLevel(l: Int): Seq[Header] = chain.filter(maxLevelOf(_) >= l)

  def validate: Try[Unit] = {
    failFast
      .demand(validPrefix, s"Invalid prefix length")
      .demand(chain.tail.forall(_.interlinks.headOption.contains(chain.head.id)), "Chain is not anchored")
      .result
      .toTry
  }

  def isBetterThan(that: NiPoPowProofPrefix): Boolean = {
    val (thisDivergingChain, thatDivergingChain) = lowestCommonAncestor(chain, that.chain)
      .map(h => chain.filter(_.height > h.height) -> that.chain.filter(_.height > h.height))
      .getOrElse(chain -> that.chain)
    bestArg(thisDivergingChain)(m) > bestArg(thatDivergingChain)(m)
  }

  private def validPrefix: Boolean = {
    val levels = chain.tail.map(maxLevelOf)
    (0 to levels.max).forall(l => chain.count(h => maxLevelOf(h) >= l) >= m) // todo: check max qty overflow as well.
  }

}

object NiPoPowProofPrefix {
  val TypeId: ModifierTypeId = ModifierTypeId @@ (111: Byte)
}

object NiPoPowProofPrefixSerializer extends Serializer[NiPoPowProofPrefix] {

  override def toBytes(obj: NiPoPowProofPrefix): Array[Byte] = {
    Bytes.concat(
      Ints.toByteArray(obj.m),
      idToBytes(obj.suffixId),
      Ints.toByteArray(obj.chain.size),
      Bytes.concat(obj.chain.map(h => Ints.toByteArray(h.bytes.length) ++ h.bytes): _*)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[NiPoPowProofPrefix] = Try {
    import cats.implicits._
    val m = Ints.fromByteArray(bytes.take(4))
    val suffixId = bytesToId(bytes.slice(4, 4 + Constants.ModifierIdSize))
    val prefixSize = Ints.fromByteArray(bytes.slice(4 + Constants.ModifierIdSize, 4 + Constants.ModifierIdSize + 4))
    val (prefixTryList, _) = (0 until prefixSize)
      .foldLeft((List.empty[Try[Header]], bytes.drop(12))) {
        case ((acc, leftBytes), _) =>
          val headerLen = Ints.fromByteArray(leftBytes.take(4))
          val headerTry = HeaderSerializer.parseBytes(leftBytes.slice(4, 4 + headerLen))
          (acc :+ headerTry, leftBytes.drop(4 + headerLen))
      }
    val prefixTry: Try[List[Header]] = prefixTryList.sequence
    prefixTry.map(NiPoPowProofPrefix(m, _, suffixId, Some(bytes.length)))
  }.flatten

}
