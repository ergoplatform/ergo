package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

final case class PoPowProofPrefix(m: Int,
                                  chain: Seq[Header],
                                  suffixId: ModifierId,
                                  sizeOpt: Option[Int] = None)
  extends ErgoPersistentModifier {

  import PoPowAlgos._

  override type M = PoPowProofPrefix

  override val modifierTypeId: ModifierTypeId = PoPoWProof.TypeId

  override def serializedId: Array[Byte] = Algos.hash(bytes)

  override def serializer: ScorexSerializer[M] = NiPoPowProofPrefixSerializer

  override def parentId: ModifierId = chain.head.id

  def chainOfLevel(l: Int): Seq[Header] = chain.filter(maxLevelOf(_) >= l)

  //  def validate: Try[Unit] = {
  //    failFast
  //      .demand(validPrefix, s"Invalid prefix length")
  //      .demand(chain.tail.forall(_.interlinks.headOption.contains(chain.head.id)), "Chain is not anchored")
  //      .result
  //      .toTry
  //  }

  def isBetterThan(that: PoPowProofPrefix): Boolean = {
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

object PoPowProofPrefix {
  val TypeId: ModifierTypeId = ModifierTypeId @@ (111: Byte)
}

object NiPoPowProofPrefixSerializer extends ScorexSerializer[PoPowProofPrefix] {

  override def serialize(obj: PoPowProofPrefix, w: Writer): Unit = {
    w.putInt(obj.m)
    w.putBytes(idToBytes(obj.suffixId))
    w.putInt(obj.chain.size)
    obj.chain.foreach { h =>
      val hBytes = h.bytes
      w.putInt(hBytes.length)
      w.putBytes(hBytes)
    }
  }

  override def parse(r: Reader): PoPowProofPrefix = {
    val startPos = r.position
    val m = r.getInt()
    val suffixId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val prefixSize = r.getInt()
    val prefix = (0 until prefixSize).map { _ =>
      val size = r.getInt()
      HeaderSerializer.parseBytes(r.getBytes(size))
    }
    PoPowProofPrefix(m, prefix, suffixId, Some(r.position - startPos))
  }

}
