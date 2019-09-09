package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

final case class PoPowProofPrefix(m: Int,
                                  chain: Seq[PoPowHeader],
                                  suffixId: ModifierId,
                                  sizeOpt: Option[Int] = None)
  extends ErgoPersistentModifier {

  import PoPowAlgos._

  override type M = PoPowProofPrefix

  override val modifierTypeId: ModifierTypeId = PoPowProof.modifierTypeId

  override def serializedId: Array[Byte] = Algos.hash(bytes)

  override def serializer: ScorexSerializer[M] = PoPowProofPrefixSerializer

  override def parentId: ModifierId = chain.head.id

  def headersChain: Seq[Header] = chain.map(_.header)

  def chainOfLevel(l: Int): Seq[PoPowHeader] = chain.filter(x => maxLevelOf(x.header) >= l)

  def isBetterThan(that: PoPowProofPrefix): Boolean = {
    val (thisDivergingChain, thatDivergingChain) = lowestCommonAncestor(headersChain, that.headersChain)
      .map(h => headersChain.filter(_.height > h.height) -> that.headersChain.filter(_.height > h.height))
      .getOrElse(headersChain -> that.headersChain)
    bestArg(thisDivergingChain)(m) > bestArg(thatDivergingChain)(m)
  }

}

object PoPowProofPrefix {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (111: Byte)
}

object PoPowProofPrefixSerializer extends ScorexSerializer[PoPowProofPrefix] {

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
      PoPowHeaderSerializer.parseBytes(r.getBytes(size))
    }
    PoPowProofPrefix(m, prefix, suffixId, Some(r.position - startPos))
  }

}
