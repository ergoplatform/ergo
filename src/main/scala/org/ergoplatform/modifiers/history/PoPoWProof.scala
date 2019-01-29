package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Shorts}
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ModifierValidator
import scorex.util.{ModifierId, bytesToId}

import scala.annotation.tailrec
import scala.util.Try

case class PoPoWProof(m: Byte,
                      k: Byte,
                      i: Byte,
                      innerchain: Seq[Header],
                      suffix: Seq[Header],
                      override val sizeOpt: Option[Int] = None)
                     (implicit powScheme: AutolykosPowScheme) extends Comparable[PoPoWProof] with Ordered[PoPoWProof]
  with ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = PoPoWProof.modifierTypeId

  override def parentId: ModifierId = ???

  override def serializedId: Array[Byte] = Algos.hash(bytes)

  override lazy val id: ModifierId = bytesToId(serializedId)

  override type M = PoPoWProof

  override lazy val serializer: Serializer[PoPoWProof] = new PoPoWProofSerializer(powScheme)

  //todo: implement
  override def compare(that: PoPoWProof): Int = ???

}

object PoPoWProof {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (105: Byte)
}

@SuppressWarnings(Array("TraversableHead"))
class PoPoWProofSerializer(powScheme: AutolykosPowScheme) extends Serializer[PoPoWProof] {
  override def toBytes(obj: PoPoWProof): Array[Byte] = {
    val suffixTailBytes = scorex.core.utils.concatBytes(obj.suffix.tail.map { h =>
      val bytes = HeaderSerializer.bytesWithoutInterlinks(h)
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    })
    val innerchainBytes = scorex.core.utils.concatBytes(obj.innerchain.map { h =>
      val bytes = h.bytes
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    })
    Bytes.concat(Array(obj.m, obj.k, obj.i),
      Shorts.toByteArray(obj.suffix.head.bytes.length.toShort),
      obj.suffix.head.bytes,
      suffixTailBytes,
      Shorts.toByteArray(obj.innerchain.length.toShort),
      innerchainBytes)
  }

  @SuppressWarnings(Array("TryGet"))
  override def parseBytes(bytes: Array[Byte]): Try[PoPoWProof] = Try {
    val m = bytes.head
    val k = bytes(1)
    val i = bytes(2)
    val headSuffixLength = Shorts.fromByteArray(bytes.slice(3, 5))
    require(headSuffixLength > 0)
    val headSuffix = HeaderSerializer.parseBytes(bytes.slice(5, 5 + headSuffixLength)).get

    def parseSuffixes(index: Int, acc: Seq[Header]): (Int, Seq[Header]) = {
      if (acc.lengthCompare(k.toInt) == 0)  {
        (index, acc.reverse)
      } else {
        val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
        require(l > 0)
        val headerWithoutInterlinks = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
        val interlinks = Seq.empty
        parseSuffixes(index + 2 + l, headerWithoutInterlinks.copy(interlinks = interlinks) +: acc)
      }
    }

    val (index, suffix) = parseSuffixes(5 + headSuffixLength, Seq(headSuffix))
    val innerchainLength = Shorts.fromByteArray(bytes.slice(index, index + 2))
    require(innerchainLength > 0)

    @tailrec
    def createInnerChain(index: Int, step: Int = 0, chain: Seq[Header] = Seq.empty): Seq[Header] = {
      if (step < innerchainLength) {
        val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
        require(l > 0)
        val header = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
        createInnerChain(index + 2 + l, step + 1, chain ++ Seq(header))
      } else {
        chain
      }
    }
    PoPoWProof(m, k, i, createInnerChain(index + 2), suffix, Some(bytes.length))(powScheme)
  }
}
