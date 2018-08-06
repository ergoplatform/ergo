package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Shorts}
import org.ergoplatform.mining.PowScheme
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core._
import scorex.core.serialization.Serializer

import scala.util.{Failure, Success, Try}

case class PoPoWProof(m: Byte,
                      k: Byte,
                      i: Byte,
                      innerchain: Seq[Header],
                      suffix: Seq[Header])(implicit powScheme: PowScheme) extends Comparable[PoPoWProof] with Ordered[PoPoWProof]
  with ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = PoPoWProof.modifierTypeId

  override def parentId: ModifierId = ???

  override def serializedId: Array[Byte] = Algos.hash(bytes)

  override lazy val id: ModifierId = bytesToId(serializedId)

  override type M = PoPoWProof

  override lazy val serializer: Serializer[PoPoWProof] = new PoPoWProofSerializer(powScheme)

  //todo: implement
  override def compare(that: PoPoWProof): Int = ???

  val size = None

}

object PoPoWProof {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (105: Byte)
}

@SuppressWarnings(Array("TraversableHead", "CollectionIndexOnNonIndexedSeq"))
class PoPoWProofUtils(powScheme: PowScheme) {

  //todo: complete validation, no PoW validation, linking structure validation, genesis validation
  def validate(proof: PoPoWProof): Try[Unit] = {
    //todo: why initial difficulty here?
    val innerDifficulty: BigInt = Constants.InitialDifficulty * BigInt(2).pow(proof.i)
    if (proof.suffix.lengthCompare(proof.k) != 0) {
      Failure(new Error(s"Incorrect suffix ${proof.suffix.length} != ${proof.k}"))
    } else if (proof.k < 1) {
      Failure(new Error(s"k should positive, ${proof.k} given"))
    } else if (proof.m < 1) {
      Failure(new Error(s"m should positive, ${proof.m} given"))
    } else if (!(proof.suffix.head.interlinks(proof.i) == proof.innerchain.last.id)) {
      Failure(new Error(s"Incorrect link form suffix to innerchain in $proof"))
    } else if (proof.innerchain.length < proof.m) {
      Failure(new Error(s"Innerchain length is not enough in $proof"))
    } else if (!proof.innerchain.forall(h => powScheme.realDifficulty(h) >= innerDifficulty)) {
      Failure(new Error(s"Innerchain difficulty is not enough in $proof"))
    } else if (!proof.suffix.sliding(2).filter(_.length == 2).forall(s => s(1).parentId == s.head.id)) {
      Failure(new Error(s"Suffix links are incorrect in $proof"))
    } else if (!proof.innerchain.sliding(2).filter(_.length == 2)
      .forall(s => s(1).interlinks(proof.i) == s.head.id)) {
      Failure(new Error(s"Innerchain links are incorrect in $proof"))
    } else {
      Success(Unit)
    }
  }

  def isLevel(header: Header, level: Int): Boolean = {
    val headerDiff = powScheme.realDifficulty(header)
    val levelDiff = header.requiredDifficulty * BigInt(2).pow(level)
    headerDiff >= levelDiff
  }

  def maxLevel(header: Header): Int = {
    var level = 0
    while (isLevel(header, level + 1)) level = level + 1
    level
  }

  def constructInterlinkVector(parent: Header): Seq[ModifierId] = {
    if (parent.isGenesis) {
      //initialize interlink vector at first block after genesis
      Seq(parent.id)
    } else {

      val genesisId = parent.interlinks.head

      val tail = parent.interlinks.tail

      val pLevel = maxLevel(parent)

      val pLevels = if (pLevel > 0) Seq.fill(pLevel)(parent.id) else Seq.empty

      val priorLevels = if(tail.length > pLevel) tail.drop(pLevel) else Seq.empty

      (genesisId +: pLevels) ++ priorLevels
    }
  }
}

@SuppressWarnings(Array("TraversableHead"))
class PoPoWProofSerializer(powScheme: PowScheme) extends Serializer[PoPoWProof] {
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
    val headSuffix = HeaderSerializer.parseBytes(bytes.slice(5, 5 + headSuffixLength)).get

    def parseSuffixes(index: Int, acc: Seq[Header]): (Int, Seq[Header]) = {
      if (acc.lengthCompare(k.toInt) == 0)  {
        (index, acc.reverse)
      } else {
        val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val headerWithoutInterlinks = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
        val interlinks = new PoPoWProofUtils(powScheme).constructInterlinkVector(acc.head)
        parseSuffixes(index + 2 + l, headerWithoutInterlinks.copy(interlinks = interlinks) +: acc)
      }
    }

    var (index, suffix) = parseSuffixes(5 + headSuffixLength, Seq(headSuffix))
    val innerchainLength = Shorts.fromByteArray(bytes.slice(index, index + 2))
    index = index + 2
    val innerchain = (0 until innerchainLength) map { _ =>
      val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
      val header = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
      index = index + 2 + l
      header
    }
    PoPoWProof(m, k, i, innerchain, suffix)(powScheme)
  }
}
