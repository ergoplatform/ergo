package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.core.validation.ModifierValidator
import scorex.core.utils.ScorexEncoding
import scorex.util.serialization.{Reader, Writer}
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

  override lazy val serializer: ScorexSerializer[PoPoWProof] = new PoPoWProofSerializer(powScheme)

  //todo: implement
  override def compare(that: PoPoWProof): Int = ???

}

object PoPoWProof {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (105: Byte)
}

@SuppressWarnings(Array("TraversableHead", "CollectionIndexOnNonIndexedSeq"))
class PoPoWProofUtils(powScheme: AutolykosPowScheme) extends ScorexEncoding with ModifierValidator{

  //todo: complete validation, no PoW validation, linking structure validation, genesis validation
  def validate(proof: PoPoWProof): Try[Unit] = {
    //todo: why initial difficulty here?
    val innerDifficulty: BigInt = Constants.InitialDifficulty * BigInt(2).pow(proof.i)
    failFast
      .validate(proof.suffix.lengthCompare(proof.k) == 0) {
        error(s"Incorrect suffix ${proof.suffix.length} != ${proof.k}")
      }
      .validate(proof.k >= 1) {
        error(s"k should positive, ${proof.k} given")
      }
      .validate(proof.m >= 1) {
        error(s"m should positive, ${proof.m} given")
      }
      .validate(proof.suffix.head.interlinks(proof.i) == proof.innerchain.last.id) {
        error(s"Incorrect link form suffix to innerchain in $proof")
      }
      .validate(proof.innerchain.length >= proof.m) {
        error(s"Innerchain length is not enough in $proof")
      }
      .validate(proof.innerchain.forall(h => powScheme.realDifficulty(h) >= innerDifficulty)) {
        error(s"Innerchain difficulty is not enough in $proof")
      }
      .validate(proof.suffix.sliding(2).filter(_.length == 2).forall(s => s(1).parentId == s.head.id)) {
        error(s"Suffix links are incorrect in $proof")
      }
      .validate(proof.innerchain.sliding(2).filter(_.length == 2).forall(s => s(1).interlinks(proof.i) == s.head.id)) {
        error(s"Innerchain links are incorrect in $proof")
      }
      .result
      .toTry
  }

  def isLevel(header: Header, level: Int): Boolean = {
    val headerDiff = powScheme.realDifficulty(header)
    val levelDiff = header.requiredDifficulty * BigInt(2).pow(level)
    headerDiff >= levelDiff
  }

  def maxLevel(header: Header): Int = {
    @tailrec
    def generateMaxLevel(level: Int): Int = {
      if (isLevel(header, level + 1)) {
        generateMaxLevel(level + 1)
      } else {
        level
      }
    }
    generateMaxLevel(0)
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

class PoPoWProofSerializer(powScheme: AutolykosPowScheme) extends ScorexSerializer[PoPoWProof] {

  override def serialize(obj: PoPoWProof, w: Writer): Unit = {
    w.put(obj.m)
    w.put(obj.k)
    w.put(obj.i)
    HeaderSerializer.serialize(obj.suffix.head, w)
    val suffixTail = obj.suffix.tail
    suffixTail.foreach { h =>
      HeaderSerializer.serializeWithoutInterlinks(h, w)
    }
    w.putUShort(obj.innerchain.size)
    obj.innerchain.foreach { h =>
      HeaderSerializer.serialize(h, w)
    }
  }

  override def parse(r: Reader): PoPoWProof = {
    val startPosition = r.position
    val m = r.getByte()
    val k = r.getByte()
    val i = r.getByte()
    val siffixHead = HeaderSerializer.parse(r)

    def parseSuffixes(acc: Seq[Header]): Seq[Header] = {
      if (acc.lengthCompare(k.toInt) == 0)  {
        acc.reverse
      } else {
        val headerWithoutInterlinks = HeaderSerializer.parse(r)
        val interlinks = new PoPoWProofUtils(powScheme).constructInterlinkVector(acc.head)
        parseSuffixes(headerWithoutInterlinks.copy(interlinks = interlinks) +: acc)
      }
    }

    val suffix = parseSuffixes(Seq(siffixHead))

    val innerchainLength = r.getUShort()
    require(innerchainLength > 0)

    @tailrec
    def createInnerChain(step: Int = 0, chain: Seq[Header] = Seq.empty): Seq[Header] = {
      if (step < innerchainLength) {
        val header = HeaderSerializer.parse(r)
        createInnerChain(step + 1, chain ++ Seq(header))
      } else {
        chain
      }
    }
    PoPoWProof(m, k, i, createInnerChain(), suffix, Some(r.position - startPosition))(powScheme)
  }
}
