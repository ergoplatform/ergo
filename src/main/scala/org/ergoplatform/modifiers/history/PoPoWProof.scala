package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Shorts}
import io.circe.Json
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

import scala.util.{Failure, Success, Try}

case class PoPoWProof(m: Byte,
                      k: Byte,
                      i: Byte,
                      innerchain: Seq[Header],
                      suffix: Seq[Header]) extends Comparable[PoPoWProof] with Ordered[PoPoWProof]
  with ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = PoPoWProof.ModifierTypeId

  override lazy val id: ModifierId = Algos.hash(bytes)

  override type M = PoPoWProof

  override lazy val serializer: Serializer[PoPoWProof] = PoPoWProofSerializer

  override lazy val json: Json = ???

  //todo: implement
  override def compare(that: PoPoWProof): Int = ???

}

object PoPoWProof {
  val ModifierTypeId: Byte = 105: Byte

  //todo: complete validation, no PoW validation, linking structure validation, genesis validation
  def validate(proof: PoPoWProof): Try[Unit] = {
    val innerDifficulty: BigInt = Constants.InitialDifficulty * BigInt(2).pow(proof.i)
    if (proof.suffix.length != proof.k) {
      Failure(new Error(s"Incorrect suffix ${proof.suffix.length} != ${proof.k}"))
    } else if (proof.k < 1) {
      Failure(new Error(s"k should positive, ${proof.k} given"))
    } else if (proof.m < 1) {
      Failure(new Error(s"m should positive, ${proof.m} given"))
    } else if (!(proof.suffix.head.interlinks(proof.i) sameElements proof.innerchain.last.id)) {
      Failure(new Error(s"Incorrect link form suffix to innerchain in $proof"))
    } else if (proof.innerchain.length < proof.m) {
      Failure(new Error(s"Innerchain length is not enough in $proof"))
    } else if (!proof.innerchain.forall(_.realDifficulty >= innerDifficulty)) {
      Failure(new Error(s"Innerchain difficulty is not enough in $proof"))
    } else if (!proof.suffix.sliding(2).filter(_.length == 2).forall(s => s(1).parentId sameElements s.head.id)) {
      Failure(new Error(s"Suffix links are incorrect in $proof"))
    } else if (!proof.innerchain.sliding(2).filter(_.length == 2)
      .forall(s => s(1).interlinks(proof.i) sameElements s.head.id)) {
      Failure(new Error(s"Innerchain links are incorrect in $proof"))
    } else {
      Success()
    }
  }

  def constructInterlinkVector(parent: Header): Seq[Array[Byte]] = {
    if (parent.height == 0) {                            //genesis block
      Seq(parent.id)
    } else {

      val genesisId = parent.interlinks.head

      def generateInnerchain(curDifficulty: BigInt, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = {
        if (parent.realDifficulty >= curDifficulty) {
          generateInnerchain(curDifficulty * 2, acc :+ parent.id)
        } else {
          parent.interlinks.find(pId => Algos.blockIdDifficulty(pId) >= curDifficulty) match {
            case Some(id) if !(id sameElements genesisId) => generateInnerchain(curDifficulty * 2, acc :+ id)
            case _ => acc
          }
        }
      }

      genesisId +: generateInnerchain(Constants.InitialDifficulty * 2, Seq[Array[Byte]]())
    }
  }

}

object PoPoWProofSerializer extends Serializer[PoPoWProof] {
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

  override def parseBytes(bytes: Array[Byte]): Try[PoPoWProof] = Try {
    val m = bytes.head
    val k = bytes(1)
    val i = bytes(2)
    val headSuffixLength = Shorts.fromByteArray(bytes.slice(3, 5))
    val headSuffix = HeaderSerializer.parseBytes(bytes.slice(5, 5 + headSuffixLength)).get

    def parseSuffixes(index: Int, acc: Seq[Header]): (Int, Seq[Header]) = {
      if (acc.length == k) (index, acc.reverse)
      else {
        val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val headerWithoutInterlinks = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
        val interlinks = PoPoWProof.constructInterlinkVector(acc.head)
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
    PoPoWProof(m, k, i, innerchain, suffix)
  }
}


