package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Shorts}
import io.circe.Json
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

import scala.util.Try

case class PoPoWProof(m: Byte,
                      k: Byte,
                      i: Byte,
                      innerchain: Seq[Header],
                      suffix: Seq[Header]) extends Comparable[PoPoWProof] with Ordered[PoPoWProof]
  with ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = PoPoWProof.ModifierTypeId

  override lazy val id: ModifierId = Algos.hash(bytes)

  override type M = PoPoWProof

  override lazy val serializer: Serializer[PoPoWProof] = ???

  override lazy val json: Json = ???

  override def compare(that: PoPoWProof): Int = ???

  lazy val validate: Try[Unit] = Try {
    require(suffix.length == k, s"${suffix.length} == $k")

    suffix.foldRight(Array[Byte]()) { (a, b) =>
      if (b.nonEmpty) require(b sameElements a.id)
      a.parentId
    }

    require(suffix.head.interlinks(i) sameElements innerchain.last.id)

    val difficulty: BigInt = Constants.InitialDifficulty * Math.pow(2, i).toInt
    require(innerchain.length >= m, s"${innerchain.length} >= $m")
    innerchain.foreach(b => require(b.realDifficulty >= difficulty, s"$b: ${b.realDifficulty} >= $difficulty"))

    innerchain.foldRight(Array[Byte]()) { (a, b) =>
      if (b.nonEmpty) {
        require(b sameElements a.id)

      }
      //last element may not contain a.interlinks(i)
      Try(a.interlinks(i)).getOrElse(Array.fill(32)(0.toByte))
    }

    //TODO check that genesis links are correct
  }

}

object PoPoWProof {
  val ModifierTypeId: Byte = 105: Byte

  def constructInterlinkVector(parent: Header): Seq[Array[Byte]] = {
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

object PoPoWProofSerializer extends Serializer[PoPoWProof] {
  override def toBytes(obj: PoPoWProof): Array[Byte] = {
    val suffixTailBytes = scorex.core.utils.concatBytes(obj.suffix.tail.map { h =>
      val bytes = HeaderSerializer.bytesWithoutInterlinks(h)
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    })
    val interchainBytes = scorex.core.utils.concatBytes(obj.innerchain.map { h =>
      val bytes = h.bytes
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    })
    Bytes.concat(Array(obj.m, obj.k, obj.i),
      Shorts.toByteArray(obj.suffix.head.bytes.length.toShort),
      obj.suffix.head.bytes,
      suffixTailBytes,
      Shorts.toByteArray(obj.innerchain.length.toShort),
      interchainBytes)
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
    val interchainLength = Shorts.fromByteArray(bytes.slice(index, index + 2))
    index = index + 2
    val innerchain = (0 until interchainLength) map { _ =>
      val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
      val header = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
      index = index + 2 + l
      header
    }
    PoPoWProof(m, k, i, innerchain, suffix)
  }
}


