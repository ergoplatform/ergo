package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.settings.Constants
import scorex.util.{ModifierId, bytesToId, idToBytes}

object PoPowAlgos {

  val InterlinksPrefixCode: Byte = 0x01

  @inline def maxLevelOf(header: Header): Int = {
    if (!header.isGenesis) {
      def log2(x: Double) = math.log(x) / math.log(2)
      val requiredTarget = org.ergoplatform.mining.q / RequiredDifficulty.decodeCompactBits(header.nBits)
      val realTarget = header.powSolution.d
      val level = log2(requiredTarget.doubleValue) - log2(realTarget.doubleValue)
      level.toInt
    } else {
      Int.MaxValue
    }
  }

  /**
    * Computes interlinks vector for the next level after `prevHeader`.
    */
  @inline def updateInterlinks(prevHeader: Header, prevInterlinks: Seq[ModifierId]): Seq[ModifierId] = {
    if (!prevHeader.isGenesis) {
      assert(prevInterlinks.nonEmpty, "Interlinks vector could not be empty in case of non-genesis header")
      val genesis = prevInterlinks.head
      val tail = prevInterlinks.tail
      val prevLevel = maxLevelOf(prevHeader)
      if (prevLevel > 0) {
        (genesis +: tail.dropRight(prevLevel)) ++ Seq.fill(prevLevel)(prevHeader.id)
      } else {
        prevInterlinks
      }
    } else {
      Seq(prevHeader.id)
    }
  }

  /**
    * Packs interlinks into extension key-value format.
    */
  @inline def packInterlinks(links: Seq[ModifierId]): Seq[(Array[Byte], Array[Byte])] = {
    def loop(rem: Seq[(ModifierId, Int)],
             acc: Seq[(Array[Byte], Array[Byte])]): Seq[(Array[Byte], Array[Byte])] = {
      rem match {
        case (headLink, idx) :: _ =>
          val duplicatesQty = links.count(_ == headLink)
          val filed = Array(InterlinksPrefixCode, idx.toByte) -> (duplicatesQty.toByte +: idToBytes(headLink))
          loop(rem.drop(duplicatesQty), acc :+ filed)
        case Nil =>
          acc
      }
    }
    loop(links.zipWithIndex, Seq.empty)
  }

  /**
    * Unpacks interlinks from key-value format of extension.
    */
  @inline def unpackInterlinks(fields: Seq[(Array[Byte], Array[Byte])]): Seq[ModifierId] = {
    fields
      .filter(_._1.headOption.contains(InterlinksPrefixCode))
      .foldLeft(Seq.empty[ModifierId]) { case (acc, (_, v)) =>
        assert(v.lengthCompare(Constants.ModifierIdSize + 1) == 0)
        val duplicatesQty = v.head.toInt
        val link = bytesToId(v.tail)
        acc ++ Seq.fill(duplicatesQty)(link)
      }
  }

}
