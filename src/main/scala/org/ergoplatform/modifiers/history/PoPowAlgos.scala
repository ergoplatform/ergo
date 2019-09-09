package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history.Extension.InterlinksVectorPrefix
import org.ergoplatform.settings.{Constants, PoPowParams}
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.{Failure, Success, Try}

/**
  * A set of utilities for working with PoPoW security protocol.
  */
object PoPowAlgos {

  /**
    * Computes interlinks vector for the next level after `prevHeader`.
    */
  @inline def updateInterlinks(prevHeaderOpt: Option[Header], prevExtensionOpt: Option[Extension]): Seq[ModifierId] =
    prevHeaderOpt
      .flatMap { h =>
        prevExtensionOpt
          .flatMap(ext => unpackInterlinks(ext.fields).toOption)
          .map(updateInterlinks(h, _))
      }
      .getOrElse(Seq.empty)

  /**
    * Computes interlinks vector for the next level after `prevHeader`.
    */
  @inline def updateInterlinks(prevHeader: Header, prevInterlinks: Seq[ModifierId]): Seq[ModifierId] =
    if (!prevHeader.isGenesis) {
      require(prevInterlinks.nonEmpty, "Interlinks vector could not be empty in case of non-genesis header")
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

  /**
    * Packs interlinks into extension key-value format.
    */
  @inline def packInterlinks(links: Seq[ModifierId]): Seq[(Array[Byte], Array[Byte])] = {
    @scala.annotation.tailrec
    def loop(rem: List[(ModifierId, Int)],
             acc: Seq[(Array[Byte], Array[Byte])]): Seq[(Array[Byte], Array[Byte])] =
      rem match {
        case (headLink, idx) :: _ =>
          val duplicatesQty = links.count(_ == headLink)
          val filed = Array(InterlinksVectorPrefix, idx.toByte) -> (duplicatesQty.toByte +: idToBytes(headLink))
          loop(rem.drop(duplicatesQty), acc :+ filed)
        case Nil =>
          acc
      }

    loop(links.zipWithIndex.toList, Seq.empty)
  }

  @inline def interlinksToExtension(links: Seq[ModifierId]): ExtensionCandidate =
    ExtensionCandidate(packInterlinks(links))

  /**
    * Unpacks interlinks from key-value format of extension.
    */
  @inline def unpackInterlinks(fields: Seq[(Array[Byte], Array[Byte])]): Try[Seq[ModifierId]] = {
    @scala.annotation.tailrec
    def loop(rem: List[(Array[Byte], Array[Byte])],
             acc: Seq[ModifierId] = Seq.empty): Try[Seq[ModifierId]] =
      rem match {
        case head :: tail =>
          val value = head._2
          if (value.lengthCompare(Constants.ModifierIdSize + 1) == 0) {
            val duplicatesQty = 0xff & value.head.toInt
            val link = bytesToId(value.tail)
            loop(tail, acc ++ Seq.fill(duplicatesQty)(link))
          } else {
            Failure(new Exception("Interlinks improperly packed"))
          }
        case Nil =>
          Success(acc)
      }

    loop(fields.filter(_._1.headOption.contains(InterlinksVectorPrefix)).toList)
  }

  /**
    * Computes max level (μ) of the given [[Header]], such that μ = log(T) − log(id(B))
    */
  def maxLevelOf(header: Header): Int =
    if (!header.isGenesis) {
      def log2(x: Double) = math.log(x) / math.log(2)

      val requiredTarget = org.ergoplatform.mining.q / RequiredDifficulty.decodeCompactBits(header.nBits)
      val realTarget = header.powSolution.d
      val level = log2(requiredTarget.doubleValue) - log2(realTarget.doubleValue)
      level.toInt
    } else {
      Int.MaxValue
    }

  def bestArg(chain: Seq[Header])(m: Int): Int = {
    @scala.annotation.tailrec
    def loop(level: Int, acc: Seq[(Int, Int)] = Seq.empty): Seq[(Int, Int)] =
      if (level == 0) {
        loop(level + 1, acc :+ (0, chain.size)) // Supposing each header is at least of level 0.
      } else {
        val args = chain.filter(maxLevelOf(_) >= level)
        if (args.lengthCompare(m) >= 0) loop(level + 1, acc :+ (level, args.size)) else acc
      }
    loop(level = 0).map { case (lvl, size) =>
      math.pow(2, lvl) * size // 2^µ * |C↑µ|
    }.max.toInt
  }

  def lowestCommonAncestor(leftChain: Seq[Header], rightChain: Seq[Header]): Option[Header] = {
    @scala.annotation.tailrec
    def lcaIndex(startIdx: Int): Int =
      if (leftChain.lengthCompare(startIdx) <= 0 && rightChain.lengthCompare(startIdx) <= 0 &&
        leftChain(startIdx) == rightChain(startIdx)) {
        lcaIndex(startIdx + 1)
      } else {
        startIdx - 1
      }
    if (leftChain.headOption.exists(rightChain.headOption.contains(_))) Some(leftChain(lcaIndex(1))) else None
  }

  def prove(chain: Seq[PoPowHeader])(params: PoPowParams): PoPowProof = {
    require(chain.lengthCompare(params.k) >= 0, s"Can not prove chain of size < ${params.k}")
    require(chain.head.header.isGenesis, "Can not prove non-anchored chain")
    @scala.annotation.tailrec
    def provePrefix(anchoringPoint: PoPowHeader, level: Int,acc: Seq[PoPowHeader] = Seq.empty): Seq[PoPowHeader] =
      if (level >= 0) {
        val subChain = chain.dropRight(params.k)
          .filter(h => maxLevelOf(h.header) >= level && h.height >= anchoringPoint.height) // C[:−k]{B:}↑µ
        val goodChain = goodSuperChain(chain.map(_.header), subChain.map(_.header), level)(params)
        if (subChain.size >= params.m && goodChain) {
          provePrefix(subChain(subChain.size - params.m), level - 1, acc ++ subChain)
        } else {
          provePrefix(anchoringPoint, level - 1, acc ++ subChain)
        }
      } else {
        acc
      }
    val suffix = chain.takeRight(params.k)
    val maxLevel = suffix.head.interlinks.size - 1
    val prefix = provePrefix(chain.head, maxLevel).distinct.sortBy(_.height)
    PoPowProof(params.m, params.k, prefix, suffix)
  }

  def goodSuperChain(chain: Seq[Header], superChain: Seq[Header], level: Int)(params: PoPowParams): Boolean =
    superChainQuality(chain, superChain, level)(params) && multiLevelQuality(chain, superChain, level)(params)

  def locallyGood(superChainSize: Int, underlyingChainSize: Int, level: Int, d: Double): Boolean =
    superChainSize > ((1 - d) * math.pow(2, -level) * underlyingChainSize)

  /**
    * @param chain      - Full chain (C)
    * @param superChain - Super-chain of level µ (C↑µ)
    * @param level      - Level of super-chain (µ)
    */
  def superChainQuality(chain: Seq[Header], superChain: Seq[Header], level: Int)
                       (params: PoPowParams): Boolean = {
    val downChain = chain
      .filter(h => h.height >= superChain.head.height && h.height <= superChain.last.height) // C[C↑µ[0]:C↑µ[−1]] or C'↓
    @scala.annotation.tailrec
    def checkLocalGoodnessAt(mValue: Int): Boolean = {
      val superChainSuffixSize = superChain.takeRight(mValue).size
      val downChainSuffixSize = downChain.takeRight(mValue).size
      def isLocallyGood(m: Int) = locallyGood(
        math.min(superChainSuffixSize, m), math.min(downChainSuffixSize, m), level, params.d)
      mValue match {
        case mToTest if mToTest < chain.size && isLocallyGood(mToTest)=>
          checkLocalGoodnessAt(mToTest + 1)
        case mToTest if mToTest < chain.size =>
          false
        case _ =>
          true
      }
    }
    checkLocalGoodnessAt(params.m)
  }

  def multiLevelQuality(chain: Seq[Header], superChain: Seq[Header], level: Int)
                       (params: PoPowParams): Boolean = {
    val downChain = chain.dropWhile(_ == superChain.head).takeWhile(_ == superChain.last) // C'↓
    @scala.annotation.tailrec
    def checkQualityAt(levelToCheck: Int): Boolean =
      levelToCheck match {
        case lvl if lvl > 0 =>
          val subChain = downChain.filter(maxLevelOf(_) >= lvl - 1) // C* = C'↓↑µ'−1
          if (subChain.nonEmpty) {
            val upperSubChainSize = subChain.count(maxLevelOf(_) >= lvl) // |C*↑µ'|
            if (upperSubChainSize >= params.k1 &&
              !(subChain.count(maxLevelOf(_) >= level) >= (1 - params.d) * math.pow(2, level - lvl) * upperSubChainSize)) {
              false
            } else {
              checkQualityAt(lvl - 1)
            }
          } else {
            checkQualityAt(lvl - 1)
          }
        case _ =>
          true
      }
    checkQualityAt(level)
  }

}
