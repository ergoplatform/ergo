package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.settings.NiPoPowSettings
import scorex.util.ModifierId

class NiPoPowAlgos(settings: NiPoPowSettings) {

  import NiPoPowAlgos._

  private val m = settings.m
  private val k = settings.k
  private val k1 = settings.k1
  private val d = settings.d

  def prove(chain: Seq[Header]): NiPoPowProof = {
    assert(chain.lengthCompare(k) >= 0, s"Can not prove chain of size < $k")
    assert(chain.head.isGenesis, "Can not prove not anchored chain")
    def provePrefix(anchoringPoint: Header, level: Int, acc: Seq[Header] = Seq.empty): Seq[Header] = {
      if (level >= 0) {
        val subChain = chain.dropRight(k)
          .filter(h => maxLevelOf(h) >= level && h.height >= anchoringPoint.height) // C[:−k]{B:}↑µ
        if (subChain.size >= m && goodSuperChain(chain, subChain, level)) {
          provePrefix(subChain(subChain.size - m), level - 1, acc ++ subChain)
        } else {
          provePrefix(anchoringPoint, level - 1, acc ++ subChain)
        }
      } else {
        acc
      }
    }
    val suffix = chain.takeRight(k)
    val prefix = provePrefix(chain.head, suffix.head.interlinks.size - 1).distinct.sortBy(_.height)
    NiPoPowProof(m, k, prefix, suffix)
  }

  def goodSuperChain(chain: Seq[Header], superChain: Seq[Header], level: Int): Boolean = {
    superChainQuality(chain, superChain, level) && multiLevelQuality(chain, superChain, level)
  }

  private def locallyGood(superChainSize: Int, underlyingChainSize: Int, level: Int): Boolean = {
    superChainSize > ((1 - d) * math.pow(2, -level) * underlyingChainSize)
  }

  /** @param chain      - Full chain (C)
    * @param superChain - Super-chain of level µ (C↑µ)
    * @param level      - Level of super-chain (µ) */
  private def superChainQuality(chain: Seq[Header], superChain: Seq[Header], level: Int): Boolean = {
    val downChain = chain
      .filter(h => h.height >= superChain.head.height && h.height <= superChain.last.height) // C[C↑µ[0]:C↑µ[−1]] or C'↓
    def checkLocalGoodnessAt(mValue: Int): Boolean = {
      val superChainSuffixSize = superChain.takeRight(mValue).size
      val downChainSuffixSize = downChain.takeRight(mValue).size
      mValue match {
        case mToTest if mToTest < chain.size &&
          locallyGood(math.min(superChainSuffixSize, mToTest), math.min(downChainSuffixSize, mToTest), level) =>
          checkLocalGoodnessAt(mToTest + 1)
        case mToTest if mToTest < chain.size =>
          false
        case _ =>
          true
      }
    }
    checkLocalGoodnessAt(m)
  }

  private def multiLevelQuality(chain: Seq[Header], superChain: Seq[Header], level: Int): Boolean = {
    val downChain = chain.dropWhile(_ == superChain.head).takeWhile(_ == superChain.last) // C'↓
    def checkQualityAt(levelToCheck: Int): Boolean = {
      levelToCheck match {
        case lvl if lvl > 0 =>
          val subChain = downChain.filter(maxLevelOf(_) >= lvl - 1) // C* = C'↓↑µ'−1
          if (subChain.nonEmpty) {
            val upperSubChainSize = subChain.count(maxLevelOf(_) >= lvl) // |C*↑µ'|
            if (upperSubChainSize >= k1 &&
              !(subChain.count(maxLevelOf(_) >= level) >= (1 - d) * math.pow(2, level - lvl) * upperSubChainSize)) {
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
    }
    checkQualityAt(level)
  }

}

object NiPoPowAlgos {

  def maxLevelOf(header: Header): Int = {
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

  def lowestCommonAncestor(leftChain: Seq[Header], rightChain: Seq[Header]): Option[Header] = {
    def lcaIndex(startIdx: Int): Int = {
      if (leftChain.lengthCompare(startIdx) <= 0 && rightChain.lengthCompare(startIdx) <= 0 &&
        leftChain(startIdx) == rightChain(startIdx)) {
        lcaIndex(startIdx + 1)
      } else {
        startIdx - 1
      }
    }
    if (leftChain.headOption.exists(rightChain.headOption.contains(_))) Some(leftChain(lcaIndex(1))) else None
  }

  def updateInterlinks(header: Header): Seq[ModifierId] = {
    if (!header.isGenesis) {
      val genesis = header.interlinks.head
      val tail = header.interlinks.tail
      val prevLevel = maxLevelOf(header)
      if (prevLevel > 0) {
        (genesis +: tail.dropRight(prevLevel)) ++ Seq.fill(prevLevel)(header.id)
      } else {
        header.interlinks
      }
    } else {
      Seq(header.id)
    }
  }

  def bestArg(chain: Seq[Header])(m: Int): Int = {
    def loop(level: Int, acc: Seq[(Int, Int)] = Seq.empty): Seq[(Int, Int)] = {
      if (level == 0) {
        loop(level + 1, acc :+ (0, chain.size)) // Supposing each header is at least of level 0.
      } else {
        val args = chain.filter(maxLevelOf(_) >= level)
        if (args.lengthCompare(m) >= 0) loop(level + 1, acc :+ (level, args.size)) else acc
      }
    }
    loop(level = 0).map { case (lvl, size) =>
      math.pow(2, lvl) * size // 2^µ * |C↑µ|
    }.max.toInt
  }

}
