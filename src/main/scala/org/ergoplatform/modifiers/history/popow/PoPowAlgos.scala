package org.ergoplatform.modifiers.history.popow

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history.Extension.InterlinksVectorPrefix
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate, Header}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.Constants
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  *
  * @param m
  * @param k
  */
case class PoPowParams(m: Int, k: Int)


/**
  * A set of utilities for working with NiPoPoW protocol.
  *
  * Based on papers:
  *
  * [KMZ17] Non-Interactive Proofs of Proof-of-Work https://fc20.ifca.ai/preproceedings/74.pdf
  *
  * [KLS16] Proofs of Proofs of Work with Sublinear Complexity http://fc16.ifca.ai/bitcoin/papers/KLS16.pdf
  *
  * Please note that for [KMZ17] we're using the version published @ Financial Cryptography 2020, which is different
  * from previously published versions on IACR eprint.
  */
object PoPowAlgos {

  private def log2(x: Double): Double = math.log(x) / math.log(2)

  /**
    * Computes interlinks vector for a header next to `prevHeader`.
    */
  @inline def updateInterlinks(prevHeaderOpt: Option[Header], prevExtensionOpt: Option[Extension]): Seq[ModifierId] =
    prevHeaderOpt
      .flatMap { prevHeader =>
        prevExtensionOpt
          .flatMap(ext => unpackInterlinks(ext.fields).toOption)
          .map(updateInterlinks(prevHeader, _))
      }
      .getOrElse(Seq.empty)

  /**
    * Computes interlinks vector for a header next to `prevHeader`.
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
    * Packs interlinks into key-value format of the block extension.
    */
  @inline def packInterlinks(links: Seq[ModifierId]): Seq[(Array[Byte], Array[Byte])] = {
    @scala.annotation.tailrec
    def loop(rem: List[(ModifierId, Int)],
             acc: Seq[(Array[Byte], Array[Byte])]): Seq[(Array[Byte], Array[Byte])] =
      rem match {
        case (headLink, idx) :: _ =>
          val duplicatesQty = links.count(_ == headLink)
          val filled = Array(InterlinksVectorPrefix, idx.toByte) -> (duplicatesQty.toByte +: idToBytes(headLink))
          loop(rem.drop(duplicatesQty), acc :+ filled)
        case Nil =>
          acc
      }

    loop(links.zipWithIndex.toList, Seq.empty)
  }

  @inline def interlinksToExtension(links: Seq[ModifierId]): ExtensionCandidate =
    ExtensionCandidate(packInterlinks(links))

  /**
    * Unpacks interlinks from key-value format of block extension.
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
      val requiredTarget = org.ergoplatform.mining.q / RequiredDifficulty.decodeCompactBits(header.nBits)
      val realTarget = header.powSolution.d
      val level = log2(requiredTarget.doubleValue) - log2(realTarget.doubleValue)
      level.toInt
    } else {
      Int.MaxValue
    }

  /**
    * Computes best score of a given chain.
    * The score value depends on number of µ-superblocks in the given chain.
    *
    * see [KMZ17], Algorithm 4
    *
    * [KMZ17]:
    * "To find the best argument of a proof π given b, best-arg_m collects all the μ
    * indices which point to superblock levels that contain valid arguments after block b.
    * Argument validity requires that there are at least m μ-superblocks following block b,
    * which is captured by the comparison|π↑μ{b:}|≥m. 0 is always considered a valid level,
    * regardless of how many blocks are present there. These level indices are collected into set M.
    * For each of these levels, the score of their respective argument is evaluated by weighting the
    * number of blocks by the level as 2μ|π↑μ{b:}|. The highest possible score across all levels is returned."
    *
    * function best-arg_m(π, b)
    * M←{μ:|π↑μ{b:}|≥m}∪{0}
    * return max_{μ∈M} {2μ·|π↑μ{b:}|}
    * end function
    */
  def bestArg(chain: Seq[Header])(m: Int): Int = {
    @scala.annotation.tailrec
    def loop(level: Int, acc: Seq[(Int, Int)] = Seq.empty): Seq[(Int, Int)] =
      if (level == 0) {
        loop(level + 1, (0, chain.size) +: acc) // Supposing each header is at least of level 0.
      } else {
        val args = chain.filter(maxLevelOf(_) >= level)
        if (args.lengthCompare(m) >= 0) loop(level + 1, (level, args.size) +: acc) else acc
      }

    loop(level = 0).map { case (lvl, size) =>
      math.pow(2, lvl) * size // 2^µ * |C↑µ|
    }.max.toInt
  }

  /**
    * Finds the last common header (branching point) between `leftChain` and `rightChain`.
    */
  def lowestCommonAncestor(leftChain: Seq[Header], rightChain: Seq[Header]): Option[Header] = {
    @scala.annotation.tailrec
    def lcaIndex(startIdx: Int): Int =
      if (leftChain.lengthCompare(startIdx) >= 0 && rightChain.lengthCompare(startIdx) >= 0 &&
        leftChain(startIdx) == rightChain(startIdx)) {
        lcaIndex(startIdx + 1)
      } else {
        startIdx - 1
      }

    if (leftChain.headOption.exists(rightChain.headOption.contains(_))) Some(leftChain(lcaIndex(1))) else None
  }

  /**
    * Computes NiPoPow proof for the given `chain` according to given `params`.
    */
  def prove(chain: Seq[PoPowHeader])(params: PoPowParams): PoPowProof = {
    val k = params.k
    val m = params.m

    require(params.k >= 0, s"$k < 0")
    require(chain.lengthCompare(k + m) >= 0, s"Can not prove chain of size < ${k + m}")
    require(chain.head.header.isGenesis, "Can not prove non-anchored chain")

    @scala.annotation.tailrec
    def provePrefix(anchoringPoint: PoPowHeader,
                    level: Int,
                    acc: Seq[PoPowHeader] = Seq.empty): Seq[PoPowHeader] =
      if (level >= 0) {
        val subChain = chain.dropRight(params.k)
          .filter(h => maxLevelOf(h.header) >= level && h.height >= anchoringPoint.height) // C[:−k]{B:}↑µ
        if (m < subChain.size) {
          provePrefix(subChain(subChain.size - params.m), level - 1, acc ++ subChain)
        } else {
          provePrefix(anchoringPoint, level - 1, acc ++ subChain)
        }
      } else {
        acc
      }

    val suffix = chain.takeRight(params.k).map(_.header)
    val maxLevel = chain.dropRight(params.k).last.interlinks.size - 1
    val prefix = provePrefix(chain.head, maxLevel).distinct.sortBy(_.height)
    PoPowProof(m, k, prefix, suffix)
  }

  def prove(histReader: ErgoHistoryReader)(params: PoPowParams): PoPowProof = {
    type Height = Int

    val k = params.k
    val m = params.m

    require(params.k >= 0, s"$k < 0")
    require(histReader.headersHeight >= k + m, s"Can not prove chain of size < ${k + m}")

    def linksWithIndexes(header: PoPowHeader): Seq[(ModifierId, Int)] = header.interlinks.tail.reverse.zipWithIndex

    def previousHeaderIdAtLevel(level: Int, currentHeader: PoPowHeader): Option[ModifierId] = {
      linksWithIndexes(currentHeader).find(_._2 == level).map(_._1)
    }

    @scala.annotation.tailrec
    def collectLevel(nextHeaderId: ModifierId,
                     level: Int,
                     anchoringHeight: Height,
                     acc: Seq[PoPowHeader] = Seq.empty): Seq[PoPowHeader] = {
      val nextHeader = histReader.popowHeader(nextHeaderId).get
      if (nextHeader.height < anchoringHeight) {
        acc
      } else {
        val newAcc = nextHeader +: acc
        previousHeaderIdAtLevel(level, nextHeader) match {
          case Some(newPrevHeaderId) => collectLevel(newPrevHeaderId, level, anchoringHeight, newAcc)
          case None => newAcc
        }
      }
    }

    def provePrefix(initAnchoringHeight: Height,
                    lastHeader: PoPowHeader): Seq[PoPowHeader] = {

      val collected = mutable.TreeMap[ModifierId, PoPowHeader]()

      val levels = linksWithIndexes(lastHeader)
      levels.foldRight(initAnchoringHeight) { case ((prevHeaderId, levelIdx), anchoringHeight) =>
        val levelHeaders = collectLevel(prevHeaderId, levelIdx, anchoringHeight)
        levelHeaders.foreach(ph => collected.update(ph.id, ph))
        if (m < levelHeaders.length) levelHeaders(levelHeaders.length - m).height
        else anchoringHeight
      }
      collected.values.toSeq
    }

    val suffix = histReader.lastHeaders(k).headers
    val kMinusOnePopowHeader = histReader.popowHeader(suffix.head.parentId).get

    val genesisHeight = 1
    val prefix = provePrefix(genesisHeight, kMinusOnePopowHeader)

    PoPowProof(m, k, prefix, suffix)
  }

}
