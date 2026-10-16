package org.ergoplatform.modifiers.history.popow

import org.ergoplatform.mining.difficulty.DifficultyAdjustment
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.ChainSettings
import scorex.util.ModifierId

import scala.collection.mutable
import scala.util.Try

/**
  * Container for NiPoPoW methods working with blockchain database instead of in-memory collections,
  * for performance's sake.
  */
object NipopowProverWithDbAlgs {

  /**
   * Computes NiPoPow proof for the chain stored in `histReader`'s database,
   * or a prefix of the chain which contains a specific header (if `headerIdOpt` is specified).
   * In the latter case, header will be the first header of the suffix of length `k`
   * (`suffixHead` field of the result). Fails if fewer than `k - 1` connected successors are available.
   */
  def prove(histReader: ErgoHistoryReader,
            headerIdOpt: Option[ModifierId] = None, chainSettings: ChainSettings)(params: PoPowParams): Try[NipopowProof] = {
    val diffAdjustment = new DifficultyAdjustment(chainSettings)
    Try {
      type Height = Int

      val k = params.k
      val m = params.m

      require(histReader.headersHeight >= k + m, s"Can not prove chain of size < ${k + m}")

      def linksWithIndexes(header: PoPowHeader): Seq[(ModifierId, Int)] = header.interlinks.tail.reverse.zipWithIndex

      def previousHeaderIdAtLevel(level: Int, currentHeader: PoPowHeader): Option[ModifierId] = {
        linksWithIndexes(currentHeader).find(_._2 == level).map(_._1)
      }

      @scala.annotation.tailrec
      def collectLevel(prevHeaderId: ModifierId,
                       level: Int,
                       anchoringHeight: Height,
                       acc: Seq[PoPowHeader] = Seq.empty): Seq[PoPowHeader] = {
        val prevHeader = histReader.popowHeader(prevHeaderId).get // to be caught in outer (prove's) Try
        if (prevHeader.height < anchoringHeight) {
          acc
        } else {
          val newAcc = prevHeader +: acc
          previousHeaderIdAtLevel(level, prevHeader) match {
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
          if (m < levelHeaders.length) {
            levelHeaders(levelHeaders.length - m).height
          } else {
            anchoringHeight
          }
        }
        collected.values.toSeq
      }

      val (suffixHead, suffixTail) = headerIdOpt match {
        case Some(headerId) =>
          val suffixHead = histReader.popowHeader(headerId).get // to be caught in outer (prove's) Try
          val suffixTail = histReader.bestHeadersAfter(suffixHead.header, k - 1)
          suffixHead -> suffixTail
        case None =>
          val suffix = histReader.lastHeaders(k).headers
          histReader.popowHeader(suffix.head.id).get -> suffix.tail // .get to be caught in outer (prove's) Try
      }
      require(suffixTail.lengthCompare(k - 1) == 0,
        s"Cannot construct NiPoPoW proof with suffix length ${suffixTail.size + 1}: expected $k")
      require((suffixHead.header +: suffixTail).zip(suffixTail).forall {
        case (parent, child) => child.parentId == parent.id
      }, "Cannot construct NiPoPoW proof with disconnected suffix")

      val storedHeights = mutable.Set[Height]() // cache to filter out duplicate headers
      val prefixBuilder = mutable.ArrayBuilder.make[PoPowHeader]()

      val genesisHeight = 1
      prefixBuilder += histReader.popowHeader(genesisHeight).get // to be caught in outer (prove's) Try
      storedHeights += genesisHeight

      if (params.continuous) {
        // put headers needed to check difficulty of new blocks after suffix into prefix
        val epochLength = chainSettings.eip37EpochLength.getOrElse(chainSettings.epochLength)
        val requiredHeights = diffAdjustment.heightsForNextRecalculation(suffixHead.height, epochLength)
          .filter(height => height > 0 && height < suffixHead.height).toVector

        // A requested suffix head can belong to a retained fork. Resolve difficulty headers
        // on that head's ancestry, rather than replacing them with selected-chain headers
        // at the same heights. Materialize the selected fast path under the history lock so
        // a concurrent chain switch cannot change the height lookups midway through.
        val selectedHeaders = histReader.synchronized {
          if (histReader.bestHeaderIdAtHeight(suffixHead.height).contains(suffixHead.id)) {
            Some(requiredHeights.map { height =>
              histReader.popowHeader(height).getOrElse(
                throw new IllegalArgumentException(s"Missing NiPoPoW difficulty header at height $height"))
            })
          } else {
            None
          }
        }
        val difficultyHeaders = selectedHeaders.getOrElse {
          if (requiredHeights.isEmpty) {
            Vector.empty[PoPowHeader]
          } else {
            val minHeight = requiredHeights.min
            val ancestors = histReader.headerChainBack(
              suffixHead.height - minHeight + 1, suffixHead.header, _ => false
            ).headers.map(header => header.height -> header.id).toMap
            requiredHeights.map { height =>
              val id = ancestors.getOrElse(height,
                throw new IllegalArgumentException(s"Missing NiPoPoW difficulty ancestor at height $height"))
              histReader.popowHeader(id).getOrElse(
                throw new IllegalArgumentException(s"Missing NiPoPoW difficulty header at height $height"))
            }
          }
        }
        difficultyHeaders.foreach { ph =>
          prefixBuilder += ph
          storedHeights += ph.height
        }
      }

      provePrefix(genesisHeight, suffixHead).foreach { ph =>
        if (!storedHeights.contains(ph.height)) {
          prefixBuilder += ph
          storedHeights += ph.height
        }
      }

      val prefix = prefixBuilder.result().sortBy(_.height)

      NipopowProof(new NipopowAlgos(chainSettings), m, k, prefix, suffixHead, suffixTail, params.continuous)
    }
  }

}
