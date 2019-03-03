package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor.{BlockProcessing, ToProcess}
import org.ergoplatform.settings.Algos
import scorex.core.consensus.History.ProgressInfo
import scorex.util.{ModifierId, bytesToId}

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.util.Try

/**
  * Contains functions required by History to process Transactions and Proofs when we have them.
  * Prune modifiers older then blocksToKeep.
  */
trait FullBlockProcessor extends HeadersProcessor {

  private var fullChainMonitor = FullBlockProcessor.emptyMonitor

  /**
    * Id of header that contains transactions and proofs
    */
  override def bestFullBlockIdOpt: Option[ModifierId] = historyStorage.getIndex(BestFullBlockKey)
    .map(w => bytesToId(w.data))

  protected def getFullBlock(h: Header): Option[ErgoFullBlock]

  protected def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain)

  /** Process full block when we have one.
    *
    * @param fullBlock - block to process
    * @param newMod    - new modifier we are going to put in history
    * @return ProgressInfo required for State to process to be consistent with History
    */
  protected def processFullBlock(fullBlock: ErgoFullBlock,
                                 newMod: ErgoPersistentModifier): ProgressInfo[ErgoPersistentModifier] = {
    val bestFullChain = calculateBestFullChain(fullBlock.header)
    val newBestBlockId = bestFullChain.last
    processing(ToProcess(fullBlock, newMod, newBestBlockId, config.blocksToKeep, bestFullChain))
  }

  private def processing: BlockProcessing =
    processValidFirstBlock orElse
      processBetterChain orElse
      nonBestBlock

  private def processValidFirstBlock: BlockProcessing = {
    case ToProcess(fullBlock, newModRow, newBestBlockId, _, idsToApply)
      if isValidFirstFullBlock(fullBlock.header) =>

      val headerChain = headerChainBack(10, fullBlock.header, _.height == 1)
      val toApply = idsToApply.flatMap(id => typedModifierById[Header](id).flatMap(getFullBlock))
      logStatus(Seq(), toApply, fullBlock, None)
      updateStorage(newModRow, newBestBlockId)
      ProgressInfo(None, Seq.empty, headerChain.headers.dropRight(1) ++ toApply, Seq.empty)
  }

  private def processBetterChain: BlockProcessing = {
    case toProcess@ToProcess(fullBlock, newModRow, newBestBlockId, blocksToKeep, _)
      if bestFullBlockOpt.nonEmpty && isBetterChain(newBestBlockId) =>

      val prevBest = bestFullBlockOpt.get
      val newBestBlockHeader = typedModifierById[Header](newBestBlockId).get
      val (prevChain, newChain) = commonBlockThenSuffixes(prevBest.header, newBestBlockHeader)
      val toRemove: Seq[ErgoFullBlock] = prevChain.tail.headers.flatMap(getFullBlock)
      val toApply: Seq[ErgoFullBlock] = newChain.tail.headers
        .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getFullBlock(h))

      if (toApply.lengthCompare(newChain.length - 1) != 0) {
        //block have higher score but is not linkable to full chain
        nonBestBlock(toProcess)
      } else {
        //application of this block leads to full chain with higher score
        logStatus(toRemove, toApply, fullBlock, Some(prevBest))
        val branchPoint = toRemove.headOption.map(_ => prevChain.head.id)

        val minForkRootHeight = newBestBlockHeader.height - config.blocksToKeep
        // remove outdated blocks
        if (fullChainMonitor.nonEmpty) fullChainMonitor = fullChainMonitor.dropUntil(minForkRootHeight)

        updateStorage(newModRow, newBestBlockId)

        if (blocksToKeep >= 0) {
          val lastKept = pruningProcessor.updateBestFullBlock(fullBlock.header)
          val bestHeight: Int = newBestBlockHeader.height
          val diff = bestHeight - prevBest.header.height
          pruneBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
        }
        ProgressInfo(branchPoint, toRemove, toApply, Seq.empty)
      }
  }

  private def nonBestBlock: BlockProcessing = {
    case params =>
      val block = params.fullBlock
      if (block.header.height > bestFullBlockHeight - config.keepVersions) {
        fullChainMonitor = fullChainMonitor.add(block.id, block.parentId, block.header.height)
      }
      //Orphaned block or full chain is not initialized yet
      logStatus(Seq(), Seq(), params.fullBlock, None)
      historyStorage.insert(storageVersion(params.newModRow), Seq.empty, Seq(params.newModRow))
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  private def isValidFirstFullBlock(header: Header): Boolean = {
    pruningProcessor.isHeadersChainSynced &&
      header.height == pruningProcessor.minimalFullBlockHeight &&
      bestFullBlockIdOpt.isEmpty
  }

  /**
    *
    * @param id - id of a header to compare
    * @return `true`, if block with id `id` is better, than current best block, `false` otherwise.
    */
  private def isBetterChain(id: ModifierId): Boolean = {
    (bestFullBlockIdOpt.flatMap(bfi => scoreOf(bfi)), scoreOf(id)) match {
      case (Some(prevBestScore), Some(score)) if score > prevBestScore => true
      case _ => false
    }
  }

  private def continuationChains(fromHeader: Header): Seq[Seq[ModifierId]] = {
    @tailrec
    def loop(currentHeight: Option[Int], acc: Seq[Seq[(ModifierId, ModifierId)]]): Seq[Seq[ModifierId]] = {
      val nextLevelBlocks = currentHeight.toList
        .flatMap { h =>
          headerIdsAtHeight(h + 1)
            .flatMap { id =>
              fullChainMonitor.get(id, h + 1)
                .map(parentId => id -> parentId)
                .orElse {
                  typedModifierById[Header](id).flatMap(getFullBlock).map(b => b.id -> b.parentId)
                }
            }
        }
      if (nextLevelBlocks.isEmpty) {
        acc.map(chain => chain.map(_._1).reverse)
      } else {
        val updatedChains = nextLevelBlocks.flatMap { block =>
          acc.find(chain => chain.nonEmpty && (block._2 == chain.head._1)).map(c => block +: c)
        }
        val nonUpdatedChains = acc.filter(chain => !nextLevelBlocks.exists(_._2 == chain.head._1))
        loop(currentHeight.map(_ + 1), updatedChains ++ nonUpdatedChains)
      }
    }

    loop(heightOf(fromHeader.id), Seq(Seq(fromHeader.id -> fromHeader.parentId)))
  }

  private def calculateBestFullChain(header: Header): Seq[ModifierId] = {
    continuationChains(header)
      .map(_.tail)
      .map(header.id +: _)
      .maxBy(c => scoreOf(c.last))
  }

  private def logStatus(toRemove: Seq[ErgoFullBlock],
                        toApply: Seq[ErgoFullBlock],
                        appliedBlock: ErgoFullBlock,
                        prevBest: Option[ErgoFullBlock]): Unit = {
    val toRemoveStr = if (toRemove.isEmpty) "" else s" and to remove ${toRemove.length}"
    val newStatusStr = if (toApply.isEmpty) "" else {
      s" New best block is ${toApply.last.header.encodedId} " +
        s"with height ${toApply.last.header.height} " +
        s"updates block ${prevBest.map(_.encodedId).getOrElse("None")} " +
        s"with height ${ErgoHistory.heightOf(prevBest.map(_.header))}"
    }
    log.info(s"Full block ${appliedBlock.encodedId} appended, " +
      s"going to apply ${toApply.length}$toRemoveStr modifiers.$newStatusStr")
  }

  private def pruneBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights.flatMap(h => headerIdsAtHeight(h))
      .flatMap(id => typedModifierById[Header](id))
      .flatMap(_.sectionIds.map(_._2))
    historyStorage.remove(toRemove)
  }

  private def updateStorage(newModRow: ErgoPersistentModifier,
                            bestFullHeaderId: ModifierId): Unit = {
    val indicesToInsert = Seq(BestFullBlockKey -> Algos.idToBAW(bestFullHeaderId))
    historyStorage.insert(storageVersion(newModRow), indicesToInsert, Seq(newModRow))
      .ensuring(bestHeaderHeight >= bestFullBlockHeight, s"Headers height $bestHeaderHeight should be >= " +
        s"full height $bestFullBlockHeight")
  }

  private def storageVersion(newModRow: ErgoPersistentModifier) = Algos.idToBAW(newModRow.id)

}

private object FullBlockProcessor {

  type BlockProcessing = PartialFunction[ToProcess, ProgressInfo[ErgoPersistentModifier]]

  case class ToProcess(fullBlock: ErgoFullBlock,
                       newModRow: ErgoPersistentModifier,
                       newBestBlockId: ModifierId,
                       blocksToKeep: Int,
                       bestFullChain: Seq[ModifierId])

  case class MonitorBlock(id: ModifierId, height: Int)

  /**
    * Stores links mapping ((id, height) -> parentId) of blocks that could possibly be applied.
    */
  case class IncompleteFullChainMonitor(monitor: TreeMap[MonitorBlock, ModifierId]) {

    val nonEmpty: Boolean = monitor.nonEmpty

    def get(id: ModifierId, height: Int): Option[ModifierId] = monitor.get(MonitorBlock(id, height))

    def add(id: ModifierId, parentId: ModifierId, height: Int): IncompleteFullChainMonitor =
      IncompleteFullChainMonitor(monitor.insert(MonitorBlock(id, height), parentId))

    def dropUntil(height: Int): IncompleteFullChainMonitor =
      IncompleteFullChainMonitor(monitor.dropWhile(_._1.height < height))
  }

  private implicit val ord: Ordering[MonitorBlock] = Ordering[(Int, ModifierId)].on(x => (x.height, x.id))

  def emptyMonitor: IncompleteFullChainMonitor = IncompleteFullChainMonitor(TreeMap.empty)

}
