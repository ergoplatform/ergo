package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import scorex.core.ModifierId
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

/**
  * Contains functions required by History to process Transactions and Proofs when we have them.
  * Prune modifiers older then blocksToKeep.
  */
trait FullBlockProcessor extends HeadersProcessor with ScorexLogging {

  /**
    * Id of header that contains transactions and proofs
    */
  override def bestFullBlockIdOpt: Option[ModifierId] = historyStorage.getIndex(BestFullBlockKey).map(ModifierId @@ _.data)

  protected def getFullBlock(h: Header): Option[ErgoFullBlock]

  protected def fullBlockExists(header: Header): Boolean = getFullBlock(header).nonEmpty

  protected def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain)

  protected[history] def continuationHeaderChains(header: Header, withFilter: Header => Boolean): Seq[Seq[Header]]

  /** Process full block when we have one.
    *
    * @param fullBlock - block to process
    * @param txsAreNew - flag, that transactions where added last
    * @return ProgressInfo required for State to process to be consistent with History
    */
  protected def processFullBlock(fullBlock: ErgoFullBlock, txsAreNew: Boolean): ProgressInfo[ErgoPersistentModifier] = {
    val newModRow = calculateNewModRow(fullBlock, txsAreNew)
    val bestFullChain = calculateBestFullChain(fullBlock.header)
    val newBestAfterThis = bestFullChain.last
    processIfValidFirstBlock(fullBlock, newModRow, newBestAfterThis).
      orElse(processIfBetterChain(fullBlock, newModRow, newBestAfterThis)).
      getOrElse(nonBestBlock(fullBlock, newModRow))
  }

  protected def isValidFirstFullBlock(header: Header): Boolean = {
    header.height == pruningProcessor.minimalFullBlockHeight && bestFullBlockIdOpt.isEmpty
  }

  private def processIfValidFirstBlock(fullBlock: ErgoFullBlock,
                                       newModRow: ErgoPersistentModifier,
                                       newBestAfterThis: Header): Option[ProgressInfo[ErgoPersistentModifier]] = {
    if (isValidFirstFullBlock(fullBlock.header)) {
      Some(applyFirstFullBlock(fullBlock, newModRow, newBestAfterThis))
    } else {
      None
    }
  }

  private def applyFirstFullBlock(fullBlock: ErgoFullBlock,
                                  newModRow: ErgoPersistentModifier,
                                  newBestAfterThis: Header): ProgressInfo[ErgoPersistentModifier] = {
    logStatus(Seq(), Seq(fullBlock), fullBlock, None)
    updateStorage(newModRow, newBestAfterThis.id)
    ProgressInfo(None, Seq.empty, Some(fullBlock), Seq.empty)
  }

  private def processIfBetterChain(fullBlock: ErgoFullBlock,
                                   newModRow: ErgoPersistentModifier,
                                   newBestAfterThis: Header): Option[ProgressInfo[ErgoPersistentModifier]] = {
    for {
      prevBestFullBlock <- bestFullBlockOpt
      bestFullBlockId <- bestFullBlockIdOpt
      prevBestScore <- scoreOf(bestFullBlockId)
      score <- scoreOf(newBestAfterThis.id)
      if score > prevBestScore
      //TODO currentScore == prevBestScore
    } yield applyBetterChain(fullBlock, newModRow, prevBestFullBlock, newBestAfterThis)
  }


  private def applyBetterChain(fullBlock: ErgoFullBlock,
                               newModRow: ErgoPersistentModifier,
                               prevBest: ErgoFullBlock,
                               newBestAfterThis: Header): ProgressInfo[ErgoPersistentModifier] = {
    val (prevChain, newChain) = commonBlockThenSuffixes(prevBest.header, fullBlock.header)
    val toRemove: Seq[ErgoFullBlock] = prevChain.tail.headers.flatMap(getFullBlock)
    val toApply: Seq[ErgoFullBlock] = newChain.tail.headers
      .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getFullBlock(h))

    if (toApply.lengthCompare(newChain.length - 1) != 0) {
      //block have higher score but is not linkable to full chain
      nonBestBlock(fullBlock, newModRow)
    } else {
      //application of this block leads to full chain with higher score
      logStatus(toRemove, toApply, fullBlock, Some(prevBest))
      val branchPoint = toRemove.headOption.map(_ => prevChain.head.id)

      updateStorage(newModRow, newBestAfterThis.id)

      if (config.blocksToKeep >= 0) {
        val lastKept = pruningProcessor.updateBestFullBlock(fullBlock.header)
        val bestHeight: Int = newBestAfterThis.height
        val diff = bestHeight - prevBest.header.height
        pruneBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
      }
      ProgressInfo(branchPoint, toRemove, toApply.headOption, Seq.empty)
    }
  }

  private def nonBestBlock(fullBlock: ErgoFullBlock,
                           newModRow: ErgoPersistentModifier): ProgressInfo[ErgoPersistentModifier] = {
    //Orphaned block or full chain is not initialized yet
    logStatus(Seq(), Seq(), fullBlock, None)
    historyStorage.insert(storageVersion(newModRow), Seq.empty, Seq(newModRow))
    ProgressInfo(None, Seq.empty, None, Seq.empty)
  }

  private def calculateNewModRow(fullBlock: ErgoFullBlock, txsAreNew: Boolean): ErgoPersistentModifier = {
    if (txsAreNew) {
      fullBlock.blockTransactions
    } else {
      fullBlock.aDProofs
        .getOrElse(throw new NoSuchElementException("Only transactions can be new when proofs are empty"))
    }
  }

  private def calculateBestFullChain(header: Header) = {
    val continuations = continuationHeaderChains(header, h => getFullBlock(h).nonEmpty).map(_.tail)
    val chains = continuations.map(hc => hc.map(getFullBlock).takeWhile(_.isDefined).flatten.map(_.header))
    chains.map(c => header +: c).maxBy(c => scoreOf(c.last.id))
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
        s"with height ${prevBest.map(_.header.height).getOrElse(-1)}"
    }
    log.info(s"Full block ${appliedBlock.encodedId} appended, " +
      s"going to apply ${toApply.length}$toRemoveStr modifiers.$newStatusStr")
  }

  //Not used so far
  private def pruneOnNewBestBlock(header: Header): Unit = {
    heightOf(header.id).filter(h => h > config.blocksToKeep)
      .foreach(h => pruneBlockDataAt(Seq(h - config.blocksToKeep)))
  }

  private def pruneBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights.flatMap(h => headerIdsAtHeight(h))
      .flatMap { id => typedModifierById[Header](id) }
      .flatMap { h =>
        Seq(h.ADProofsId, h.transactionsId)
      }
    historyStorage.remove(toRemove)
  }

  private def updateStorage(newModRow: ErgoPersistentModifier,
                            bestFullHeaderId: ModifierId): Unit = {
    val indicesToInsert = Seq(BestFullBlockKey -> ByteArrayWrapper(bestFullHeaderId))
    historyStorage.insert(storageVersion(newModRow), indicesToInsert, Seq(newModRow))
      .ensuring(headersHeight >= fullBlockHeight, s"Headers height $headersHeight should be >= " +
        s"full height $fullBlockHeight")
  }

  private def storageVersion(newModRow: ErgoPersistentModifier) = ByteArrayWrapper(newModRow.id)

  protected def modifierValidation(m: ErgoPersistentModifier,
                                   headerOpt: Option[Header]): Try[Unit] = {
    if (historyStorage.contains(m.id)) {
      Failure(new Error(s"Modifier $m is already in history"))
    } else {
      val minimalHeight = pruningProcessor.minimalFullBlockHeight
      headerOpt match {
        case None =>
          Failure(new Error(s"Header for modifier $m is no defined"))
        case Some(header: Header) if header.height < minimalHeight =>
          Failure(new Error(s"Too old modifier ${m.encodedId}: ${header.height} < $minimalHeight"))
        case Some(header: Header) if !header.isCorrespondingModifier(m) =>
          Failure(new Error(s"Modifier ${m.encodedId} does not corresponds to header ${header.encodedId}"))
        case Some(_) =>
          Success()
      }
    }
  }

}
