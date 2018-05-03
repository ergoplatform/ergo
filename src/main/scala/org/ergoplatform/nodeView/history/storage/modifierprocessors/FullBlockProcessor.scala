package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor.{BlockProcessing, ToProcess}
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
    processing(ToProcess(fullBlock, newModRow, newBestAfterThis, config.blocksToKeep))
  }

  private def processing: BlockProcessing =
    processValidFirstBlock orElse
      processBetterChain orElse
      nonBestBlock

  protected def isValidFirstFullBlock(header: Header): Boolean = {
    header.height == pruningProcessor.minimalFullBlockHeight && bestFullBlockIdOpt.isEmpty
  }

  private def processValidFirstBlock: BlockProcessing = {
    case ToProcess(fullBlock, newModRow, newBestAfterThis, blocksToKeep)
       if isValidFirstFullBlock(fullBlock.header) =>

      logStatus(Seq(), Seq(fullBlock), fullBlock, None)
      updateStorage(newModRow, newBestAfterThis.id)
      ProgressInfo(None, Seq.empty, Seq(fullBlock), Seq.empty)
  }

  private def processBetterChain: BlockProcessing = {
    case toProcess @ ToProcess(fullBlock, newModRow, newBestAfterThis, blocksToKeep)
        if bestFullBlockOpt.nonEmpty && isBetterChain(newBestAfterThis.id) =>

      val prevBest = bestFullBlockOpt.get
      val (prevChain, newChain) = commonBlockThenSuffixes(prevBest.header, newBestAfterThis)
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

        updateStorage(newModRow, newBestAfterThis.id)

        if (blocksToKeep >= 0) {
          val lastKept = pruningProcessor.updateBestFullBlock(fullBlock.header)
          val bestHeight: Int = newBestAfterThis.height
          val diff = bestHeight - prevBest.header.height
          pruneBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
        }
        ProgressInfo(branchPoint, toRemove, toApply, Seq.empty)
      }
  }

  private def isBetterChain(id: ModifierId): Boolean = {
    val isBetter = for {
      bestFullBlockId <- bestFullBlockIdOpt
      prevBestScore <- scoreOf(bestFullBlockId)
      score <- scoreOf(id)
      //TODO currentScore == prevBestScore
    } yield score > prevBestScore

    isBetter getOrElse false
  }

  private def nonBestBlock: BlockProcessing = { case params =>
    //Orphaned block or full chain is not initialized yet
    logStatus(Seq(), Seq(), params.fullBlock, None)
    historyStorage.insert(storageVersion(params.newModRow), Seq.empty, Seq(params.newModRow))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
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
  private def pruneOnNewBestBlock(header: Header, blocksToKeep: Int): Unit = {
    heightOf(header.id).filter(h => h > blocksToKeep)
      .foreach(h => pruneBlockDataAt(Seq(h - blocksToKeep)))
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
          Failure(new Error(s"Header for modifier $m is not defined"))
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

object FullBlockProcessor {

  type BlockProcessing = PartialFunction[ToProcess, ProgressInfo[ErgoPersistentModifier]]

  case class ToProcess(
    fullBlock: ErgoFullBlock,
    newModRow: ErgoPersistentModifier,
    newBestAfterThis: Header,
    blocksToKeep: Int
  )
}
