package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import scorex.core.ModifierId
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

import scala.util.Try

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
    * @param fullBlock - block to process
    * @param txsAreNew - flag, that transactions where added last
    * @return ProgressInfo required for State to process to be consistent with History
    */
  protected def processFullBlock(fullBlock: ErgoFullBlock,
                                 txsAreNew: Boolean): ProgressInfo[ErgoPersistentModifier] =
    new FullBlockProcessing(fullBlock, txsAreNew).process()

  /** Implementation of the full block processing
    */
  class FullBlockProcessing(fullBlock: ErgoFullBlock, txsAreNew: Boolean) {

    private val header: Header = fullBlock.header
    private val newModRow = calculateNewModRow
    private val storageVersion = ByteArrayWrapper(newModRow.id)
    private val bestFullChain = calculateBestFullChain
    private val newBestAfterThis = bestFullChain.last
    private val newBestFullBlockId = newBestAfterThis.id

    def process(): ProgressInfo[ErgoPersistentModifier] =
      processIfGenesisBlock().
        orElse(processIfBetterChain()).
        getOrElse(nonBestBlock())

    private def processIfGenesisBlock(): Option[ProgressInfo[ErgoPersistentModifier]] = {
      if (bestFullBlockOpt.isEmpty && header.isGenesis) {
        Option(applyGenesisBlock())
      } else {
        None
      }
    }

    private def processIfBetterChain(): Option[ProgressInfo[ErgoPersistentModifier]] = {
      for {
        bestFullBlock <- bestFullBlockOpt
        bestFullBlockId <- bestFullBlockIdOpt
        prevBestScore <- scoreOf(bestFullBlockId)
        score <- scoreOf(newBestFullBlockId)
        if score > prevBestScore
        //TODO currentScore == prevBestScore
      } yield applyBetterChain(bestFullBlock)
    }

    private def applyGenesisBlock(): ProgressInfo[ErgoPersistentModifier] = {
      //TODO find a correct to start form non-genesis block. https://github.com/ergoplatform/ergo/issues/146
      logStatus(Seq(), Seq(fullBlock), None)
      updateStorage()
      ProgressInfo(None, Seq.empty, Some(fullBlock), Seq.empty)
    }

    private def applyBetterChain(prevBest: ErgoFullBlock): ProgressInfo[ErgoPersistentModifier] = {
      val (prevChain, newChain) = commonBlockThenSuffixes(prevBest.header, header)
      val toRemove: Seq[ErgoFullBlock] = prevChain.tail.headers.flatMap(getFullBlock)
      val toApply: Seq[ErgoFullBlock] = newChain.tail.headers
        .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getFullBlock(h))

      if (toApply.lengthCompare(newChain.length - 1) != 0) {
        //block have higher score but is not linkable to full chain
        nonBestBlock()
      } else {
        //application of this block leads to full chain with higher score
        logStatus(toRemove, toApply, Some(prevBest))
        val branchPoint = toRemove.headOption.map(_ => prevChain.head.id)

        updateStorage()

        if (config.blocksToKeep >= 0) {
          val bestHeight: Int = newBestAfterThis.height
          val diff = bestHeight - prevBest.header.height
          val lastKept = bestHeight - config.blocksToKeep
          pruneBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
        }
        ProgressInfo(branchPoint, toRemove, toApply.headOption, Seq.empty)
      }
    }

    private def nonBestBlock(): ProgressInfo[ErgoPersistentModifier] = {
      //Orphaned block or full chain is not initialized yet
      logStatus(Seq(), Seq(), None)
      historyStorage.insert(storageVersion, Seq.empty, Seq(newModRow))
      ProgressInfo(None, Seq.empty, None, Seq.empty)
    }


    private def calculateNewModRow: ErgoPersistentModifier = {
      if (txsAreNew) {
        fullBlock.blockTransactions
      } else {
        fullBlock.aDProofs
          .getOrElse(throw new NoSuchElementException("Only transactions can be new when proofs are empty"))
      }
    }

    private def calculateBestFullChain() = {
      val continuations = continuationHeaderChains(header, _ => true).map(_.tail)
      val headers = continuations.map(hc => hc.map(getFullBlock).takeWhile(_.isDefined).flatten.map(_.header))
      headers.map(c => header +: c).maxBy(c => scoreOf(c.last.id))
    }

    private def logStatus(toRemove: Seq[ErgoFullBlock],
                          toApply: Seq[ErgoFullBlock],
                          prevBest: Option[ErgoFullBlock]): Unit = {
      val toRemoveStr = if (toRemove.isEmpty) "" else s" and to remove ${toRemove.length}"
      val newStatusStr = if (toApply.isEmpty) "" else {
        s" New best block is ${toApply.last.header.encodedId} " +
          s"with height ${toApply.last.header.height} " +
          s"updates block ${prevBest.map(_.encodedId).getOrElse("None")} " +
          s"with height ${prevBest.map(_.header.height).getOrElse(-1)}"
      }
      log.info(s"Full block ${fullBlock.encodedId} appended, " +
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

    private def updateStorage(): Unit = {
      val indices = Seq((BestFullBlockKey, ByteArrayWrapper(newBestFullBlockId)))
      historyStorage.insert(storageVersion, indices, Seq(newModRow))
        .ensuring(headersHeight >= fullBlockHeight, s"Headers height $headersHeight should be >= " +
                                                    s"full height $fullBlockHeight")
    }

  }

}
