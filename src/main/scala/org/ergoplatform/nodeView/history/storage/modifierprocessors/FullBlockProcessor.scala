package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier, ModifierWithDigest}
import org.ergoplatform.settings.Algos
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

  /**
    * Process full block when we have one.
    *
    * @param fullBlock - block to process
    * @param txsAreNew - flag, that transactions where added last
    * @return ProgressInfo required for State to process to be consistent with History
    */
  @SuppressWarnings(Array("OptionGet"))
  protected def processFullBlock(fullBlock: ErgoFullBlock,
                                 txsAreNew: Boolean): ProgressInfo[ErgoPersistentModifier] = {
    val header: Header = fullBlock.header
    val txs: BlockTransactions = fullBlock.blockTransactions
    val adProofsOpt: Option[ADProofs] = fullBlock.aDProofs
      .ensuring(_.isDefined || txsAreNew, "Only transactions can be new when proofs are empty")
    val newModRow = if (txsAreNew) txs else adProofsOpt.get
    val storageVersion = ByteArrayWrapper(if (txsAreNew) txs.id else adProofsOpt.get.id)
    val continuations = continuationHeaderChains(header, _ => true).map(_.tail)
    val bestFullChain = continuations.map(hc => hc.map(getFullBlock).takeWhile(_.isDefined).flatten.map(_.header))
      .map(c => header +: c)
      .maxBy(c => scoreOf(c.last.id))

    val newBestAfterThis = bestFullChain.last

    (bestFullBlockOpt, bestFullBlockIdOpt.flatMap(scoreOf), scoreOf(newBestAfterThis.id)) match {
      case (None, _, _) if header.isGenesis =>
        //TODO find a correct to start form non-genesis block. https://github.com/ergoplatform/ergo/issues/146
        logStatus(Seq(), Seq(fullBlock), fullBlock, None)
        updateStorage(newModRow, storageVersion, fullBlock, newBestAfterThis.id)

      case (Some(prevBest), Some(prevBestScore), Some(score)) if score > prevBestScore =>
        //TODO currentScore == prevBestScore
        val (prevChain, newChain) = commonBlockThenSuffixes(prevBest.header, header)
        val toRemove: Seq[ErgoFullBlock] = prevChain.tail.headers.flatMap(getFullBlock)
        val toApply: Seq[ErgoFullBlock] = newChain.tail.headers
          .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getFullBlock(h))

        if (toApply.lengthCompare(newChain.length - 1) != 0) {
          //block have higher score but is not linkable to full chain
          nonBestBlock(fullBlock, newModRow, storageVersion)
        } else {
          //application of this block leads to full chain with higher score
          logStatus(toRemove, toApply, fullBlock, Some(prevBest))
          val branchPoint = toRemove.headOption.map(_ => prevChain.head.id)

          updateStorage(newModRow, storageVersion, fullBlock, newBestAfterThis.id)

          if (config.blocksToKeep >= 0) {
            val bestHeight: Int = newBestAfterThis.height
            val diff = newBestAfterThis.height - prevBest.header.height
            val lastKept = bestHeight - config.blocksToKeep
            pruneBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
          }
          ProgressInfo(branchPoint, toRemove, toApply.headOption, Seq.empty)
        }

      case (None, _, _) =>
        //Full chain is not initialized yet
        nonBestBlock(fullBlock, newModRow, storageVersion)

      case _ =>
        //Orphaned block
        nonBestBlock(fullBlock, newModRow, storageVersion)
    }
  }

  private def nonBestBlock(fullBlock: ErgoFullBlock,
                           newModRow: ErgoPersistentModifier,
                           storageVersion: ByteArrayWrapper): ProgressInfo[ErgoPersistentModifier] = {
    logStatus(Seq(), Seq(), fullBlock, None)
    historyStorage.insert(storageVersion, Seq.empty, Seq(newModRow))
    ProgressInfo(None, Seq.empty, None, Seq.empty)
  }

  private def logStatus(toRemove: Seq[ErgoFullBlock],
                        toApply: Seq[ErgoFullBlock],
                        appliedBlock: ErgoFullBlock,
                        prevBest: Option[ErgoFullBlock]): Unit = {
    val toRemoveStr = if (toRemove.nonEmpty) s" and to remove ${toRemove.length}" else ""
    val newStatusStr = if (toApply.nonEmpty) {
      s" New best block is ${toApply.last.header.encodedId} with height ${toApply.last.header.height} updates block " +
        s"${prevBest.map(_.encodedId).getOrElse("None")} with height ${prevBest.map(_.header.height).getOrElse(-1)}"
    } else ""
    log.info(s"Full block ${appliedBlock.encodedId} appended, going to apply ${toApply.length}$toRemoveStr modifiers." +
      newStatusStr)
  }

  private def pruneOnNewBestBlock(header: Header): Unit = heightOf(header.id).filter(h => h > config.blocksToKeep)
    .foreach(h => pruneBlockDataAt(Seq(h - config.blocksToKeep)))

  private def pruneBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights.flatMap(h => headerIdsAtHeight(h))
      .flatMap { id => typedModifierById[Header](id) }
      .flatMap { h =>
        Seq(h.ADProofsId, h.transactionsId)
      }
    historyStorage.remove(toRemove)
  }

  private def updateStorage(newModRow: ErgoPersistentModifier,
                            storageVersion: ByteArrayWrapper,
                            toApply: ErgoFullBlock,
                            bestFullHeaderId: ModifierId): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.insert(storageVersion, Seq((BestFullBlockKey, ByteArrayWrapper(bestFullHeaderId))), Seq(newModRow))
      .ensuring(headersHeight >= fullBlockHeight, s"Headers height $headersHeight should be >= full height $fullBlockHeight")
    ProgressInfo(None, Seq.empty, Some(toApply), Seq.empty)
  }

}
