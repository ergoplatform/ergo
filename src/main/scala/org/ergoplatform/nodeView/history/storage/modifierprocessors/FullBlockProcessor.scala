package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
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
        log.info(s"Initialize full block chain with genesis header ${newBestAfterThis.encodedId} with transactions and proofs")
        updateStorage(newModRow, storageVersion, fullBlock, newBestAfterThis.id)
      /*
            //TODO find a correct way here. State should know that we'll start from this header. https://github.com/ergoplatform/ergo/issues/146
            case (None, _, _) if config.blocksToKeep >= 0 =>

              log.info(s"Initialize full block chain with new best header ${header.encodedId} with transactions and proofs")
              updateStorage(newModRow, storageVersion, fullBlock, fullBlock.header.id)
      */
      case (Some(prevBest), _, Some(score)) if header.parentId sameElements prevBest.header.id =>
        log.info(s"New best full block with header ${newBestAfterThis.encodedId}. " +
          s"Height = ${newBestAfterThis.height}, score = $score")
        if (config.blocksToKeep >= 0) pruneOnNewBestBlock(header)
        updateStorage(newModRow, storageVersion, fullBlock, newBestAfterThis.id)

      case (Some(prevBest), Some(prevBestScore), Some(score)) if score > prevBestScore =>
        //TODO currentScore == prevBestScore
        val (prevChain, newChain) = commonBlockThenSuffixes(prevBest.header, header)
        val toRemove: Seq[ErgoFullBlock] = prevChain.tail.headers.flatMap(getFullBlock)
        val toApply: Seq[ErgoFullBlock] = newChain.tail.headers
          .flatMap(h => if(h == fullBlock.header) Some(fullBlock) else getFullBlock(h))

        if (toApply.lengthCompare(newChain.length - 1) == 0) {
          log.info(s"Process fork for new best full block with header ${newBestAfterThis.encodedId}. " +
            s"Height = ${newBestAfterThis.height}, score = $score")
          updateStorage(newModRow, storageVersion, fullBlock, newBestAfterThis.id)

          if (config.blocksToKeep >= 0) {
            val bestHeight: Int = newBestAfterThis.height
            val diff = newBestAfterThis.height - prevBest.header.height
            val lastKept = bestHeight - config.blocksToKeep
            pruneBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
          }
          ProgressInfo(Some(prevChain.head.id), toRemove, toApply.headOption, Seq.empty)
        } else {
          log.info(s"Got transactions and proofs for header ${header.encodedId} with no connection to genesis")
          historyStorage.insert(storageVersion, Seq.empty, Seq(newModRow))
          ProgressInfo(None, Seq.empty, None, Seq.empty)
        }
      case _ =>
        log.info(s"Got transactions and proofs for non-best header ${header.encodedId}")
        historyStorage.insert(storageVersion, Seq.empty, Seq(newModRow))
        ProgressInfo(None, Seq.empty, None, Seq.empty)
    }
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
    ProgressInfo(None, Seq.empty, Some(toApply), Seq.empty)
  }

}
