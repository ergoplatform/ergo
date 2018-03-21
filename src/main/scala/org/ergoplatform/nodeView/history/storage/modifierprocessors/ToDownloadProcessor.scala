package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, HeaderChain}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{ChainSettings, NodeConfigurationSettings}
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.core.{ModifierId, ModifierTypeId}

import scala.annotation.tailrec

trait ToDownloadProcessor extends ScorexLogging {

  protected val config: NodeConfigurationSettings

  protected val chainSettings: ChainSettings

  protected val timeProvider: NetworkTimeProvider

  private lazy val downloadProofs = config.stateType.isInstanceOf[StateType.DigestType]

  /**
    * Start height to download full blocks.
    * Int.MaxValue when headers chain is not synchronized with the network and no full blocks download needed
    */
  protected var minimalFullBlockHeight: Int = Int.MaxValue

  def bestFullBlockOpt: Option[ErgoFullBlock]

  def headerIdsAtHeight(height: Int): Seq[ModifierId]

  def typedModifierById[T <: ErgoPersistentModifier](id: ModifierId): Option[T]

  def contains(id: ModifierId): Boolean

  /**
    * @return true if we estimate, that our chain is synced with the network. Start downloading full blocks after that
    */
  def isHeadersChainSynced: Boolean = minimalFullBlockHeight != Int.MaxValue

  def nextModifiersToDownload(howMany: Int, excluding: Iterable[ModifierId]): Seq[(ModifierTypeId, ModifierId)] = {
    @tailrec
    def continuation(height: Int, acc: Seq[(ModifierTypeId, ModifierId)]): Seq[(ModifierTypeId, ModifierId)] = {
      if (acc.lengthCompare(howMany) >= 0) {
        acc
      } else {
        headerIdsAtHeight(height).headOption.flatMap(id => typedModifierById[Header](id)) match {
          case Some(bestHeaderAtThisHeight) =>
            val toDownload = requiredModifiersForHeader(bestHeaderAtThisHeight)
              .filter(m => !excluding.exists(ex => ex sameElements m._2))
              .filter(m => !contains(m._2))
            continuation(height + 1, acc ++ toDownload)
          case None => acc
        }
      }
    }

    bestFullBlockOpt match {
      case _ if !isHeadersChainSynced =>
        Seq.empty
      case Some(fb) =>
        continuation(fb.header.height + 1, Seq.empty)
      case None =>
        continuation(minimalFullBlockHeight, Seq.empty)
    }
  }

  protected def onNewBestHeader(header: Header): Unit = {
    if (config.blocksToKeep >= 0 && isHeadersChainSynced) minimalFullBlockHeight = header.height - config.blocksToKeep
  }

  /**
    * Checks, whether it's time to download full chain and return toDownload modifiers
    * TODO Required to return to nodeViewHolder, but never used
    */
  protected def toDownload(header: Header): Seq[(ModifierTypeId, ModifierId)] = {

    if (!config.verifyTransactions) {
      // Regime that do not download and verify transaction
      Seq.empty
    } else if (header.height >= minimalFullBlockHeight) {
      // Already synced and header is not too far back. Download required modifiers
      requiredModifiersForHeader(header)
    } else if (!isHeadersChainSynced && isNewHeader(header)) {
      // Headers chain is synced after this header. Start downloading full blocks
      log.info(s"Headers chain is synced after header $header")
      if (config.blocksToKeep >= 0) {
        minimalFullBlockHeight = header.height - config.blocksToKeep
      } else {
        minimalFullBlockHeight = 0
      }
      Seq.empty
    } else {
      Seq.empty
    }
  }

  private def requiredModifiersForHeader(h: Header): Seq[(ModifierTypeId, ModifierId)] = {
    if (!config.verifyTransactions) {
      Seq.empty
    } else if (downloadProofs) {
      Seq((BlockTransactions.modifierTypeId, h.transactionsId), (ADProofs.modifierTypeId, h.ADProofsId))
    } else {
      Seq((BlockTransactions.modifierTypeId, h.transactionsId))
    }
  }


  protected def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain

  /**
    * Estimate, that this block is new enough.
    * TODO use the same function to start mining
    */
  private def isNewHeader(h: Header): Boolean = {
    timeProvider.time() - h.timestamp < chainSettings.blockInterval.toMillis * 5
  }

}
