package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history._
import org.ergoplatform.settings.{ChainSettings, ErgoSettings, NodeConfigurationSettings}
import scorex.core.ModifierTypeId
import scorex.core.utils.NetworkTimeProvider
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec

/**
  * Trait that calculates next modifiers we should download to synchronize our full chain with headers chain
  */
trait ToDownloadProcessor extends BasicReaders with ScorexLogging {

  protected val timeProvider: NetworkTimeProvider

  protected val settings: ErgoSettings

  // A node is considering that the chain is synced if sees a block header with timestamp no more
  // than headerChainDiff blocks on average from future
  private lazy val headerChainDiff = settings.nodeSettings.headerChainDiff

  protected[history] lazy val pruningProcessor: FullBlockPruningProcessor =
    new FullBlockPruningProcessor(nodeSettings, chainSettings)

  protected def nodeSettings: NodeConfigurationSettings = settings.nodeSettings

  protected def chainSettings: ChainSettings = settings.chainSettings

  protected def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain

  def isInBestChain(id: ModifierId): Boolean

  /** Returns true if we estimate that our chain is synced with the network. Start downloading full blocks after that
    */
  def isHeadersChainSynced: Boolean = pruningProcessor.isHeadersChainSynced

  /** Returns Next `howMany` modifier ids satisfying `filter` condition our node should download
    * to synchronize full blocks
    */
  def nextModifiersToDownload(howMany: Int, condition: ModifierId => Boolean): Seq[(ModifierTypeId, ModifierId)] = {
    @tailrec
    def continuation(height: Int, acc: Seq[(ModifierTypeId, ModifierId)]): Seq[(ModifierTypeId, ModifierId)] = {
      if (acc.lengthCompare(howMany) >= 0) {
        acc.take(howMany)
      } else {
        val headersAtThisHeight = headerIdsAtHeight(height).flatMap(id => typedModifierById[Header](id))

        if (headersAtThisHeight.nonEmpty) {
          val toDownload = headersAtThisHeight.flatMap(h => requiredModifiersForHeader(h))
                       .filter(m => condition(m._2))
                    continuation(height + 1, acc ++ toDownload)
        } else {
          acc
        }
      }
    }

    bestFullBlockOpt match {
      case _ if !isHeadersChainSynced || !nodeSettings.verifyTransactions =>
        // do not download full blocks if no headers-chain synced yet or SPV mode
        Seq.empty
      case Some(fb) =>
        // download children blocks of last 100 full blocks applied to the best chain
        val minHeight = Math.max(1, fb.header.height - 100)
        continuation(minHeight, Seq.empty)
      case _ =>
        // if headers-chain is synced and no full blocks applied yet, find full block height to go from
        continuation(pruningProcessor.minimalFullBlockHeight, Seq.empty)
    }
  }

  /**
    * Checks whether it's time to download full chain, and returns toDownload modifiers
    */
  protected def toDownload(header: Header): Seq[(ModifierTypeId, ModifierId)] = {
    if (!nodeSettings.verifyTransactions) {
      // A regime that do not download and verify transaction
      Seq.empty
    } else if (pruningProcessor.shouldDownloadBlockAtHeight(header.height)) {
      // Already synced and header is not too far back. Download required modifiers.
      requiredModifiersForHeader(header)
    } else if (!isHeadersChainSynced && header.isNew(timeProvider, chainSettings.blockInterval * headerChainDiff)) {
      // Headers chain is synced after this header. Start downloading full blocks
      pruningProcessor.updateBestFullBlock(header)
      log.info(s"Headers chain is likely synced after header ${header.encodedId} at height ${header.height}")
      Seq.empty
    } else {
      Seq.empty
    }
  }

  private def requiredModifiersForHeader(h: Header): Seq[(ModifierTypeId, ModifierId)] = {
    if (!nodeSettings.verifyTransactions) {
      Seq.empty
    } else if (nodeSettings.stateType.requireProofs) {
      h.sectionIds
    } else {
      h.sectionIds.tail
    }
  }

}
