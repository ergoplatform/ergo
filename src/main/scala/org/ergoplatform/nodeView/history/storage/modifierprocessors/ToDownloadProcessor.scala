package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history._
import org.ergoplatform.settings.{ChainSettings, NodeConfigurationSettings}
import scorex.core.ModifierTypeId
import scorex.core.utils.NetworkTimeProvider
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec

/**
  * Trait that calculates next modifiers we should download to synchronize our full chain with headers chain
  */
trait ToDownloadProcessor extends BasicReaders with ScorexLogging {

  protected val config: NodeConfigurationSettings

  protected val chainSettings: ChainSettings

  protected val timeProvider: NetworkTimeProvider

  protected[history] lazy val pruningProcessor: FullBlockPruningProcessor =
    new FullBlockPruningProcessor(config, chainSettings)

  protected def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain

  /** Returns true if we estimate that our chain is synced with the network. Start downloading full blocks after that
    */
  def isHeadersChainSynced: Boolean = pruningProcessor.isHeadersChainSynced

  /** Returns Next `howMany` modifier ids satisfying `filter` condition our node should download
    * to synchronize full block chain with headers chain
    */
  def nextModifiersToDownload(howMany: Int, condition: ModifierId => Boolean): Seq[(ModifierTypeId, ModifierId)] = {
    @tailrec
    def continuation(height: Int, acc: Seq[(ModifierTypeId, ModifierId)]): Seq[(ModifierTypeId, ModifierId)] = {
      if (acc.lengthCompare(howMany) >= 0) {
        acc.take(howMany)
      } else {
        headerIdsAtHeight(height).headOption.flatMap(id => typedModifierById[Header](id)) match {
          case Some(bestHeaderAtThisHeight) =>
            val toDownload = requiredModifiersForHeader(bestHeaderAtThisHeight)
              .filter(m => condition(m._2))
            continuation(height + 1, acc ++ toDownload)
          case None => acc
        }
      }
    }

    bestFullBlockOpt match {
      case _ if !isHeadersChainSynced || !config.verifyTransactions =>
        Seq.empty
      case Some(fb) =>
        continuation(fb.header.height + 1, Seq.empty)
      case None =>
        continuation(pruningProcessor.minimalFullBlockHeight, Seq.empty)
    }
  }

  /**
    * Checks whether it's time to download full chain, and returns toDownload modifiers
    */
  protected def toDownload(header: Header): Seq[(ModifierTypeId, ModifierId)] = {
    if (!config.verifyTransactions) {
      // A regime that do not download and verify transaction
      Seq.empty
    } else if (pruningProcessor.shouldDownloadBlockAtHeight(header.height)) {
      // Already synced and header is not too far back. Download required modifiers.
      requiredModifiersForHeader(header)
    } else if (!isHeadersChainSynced && header.isNew(timeProvider, chainSettings.blockInterval * 5)) {
      // Headers chain is synced after this header. Start downloading full blocks
      pruningProcessor.updateBestFullBlock(header)
      log.info(s"Headers chain is likely synced after header ${header.encodedId} at height ${header.height}")
      Seq.empty
    } else {
      Seq.empty
    }
  }

  private def requiredModifiersForHeader(h: Header): Seq[(ModifierTypeId, ModifierId)] = {
    if (!config.verifyTransactions) {
      Seq.empty
    } else if (config.stateType.requireProofs) {
      h.sectionIds
    } else {
      h.sectionIds.tail
    }
  }
}
