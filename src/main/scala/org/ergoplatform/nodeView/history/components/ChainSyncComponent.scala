package org.ergoplatform.nodeView.history.components

import org.ergoplatform.modifiers.history._
import org.ergoplatform.settings.{ChainSettings, ErgoSettings, NodeConfigurationSettings}
import scorex.core.ModifierTypeId
import scorex.core.utils.NetworkTimeProvider
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec

/**
  * Trait that calculates next modifiers we should download to synchronize our full chain with headers chain
  */
trait ChainSyncComponent extends ScorexLogging {
  self: BasicReaders with Configuration =>

  private val maxTimeDiffFactor = settings.chainSettings.maxTimeDiffFactor

  protected[history] lazy val pruningProcessor: ChainSyncController =
    new ChainSyncController(nodeSettings, chainSettings)

  protected def nodeSettings: NodeConfigurationSettings = settings.nodeSettings

  protected def chainSettings: ChainSettings = settings.chainSettings

  protected def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain

  def isInBestChain(id: ModifierId): Boolean

  /**
    * Returns true if we estimate that our chain is synced with the network. Start downloading full blocks after that
    */
  def isHeadersChainSynced: Boolean = pruningProcessor.isHeadersChainSynced

  /**
    * Returns Next `howMany` modifier ids satisfying `filter` condition our node should download
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
      case _ if !isHeadersChainSynced || !nodeSettings.verifyTransactions =>
        Seq.empty
      case Some(fb) if isInBestChain(fb.id) =>
        continuation(fb.header.height + 1, Seq.empty)
      case _ =>
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
    } else if (!isHeadersChainSynced && header.isNew(timeProvider, chainSettings.blockInterval * maxTimeDiffFactor)) {
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
