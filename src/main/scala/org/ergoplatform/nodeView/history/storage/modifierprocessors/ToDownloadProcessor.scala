package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, HeaderChain}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.{ChainSettings, NodeConfigurationSettings}
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.core.{ModifierId, ModifierTypeId}

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
  * Trait that calculates next modifiers we should download to synchronize our full chain with headers chain
  */
trait ToDownloadProcessor extends ScorexLogging {

  protected[history] lazy val pruningProcessor: FullBlockPruningProcessor = new FullBlockPruningProcessor(config)

  protected val config: NodeConfigurationSettings

  protected val chainSettings: ChainSettings

  protected val timeProvider: NetworkTimeProvider

  private var isHeadersChainSyncedVar: Boolean = false

  def bestFullBlockOpt: Option[ErgoFullBlock]

  def headerIdsAtHeight(height: Int): Seq[ModifierId]

  def typedModifierById[T <: ErgoPersistentModifier : ClassTag](id: ModifierId): Option[T]

  def contains(id: ModifierId): Boolean

  protected def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain

  /**
    * @return true if we estimate, that our chain is synced with the network. Start downloading full blocks after that
    */
  def isHeadersChainSynced: Boolean = isHeadersChainSyncedVar

  /**
    *
    * @return Next `howMany` modifier ids satisfying `filter` condition our node should download
    *         to synchronize full block chain with headers chain
    */
  def nextModifiersToDownload(howMany: Int, filter: ModifierId => Boolean): Seq[(ModifierTypeId, ModifierId)] = {
    @tailrec
    def continuation(height: Int, acc: Seq[(ModifierTypeId, ModifierId)]): Seq[(ModifierTypeId, ModifierId)] = {
      if (acc.lengthCompare(howMany) >= 0) {
        acc
      } else {
        headerIdsAtHeight(height).headOption.flatMap(id => typedModifierById[Header](id)) match {
          case Some(bestHeaderAtThisHeight) =>
            val toDownload = requiredModifiersForHeader(bestHeaderAtThisHeight)
              .filter(m => filter(m._2))
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
        continuation(pruningProcessor.minimalFullBlockHeight, Seq.empty)
    }
  }

  /**
    * Checks, whether it's time to download full chain and return toDownload modifiers
    */
  protected def toDownload(header: Header): Seq[(ModifierTypeId, ModifierId)] = {

    if (!config.verifyTransactions) {
      // Regime that do not download and verify transaction
      Seq.empty
    } else if (header.height >= pruningProcessor.minimalFullBlockHeight) {
      // Already synced and header is not too far back. Download required modifiers
      requiredModifiersForHeader(header)
    } else if (!isHeadersChainSynced && isNewHeader(header)) {
      // Headers chain is synced after this header. Start downloading full blocks
      log.info(s"Headers chain is synced after header ${header.encodedId} at height ${header.height}")
      isHeadersChainSyncedVar = true
      pruningProcessor.updateBestFullBlock(header)
      Seq.empty
    } else {
      Seq.empty
    }
  }

  private def requiredModifiersForHeader(h: Header): Seq[(ModifierTypeId, ModifierId)] = {
    if (!config.verifyTransactions) {
      Seq.empty
    } else if (config.stateType.requireProofs) {
      Seq((BlockTransactions.modifierTypeId, h.transactionsId), (ADProofs.modifierTypeId, h.ADProofsId))
    } else {
      Seq((BlockTransactions.modifierTypeId, h.transactionsId))
    }
  }

  /**
    * Estimate, that this block is new enough.
    * TODO use the same function to start mining
    */
  private def isNewHeader(h: Header): Boolean = {
    timeProvider.time() - h.timestamp < chainSettings.blockInterval.toMillis * 5
  }

}
