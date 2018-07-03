package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.{ChainSettings, NodeConfigurationSettings}
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.core.{ModifierId, ModifierTypeId}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

/**
  * Trait that calculates next modifiers we should download to synchronize our full chain with headers chain
  */
trait ToDownloadProcessor extends ScorexLogging {

  protected[history] lazy val pruningProcessor: FullBlockPruningProcessor = new FullBlockPruningProcessor(config)

  protected val config: NodeConfigurationSettings

  protected val chainSettings: ChainSettings

  protected val timeProvider: NetworkTimeProvider

  /**
    * Ids of block sections we are waiting to create block from full block
    */
  private val missedModifiers: ArrayBuffer[(ModifierTypeId, ModifierId)] =
    ArrayBuffer[(ModifierTypeId, ModifierId)]()

  private var isHeadersChainSyncedVar: Boolean = false

  def bestFullBlockOpt: Option[ErgoFullBlock]

  def headerIdsAtHeight(height: Int): Seq[ModifierId]

  def typedModifierById[T <: ErgoPersistentModifier](id: ModifierId): Option[T]

  def contains(id: ModifierId): Boolean

  /**
    * @return true if we estimate, that our chain is synced with the network. Start downloading full blocks after that
    */
  def isHeadersChainSynced: Boolean = isHeadersChainSyncedVar

  /**
    * Add ids of modifiers, required for full chain to `missedModifiers` list.
    * One-time function that runs, when headers chain is synced
    */
  private def updateMissedModifiers(): Unit = {
    @tailrec
    def continuation(height: Int): Unit = {
      headerIdsAtHeight(height).headOption.flatMap(id => typedModifierById[Header](id)) match {
        case Some(bestHeaderAtThisHeight) =>
          requiredModifiersForHeader(bestHeaderAtThisHeight)
            .filter(m => !contains(m._2))
            .foreach(m => missedModifiers += m)
          continuation(height + 1)
        case None =>
      }
    }

    continuation(pruningProcessor.minimalFullBlockHeight)
  }

  /**
    * Function that is called on successful processing of a header `h`
    *
    * @param h - processed header
    */
  protected def onProcess(h: Header): Unit = if (isHeadersChainSynced) {
    requiredModifiersForHeader(h).foreach(m => missedModifiers += m)
  }

  /**
    * Next howMany modifiers we should download to synchronize full block chain with headers chain
    */
  def missedModifiersForFullChain(howMany: Int, excluding: Iterable[ModifierId]): Seq[(ModifierTypeId, ModifierId)] = {
    if (excluding.isEmpty) {
      missedModifiers.take(howMany)
    } else {
      @tailrec
      def collect(i: Int, remaining: Int, acc: Seq[(ModifierTypeId, ModifierId)]): Seq[(ModifierTypeId, ModifierId)] = {
        if (remaining > 0) {
          Try(missedModifiers(i)) match {
            case Success(id) if !excluding.exists(_ sameElements id._2) => collect(i + 1, remaining - 1, id +: acc)
            case Success(_) => collect(i + 1, remaining, acc)
            case Failure(_) => acc
          }
        } else {
          acc
        }
      }

      collect(0, howMany, Seq())
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
      updateMissedModifiers()
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
