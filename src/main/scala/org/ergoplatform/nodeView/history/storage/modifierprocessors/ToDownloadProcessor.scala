package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.SnapshotsInfo
import org.ergoplatform.nodeView.state.UtxoState.ManifestId
import org.ergoplatform.settings.{ChainSettings, ErgoSettings, NodeConfigurationSettings}
import scorex.core.ModifierTypeId
import scorex.core.utils.NetworkTimeProvider
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec

/**
  * Trait that calculates next modifiers we should download to synchronize our full chain with headers chain
  */
trait ToDownloadProcessor extends FullBlockPruningProcessor with BasicReaders with ScorexLogging {
  import scorex.core.utils.MapPimp

  protected val timeProvider: NetworkTimeProvider

  protected val settings: ErgoSettings

  // A node is considering that the chain is synced if sees a block header with timestamp no more
  // than headerChainDiff blocks on average from future
  private lazy val headerChainDiff = settings.nodeSettings.headerChainDiff

  protected def nodeSettings: NodeConfigurationSettings = settings.nodeSettings

  protected def chainSettings: ChainSettings = settings.chainSettings

  protected def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain

  def isInBestChain(id: ModifierId): Boolean

  def estimatedTip(): Option[Height]

  private val manifestChosen: Option[ManifestId] = None

  /**
    * Get modifier ids to download to synchronize full blocks
    * @param howManyPerType how many ModifierIds per ModifierTypeId to fetch
    * @param filterFn only ModifierIds which pass filter are included into results
    * @return next max howManyPerType ModifierIds by ModifierTypeId to download filtered by condition
    */
  def nextModifiersToDownload(howManyPerType: Int,
                              condition: (ModifierTypeId, ModifierId) => Boolean): Map[ModifierTypeId, Seq[ModifierId]] = {

    val FullBlocksToDownloadAhead = 192 // how many full blocks to download forwards during active sync

    def farAwayFromBeingSynced(fb: ErgoFullBlock) = fb.height < (estimatedTip().getOrElse(0) - 128)

    @tailrec
    def continuation(height: Int,
                     acc: Map[ModifierTypeId, Vector[ModifierId]],
                     maxHeight: Int): Map[ModifierTypeId, Vector[ModifierId]] = {
      if (height > maxHeight) {
        acc
      } else {
        if (acc.values.exists(_.lengthCompare(howManyPerType) >= 0)) {
          // return if at least one of Modifier types reaches howManyPerType limit for modifier ids
          acc.mapValues(_.take(howManyPerType)).view.force
        } else {
          val headersAtThisHeight = headerIdsAtHeight(height).flatMap(id => typedModifierById[Header](id))

          if (headersAtThisHeight.nonEmpty) {
            val toDownload = headersAtThisHeight.flatMap(requiredModifiersForHeader).filter { case (mtid, mid) => condition(mtid, mid) }
            // add new modifiers to download to accumulator
            val newAcc = toDownload.foldLeft(acc) { case (newAcc, (mType, mId)) => newAcc.adjust(mType)(_.fold(Vector(mId))(_ :+ mId)) }
            continuation(height + 1, newAcc, maxHeight)
          } else {
            acc
          }
        }
      }
    }

    bestFullBlockOpt match {
      case _ if !isHeadersChainSynced || !nodeSettings.verifyTransactions =>
        // do not download full blocks if no headers-chain synced yet and suffix enabled or SPV mode
        Map.empty
      case Some(fb) if farAwayFromBeingSynced(fb) =>
        // when far away from blockchain tip
        continuation(fb.height + 1, Map.empty, fb.height + FullBlocksToDownloadAhead)
      case Some(fb) =>
        // when blockchain is about to be synced,
        // download children blocks of last 100 full blocks applied to the best chain, to get block sections from forks
        val minHeight = Math.max(1, fb.header.height - 100)
        continuation(minHeight, Map.empty, maxHeight = Int.MaxValue)
      case None if nodeSettings.utxoBootstrap =>
        if (manifestChosen.nonEmpty){
          ???
        } else {
          Map(SnapshotsInfo.modifierTypeId -> Seq.empty)
        }
      case None =>
        // if headers-chain is synced and no full blocks applied yet, find full block height to go from
        continuation(minimalFullBlockHeight, Map.empty, maxHeight = Int.MaxValue)
    }
  }

  /**
    * Checks whether it's time to download full chain, and returns toDownload modifiers
    */
  protected def toDownload(header: Header): Seq[(ModifierTypeId, ModifierId)] = {
    if (!nodeSettings.verifyTransactions) {
      // A regime that do not download and verify transaction
      Nil
    } else if (shouldDownloadBlockAtHeight(header.height)) {
      // Already synced and header is not too far back. Download required modifiers.
      requiredModifiersForHeader(header)
    } else if (!isHeadersChainSynced && header.isNew(timeProvider, chainSettings.blockInterval * headerChainDiff)) {
      // Headers chain is synced after this header. Start downloading full blocks
      updateBestFullBlock(header)
      log.info(s"Headers chain is likely synced after header ${header.encodedId} at height ${header.height}")
      Nil
    } else {
      Nil
    }
  }

  def requiredModifiersForHeader(h: Header): Seq[(ModifierTypeId, ModifierId)] = {
    if (!nodeSettings.verifyTransactions) {
      Nil
    } else if (nodeSettings.stateType.requireProofs) {
      h.sectionIds
    } else {
      h.sectionIdsWithNoProof // do not download UTXO set transformation proofs if UTXO set is stored
    }
  }

}
