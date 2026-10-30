package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.{ErgoFullBlock, NetworkObjectTypeId, SnapshotsInfoTypeId}
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.GenesisHeight
import org.ergoplatform.settings.{ChainSettings, Constants, ErgoSettings, NodeConfigurationSettings}
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec

/**
  * Trait that calculates next modifiers we should download to synchronize our full chain with headers chain
  */
trait ToDownloadProcessor
  extends FullBlockPruningProcessor with UtxoSetSnapshotProcessor with BasicReaders with ScorexLogging {

  import org.ergoplatform.utils.MapPimp

  protected val settings: ErgoSettings

  // A node is considering that the chain is synced if sees a block header with timestamp no more
  // than headerChainDiff blocks on average from future
  private lazy val headerChainDiff = settings.nodeSettings.headerChainDiff

  protected def nodeSettings: NodeConfigurationSettings = settings.nodeSettings

  protected def chainSettings: ChainSettings = settings.chainSettings

  protected def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain

  def isInBestChain(id: ModifierId): Boolean

  /**
    * @return estimated height of a best chain found in the network
    */
  def estimatedTip(): Option[Height]

  /**
    * @return min full block height the node has
    */
  def minFullBlockAvailable: Int = minimalFullBlockHeight

  /**
    * Get network object ids to download to synchronize full blocks or start UTXO set snapshot downlood
    * @param howManyPerType how many ModifierIds per ModifierTypeId to fetch
    * @param condition only ModifierIds which pass filter are included into results
    * @return next max howManyPerType ModifierIds by ModifierTypeId to download filtered by condition
    */
  def nextModifiersToDownload(howManyPerType: Int,
                              condition: (NetworkObjectTypeId.Value, ModifierId) => Boolean): Map[NetworkObjectTypeId.Value, Seq[ModifierId]] = {

    val FullBlocksToDownloadAhead = 192 // how many full blocks to download forwards during active sync

    def farAwayFromBeingSynced(fb: ErgoFullBlock): Boolean = fb.height < (estimatedTip().getOrElse(0) - 128)

    @tailrec
    def continuation(height: Int,
                     acc: Map[NetworkObjectTypeId.Value, Vector[ModifierId]],
                     maxHeight: Int): Map[NetworkObjectTypeId.Value, Vector[ModifierId]] = {
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
            if (height == maxHeight) newAcc else continuation(height + 1, newAcc, maxHeight)
          } else {
            acc
          }
        }
      }
    }

    // Include the ancestor itself in the bounded walk; a truncated walk is not an ancestor.
    def commonAncestorOf(fb: ErgoFullBlock): Option[Header] = if (isInBestChain(fb.id)) {
      None
    } else {
      val parentSteps = Math.max(0L, nodeSettings.keepVersions.toLong)
      val limit = Math.min(fb.height.toLong, parentSteps + 1L).toInt
      headerChainBack(limit, fb.header, h => isInBestChain(h.id)).headOption
        .filter(h => isInBestChain(h.id))
    }

    bestFullBlockOpt match {
      case _ if !isHeadersChainSynced || !nodeSettings.verifyTransactions =>
        // do not download full blocks if no headers-chain synced yet or SPV mode
        Map.empty
      case Some(fb) if farAwayFromBeingSynced(fb) =>
        // A heavier headers-chain can need sections below the old full-chain tip.
        val fromHeight = commonAncestorOf(fb)
          .map(h => Math.max(minimalFullBlockHeight, h.height + 1))
          .getOrElse(fb.height + 1)
        // Scanning backward must not shorten the forward window.
        val maxHeight = Math.min(fb.height.toLong + FullBlocksToDownloadAhead, Int.MaxValue.toLong).toInt
        continuation(fromHeight, Map.empty, maxHeight)
      case Some(fb) =>
        // when blockchain is about to be synced,
        // download children blocks of last 100 full blocks applied to the best chain, to get block sections from forks
        // (but not below minimal full block height: block sections below it fail `bsTooOld` validation and their sender
        // is penalized, and there could be no headers below it, e.g. after NiPoPoW bootstrap)
        val lookbackHeight = Math.max(1, fb.header.height - 100)
        val minHeight = if (lookbackHeight > minimalFullBlockHeight) {
          commonAncestorOf(fb)
            .map(h => Math.max(minimalFullBlockHeight, h.height + 1))
            .fold(lookbackHeight)(Math.min(lookbackHeight, _))
        } else {
          Math.max(minimalFullBlockHeight, fb.header.height - 100)
        }
        continuation(minHeight, Map.empty, maxHeight = Int.MaxValue)
      case None if (nodeSettings.utxoSettings.utxoBootstrap && !isUtxoSnapshotApplied) =>
        // if bootstrapping with UTXO set snapshot is chosen, and no snapshot applied yet, ask peers for snapshots
        if (utxoSetSnapshotDownloadPlan().isEmpty) {
          Map(SnapshotsInfoTypeId.value -> Seq.empty)
        } else {
          Map.empty
        }
      case None =>
        // if headers-chain is synced and no full blocks applied yet, find full block height to go from
        continuation(minimalFullBlockHeight, Map.empty, maxHeight = Int.MaxValue)
    }
  }

  /**
    * Checks whether it's time to download full chain, and returns toDownload modifiers
    */
  protected def toDownload(header: Header): Seq[(NetworkObjectTypeId.Value, ModifierId)] = {
    if (!nodeSettings.verifyTransactions) {
      // A regime that do not download and verify transaction
      Nil
    } else if (nodeSettings.utxoSettings.utxoBootstrap && !isUtxoSnapshotApplied) {
      // While bootstrapping from a UTXO set snapshot, do not download full block sections
      // until the snapshot has been applied. Block sections downloaded before the snapshot
      // would be stored as non-best and never applied to the freshly recreated state.
      // Headers-chain sync detection must still run here: otherwise, with nipopowBootstrap = false,
      // `isHeadersChainSynced` is never set, `nextModifiersToDownload` stays empty and the
      // snapshot is never requested. Only the in-memory flag may be set though: running
      // `updateBestFullBlock` would persist minimalFullBlockHeight, and `isUtxoSnapshotApplied`
      // is derived from it (readMinimalFullBlockHeight() > GenesisHeight), so with blocksToKeep >= 0
      // it would flip to true with no snapshot applied and the snapshot would be skipped forever.
      markHeadersSyncedIfFresh(header)
      Nil
    } else if (shouldDownloadBlockAtHeight(header.height)) {
      // Already synced and header is not too far back. Download required modifiers.
      requiredModifiersForHeader(header)
    } else {
      // Headers chain is synced after this header. Start downloading full blocks
      markHeadersSyncedIfFresh(header, updateBestBlock = true)
      Nil
    }
  }

  /**
    * Mark headers chain as synced when a fresh header arrives (from the network's perspective,
    * the chain is at the tip then). When `updateBestBlock` is true the best full block pointer
    * is persisted as well, which advances minimalFullBlockHeight; callers in the UTXO snapshot
    * bootstrap path must not do that (see above).
    * A node that can continue from its best full block (e.g. after a restart) keeps minimalFullBlockHeight,
    * which then moves as blocks are applied: a height computed from this header could be above the best
    * full block, and the blocks the node needs next would be rejected as too old. If such a height is
    * already stored (by an earlier version), it is lowered to the height after the best full block.
    * The node continues from its best full block only when a header is stored at the next height: after a NiPoPoW
    * bootstrap a full block can sit inside the proof's headers gap (for example the genesis block, with no header at
    * height 2), and then the height is computed from this header as before.
    */
  private def markHeadersSyncedIfFresh(header: Header, updateBestBlock: Boolean = false): Unit = {
    if (!isHeadersChainSynced && header.isNew(chainSettings.blockInterval * headerChainDiff)) {
      // A restored snapshot has no full block yet; keep its first download height until one is applied.
      val keepSnapshotFloor = updateBestBlock && nodeSettings.utxoSettings.utxoBootstrap &&
        isUtxoSnapshotApplied && bestFullBlockOpt.isEmpty
      if (updateBestBlock && !keepSnapshotFloor) {
        bestFullBlockOpt.filter(fb => headerIdsAtHeight(fb.height + 1).nonEmpty) match {
          case Some(fb) =>
            if (minimalFullBlockHeight > fb.height + 1) {
              log.warn(s"Stored minimal full block height $minimalFullBlockHeight is above the best full block " +
                s"${fb.header.encodedId} at height ${fb.height}, lowering it to ${fb.height + 1}")
              writeMinimalFullBlockHeight(fb.height + 1)
            }
            setHeadersChainSynced()
          case None =>
            val lowestHeight = lowestApplicableHeight(header)
            updateBestFullBlock(header, lowestHeight)
            if (lowestHeight > GenesisHeight) warnIfBestFullBlockBelowGap(header)
        }
      } else {
        setHeadersChainSynced()
      }
      log.info(s"Headers chain is likely synced after header ${header.encodedId} at height ${header.height}, " +
        s"minimal full block height $minimalFullBlockHeight")
    }
  }

  /**
    * After bootstrapping with a NiPoPoW proof, headers chain has gaps below the proof suffix. A full block can be
    * applied first only if its previous `LastHeadersInContext - 1` headers are connected to it (they are needed to
    * construct state context), so full blocks downloading must not start from a height below that.
    *
    * @return the lowest height a full block connected to `header` can be applied first at
    */
  private def lowestApplicableHeight(header: Header): Int = {
    if (nodeSettings.nipopowSettings.nipopowBootstrap && nodeSettings.blocksToKeep >= 0) {
      // no need to look further back than a full block downloading can start from
      val depth = nodeSettings.blocksToKeep + chainSettings.voting.votingLength + Constants.LastHeadersInContext
      val lowestConnected = headerChainBack(depth, header, _ => false).headOption.fold(header.height)(_.height)
      if (lowestConnected == GenesisHeight) GenesisHeight else lowestConnected + Constants.LastHeadersInContext - 1
    } else {
      GenesisHeight
    }
  }

  /**
    * A full block applied before its headers chain was connected (e.g. applied inside a NiPoPoW proof's headers gap by
    * a node version which did not consider the gap) can not be continued, as there is no header after it, and full
    * blocks downloading continues from the best full block. Such a database is not repaired, so tell the operator.
    */
  private def warnIfBestFullBlockBelowGap(header: Header): Unit = {
    bestFullBlockOpt.foreach { fb =>
      if (fb.height < header.height && headerIdsAtHeight(fb.height + 1).isEmpty) {
        log.warn(s"Best full block ${fb.encodedId} at height ${fb.height} has no header at height ${fb.height + 1} " +
          s"(headers chain has a gap after NiPoPoW bootstrap), so full blocks can not be applied after it. " +
          s"Resync the node (remove its database) to bootstrap again")
      }
    }
  }

  /**
    * @return block sections needed to be downloaded after header `h` , and defined by the header
    */
  def requiredModifiersForHeader(h: Header): Seq[(NetworkObjectTypeId.Value, ModifierId)] = {
    if (!nodeSettings.verifyTransactions) {
      Nil // no block sections to be downloaded in SPV mode
    } else if (nodeSettings.stateType.requireProofs) {
      h.sectionIds // download block transactions, extension and UTXO set transformations proofs in "digest" mode
    } else {
      h.sectionIdsWithNoProof // do not download UTXO set transformation proofs if UTXO set is stored
    }
  }

}
