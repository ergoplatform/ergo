package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.wallet.scanning.Scan
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.wallet.boxes.TrackedBox

import scala.collection.compat.immutable.ArraySeq
import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scorex.util.ModifierId

/**
  * Represents the diff of applying an input block to the off-chain registry.
  * Used for efficient rollback without rebuilding from mempool.
  *
  * @param addedOffChain   - boxes added to offChainBoxes by the input block
  * @param removedOffChain - boxes removed from offChainBoxes by the input block
  * @param removedOnChain  - balances removed from onChainBalances by the input block
  */
case class InputBlockDiff(
  addedOffChain: Seq[TrackedBox],
  removedOffChain: Seq[TrackedBox],
  removedOnChain: Seq[Balance]
)

object InputBlockDiff {
  def empty: InputBlockDiff = InputBlockDiff(Seq.empty, Seq.empty, Seq.empty)
}

/**
  * Holds version-agnostic off-chain data (such as off-chain boxes) in runtime memory.
  * Needed to obtain wallet state in regards with unconfirmed transactions with no reprocessing them on each request.
  *
  * ==Input Block Transaction Accounting==
  *
  * Input blocks are a special kind of off-chain transaction batch that the wallet processes via `scanInputBlock`.
  * Unlike regular mempool transactions, input blocks are explicitly tracked with
  * reversible diffs to support rollback without rebuilding the entire offchain state.
  *
  * When input block transactions are confirmed on-chain (via `ScanOnChain`), the wallet:
  *
  *   - Removes the confirmed transactions from `inputBlockTxs` (see `ErgoWalletActor`)
  *   - The on-chain scan process naturally updates `onChainBalances` via `updateOnBlock`
  *   - Off-chain boxes that became on-chain are cleaned from `offChainBoxes`
  *
  * The wallet digest (balance) is computed as:
  *   `sum(offChainBoxes) + sum(onChainBalances)`
  *
  * Input block outputs contribute via `offChainBoxes`, while spent inputs are removed from
  * either `offChainBoxes` (if they were previously off-chain) or `onChainBalances` (if on-chain).
  *
  * @param height           - latest processed block height
  * @param offChainBoxes    - boxes from off-chain transactions (includes input block outputs)
  * @param onChainBalances  - on-chain balances snapshot (required to calculate off-chain indexes)
  * @param inputBlockDiffs  - map of input block id to diff, tracking per-block changes for rollback support
  */
case class OffChainRegistry(height: Int,
                            offChainBoxes: Seq[TrackedBox],
                            onChainBalances: Seq[Balance],
                            inputBlockDiffs: Map[ModifierId, InputBlockDiff] = Map.empty) {

  import org.ergoplatform.nodeView.wallet.IdUtils._

  /**
    * Off-chain index considering on-chain balances.
    */
  lazy val digest: WalletDigest = {
    val offChainBalances = offChainBoxes.map(Balance.apply)
    val balance = offChainBalances.map(_.value).sum + onChainBalances.map(_.value).sum
    val tokensBalance = (offChainBalances ++ onChainBalances)
      .flatMap(_.assets)
      .foldLeft(mutable.LinkedHashMap.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
        acc += id -> (acc.getOrElse(id, 0L) + amt)
      }
    WalletDigest(height, balance, tokensBalance.toSeq)
  }

  /**
    * Update on receiving new off-chain transaction.
    * Also returns the boxes and balances that were removed during the update.
    */
  def updateOnTransactionWithDiff(newBoxes: Seq[TrackedBox],
                                  spentIds: Seq[EncodedBoxId],
                                  scans: Seq[Scan]): (OffChainRegistry, Seq[TrackedBox], Seq[Balance]) = {
    val removedOffChain = offChainBoxes.filter(tb => spentIds.contains(tb.boxId))
    val removedOnChain = onChainBalances.filter(b => spentIds.contains(b.id))

    val unspentCertain = offChainBoxes.flatMap { x: TrackedBox =>
      val spent = spentIds.contains(x.boxId)
      if (spent) {
        if (x.scans.size > 1 || (x.scans.size == 1 && x.scans.head > Constants.PaymentsScanId)) {
          val leave = scans.exists(s => x.scans.contains(s.scanId) && !s.removeOffchain)
          if (leave) {
            Some(x)
          } else {
            None
          }
        } else {
          None
        }
      } else {
        Some(x)
      }
    } ++ newBoxes
    val onChainBalancesUpdated = onChainBalances.filterNot(x => spentIds.contains(x.id))
    val newRegistry = this.copy(
      offChainBoxes = unspentCertain.distinct,
      onChainBalances = onChainBalancesUpdated
    )
    (newRegistry, removedOffChain, removedOnChain)
  }

  /**
    * Update on receiving new off-chain transaction.
    */
  def updateOnTransaction(newBoxes: Seq[TrackedBox],
                          spentIds: Seq[EncodedBoxId],
                          scans: Seq[Scan]): OffChainRegistry = {
    updateOnTransactionWithDiff(newBoxes, spentIds, scans)._1
  }

  /**
    * Rollback changes from a specific input block using stored diff.
    * Removes added boxes and restores removed boxes/balances.
    *
    * @param inputBlockId - id of the input block to rollback
    * @return updated registry with the input block changes undone
    */
  def rollbackInputBlock(inputBlockId: ModifierId): OffChainRegistry = {
    inputBlockDiffs.get(inputBlockId) match {
      case Some(diff) =>
        val cleanedOffChain = offChainBoxes.filterNot(tb =>
          diff.addedOffChain.exists(_.boxId == tb.boxId)
        )
        val restoredOffChain = diff.removedOffChain.filterNot(rb =>
          cleanedOffChain.exists(_.boxId == rb.boxId)
        )
        val newOffChainBoxes = (cleanedOffChain ++ restoredOffChain).distinct

        val restoredOnChain = diff.removedOnChain.filterNot(rb =>
          onChainBalances.exists(_.id == rb.id)
        )
        val newOnChainBalances = (onChainBalances ++ restoredOnChain).distinct

        this.copy(
          offChainBoxes = newOffChainBoxes,
          onChainBalances = newOnChainBalances,
          inputBlockDiffs = inputBlockDiffs - inputBlockId
        )
      case None =>
        this
    }
  }

  /**
    * Update balances snapshot according to a new block applied
    *
    * @param newHeight       - processed block height
    * @param allCertainBoxes -  all the unspent boxes to the moment
    * @param onChainIds      - ids of all boxes which became on-chain in result of a current block application
    */
  def updateOnBlock(newHeight: Int,
                    allCertainBoxes: Seq[TrackedBox],
                    onChainIds: TreeSet[EncodedBoxId]): OffChainRegistry = {
    val updatedOnChainBalances = allCertainBoxes.map(Balance.apply)
    val cleanedOffChainBoxes = offChainBoxes.filterNot(b => onChainIds.contains(EncodedBoxId @@@ b.boxId))
    this.copy(
      height = newHeight,
      offChainBoxes = cleanedOffChainBoxes,
      onChainBalances = updatedOnChainBalances
    )
  }

}

object OffChainRegistry {

  def empty: OffChainRegistry =
    OffChainRegistry(EmptyHistoryHeight, ArraySeq.empty, ArraySeq.empty, Map.empty)

  def init(walletRegistry: WalletRegistry): OffChainRegistry = {
    val unspent = walletRegistry.unspentBoxes(PaymentsScanId)
    val h = walletRegistry.fetchDigest().height
    OffChainRegistry(h, ArraySeq.empty, unspent.map(Balance.apply), Map.empty)
  }

}
