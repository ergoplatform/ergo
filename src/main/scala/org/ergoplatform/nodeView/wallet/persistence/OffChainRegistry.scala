package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.wallet.boxes.TrackedBox

/**
  * Holds version-agnostic indexes (such as off-chain boxes) in runtime memory.
  *
  * @param height           - latest processed block height
  * @param offChainBoxes    - boxes from off-chain transactions
  * @param onChainBalances  - on-chain balances snapshot (required to calculate off-chain indexes)
  */
case class OffChainRegistry(height: Int,
                            offChainBoxes: Seq[TrackedBox],
                            onChainBalances: Seq[Balance]) {

  import org.ergoplatform.nodeView.wallet.IdUtils._

  /**
    * Off-chain index considering on-chain balances.
    */
  val readIndex: RegistryIndex = {
    val offChainBalances = offChainBoxes.map(Balance.apply)
    val balance = offChainBalances.map(_.value).sum + onChainBalances.map(_.value).sum
    val tokensBalance = (offChainBalances ++ onChainBalances)
      .flatMap(_.assets)
      .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
        acc.updated(id, acc.getOrElse(id, 0L) + amt)
      }
    RegistryIndex(height, balance, tokensBalance, Seq.empty)
  }

  /**
    * Update on receiving new off-chain transaction.
    */
  def updated(createdBoxes: Seq[TrackedBox],
              spentIds: Seq[EncodedBoxId]): OffChainRegistry = {
    val unspentCertain = offChainBoxes.filterNot(x => spentIds.contains(x.boxId)) ++ createdBoxes
    val onChainBalancesUpdated = onChainBalances.filterNot(x => spentIds.contains(x.id))
    this.copy(
      offChainBoxes = unspentCertain.distinct,
      onChainBalances = onChainBalancesUpdated
    )
  }

  /**
    * Update balances snapshot according to a new block applied
    *
    * @param newHeight       - processed block height
    * @param allCertainBoxes - all certain boxes extracted from block
    *                        (required to update on-chain snapshot)
    * @param onChainIds      - ids of all boxes which became on-chain in result of a current block application
    */
  def updateOnBlock(newHeight: Int,
                    allCertainBoxes: Seq[TrackedBox],
                    onChainIds: Seq[EncodedBoxId]): OffChainRegistry = {
    val updatedOnChainBalances = allCertainBoxes.map(Balance.apply)
    val cleanedOffChainBoxes = offChainBoxes.filterNot(b => onChainIds.contains(b.boxId))
    this.copy(
      height = newHeight,
      offChainBoxes = cleanedOffChainBoxes,
      onChainBalances = updatedOnChainBalances
    )
  }

}

object OffChainRegistry {

  def empty: OffChainRegistry =
    OffChainRegistry(ErgoHistory.EmptyHistoryHeight, Seq.empty, Seq.empty)

  def init(walletRegistry: WalletRegistry):OffChainRegistry = {
    val idx = walletRegistry.readIndex
    val unspent = walletRegistry.readCertainUnspentBoxes
    val h = idx.height
    OffChainRegistry(h, Seq.empty, unspent.map(Balance.apply))
  }

}
