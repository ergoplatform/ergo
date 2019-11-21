package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.wallet.boxes.TrackedBox

/**
  * Holds version-agnostic offchain data (such as off-chain boxes) in runtime memory.
  * @param height - latest processed block height
  * @param offChainBalances - balances from off-chain transactions
  * @param onChainBalances - on-chain balances snapshot (required to calculate off-chain indexes)
  */
final case class OffChainRegistry(height: Int,
                                  offChainBalances: Seq[Balance],
                                  onChainBalances: Seq[Balance]) {

  import org.ergoplatform.nodeView.wallet.IdUtils._

  /**
    * Off-chain index considering on-chain balances.
    */
  val readIndex: RegistryDigest = {
    val balance = offChainBalances.map(_.value).sum + onChainBalances.map(_.value).sum
    val tokensBalance = (offChainBalances ++ onChainBalances)
      .flatMap(_.assets)
      .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
        acc.updated(id, acc.getOrElse(id, 0L) + amt)
      }
    RegistryDigest(height, balance, tokensBalance, Map.empty, Map.empty, Map.empty) //todo: provide app data here?
  }

  /**
    * Update on receiving new off-chain transaction.
    */
  def updated(certainBoxes: Seq[TrackedBox],
              spentIds: Seq[EncodedBoxId]): OffChainRegistry = {
    val unspentCertain = offChainBalances.filterNot(x => spentIds.contains(x.id)) ++
      certainBoxes.map { tb =>
        Balance(encodedBoxId(tb.box.id), tb.box.value,
          tb.box.additionalTokens.toArray.map(x => encodedTokenId(x._1) -> x._2).toMap)
      }
    val onChainBalancesUpdated = onChainBalances.filterNot(x => spentIds.contains(x.id))
    this.copy(
      offChainBalances = unspentCertain.distinct,
      onChainBalances = onChainBalancesUpdated
    )
  }

  /**
    * Update balances snapshot according to a new block applied
    * @param newHeight - processed block height
    * @param allCertainBoxes - all unspent boxes
    * @param onChainIds - ids of all boxes which became on-chain in result of current block application
    */
  def updateOnBlock(newHeight: Int,
                    allCertainBoxes: Seq[TrackedBox],
                    onChainIds: Seq[EncodedBoxId]): OffChainRegistry = {
    val updatedOnChainBalances = allCertainBoxes.map { tb =>
      Balance(encodedBoxId(tb.box.id), tb.box.value,
        tb.box.additionalTokens.toArray.map(x => encodedTokenId(x._1) -> x._2).toMap)
    }
    val cleanedOffChainBalances = offChainBalances.filterNot(b => onChainIds.contains(b.id))
    this.copy(
      height = newHeight,
      offChainBalances = cleanedOffChainBalances,
      onChainBalances = updatedOnChainBalances
    )
  }

}

object OffChainRegistry {

  def empty: OffChainRegistry =
    OffChainRegistry(ErgoHistory.EmptyHistoryHeight, Seq.empty, Seq.empty)

}
