package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import scorex.core.transaction.MempoolReader
import scorex.util.ModifierId

trait ErgoMemPoolReader extends MempoolReader[ErgoTransaction] {

  override def contains(id: ModifierId): Boolean

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction]

  override def size: Int

  /**
    * @return inputs spent by the mempool transactions
    */
  def spentInputs: Iterator[BoxId]

  def getAll: Seq[ErgoTransaction]

  /**
    * Returns all transactions resided in pool sorted by weight in descending order
    */
  def getAllPrioritized: Seq[ErgoTransaction]


  /**
    * Returns given number of transactions resided in pool sorted by weight in descending order
    */
  def take(limit: Int): Iterable[ErgoTransaction]

  def modifierById(modifierId: ModifierId): Option[ErgoTransaction]

  /**
    * Returns transaction ids with weights. Weight depends on a fee a transaction is paying.
    * Resulting transactions are sorted by weight in descending order.
    *
    * @param limit - number of weighted transactions to return
    * @return an ordered sequence of transaction ids with weights
    */
  def weightedTransactionIds(limit: Int): Seq[WeightedTxId]

}
