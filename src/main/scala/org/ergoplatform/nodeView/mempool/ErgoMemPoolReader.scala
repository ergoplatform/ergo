package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import scorex.core.transaction.MempoolReader
import scorex.util.ModifierId

trait ErgoMemPoolReader extends MempoolReader[ErgoTransaction] {

  override def contains(id: ModifierId): Boolean

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction]

  override def size: Int

  def getAll: Seq[ErgoTransaction]

  /**
    * Returns all transactions resided in pool sorted by weight
    */
  def getAllPrioritized: Seq[ErgoTransaction]


  /**
    * Returns given transactions resided in pool sorted by weight
    */
  def take(limit: Int): Iterable[ErgoTransaction]

  def modifierById(modifierId: ModifierId): Option[ErgoTransaction]

  def weightedTransactions(limit: Int): Seq[WeightedTxId]
}
