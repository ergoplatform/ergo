package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.transaction.MempoolReader
import scorex.util.ModifierId

import scala.collection.concurrent.TrieMap

/** Unconfirmed read-only transactions pool reader. Should not update memory pool.
  *
  * @param readOnlyPool Thread-safe memory pool container for *read-only* purposes.
  *                     We should not update this pool to avoid synchronization
  *                     with other data structures in memory pool.
  */
class ErgoMempoolReader(readOnlyPool: TrieMap[ModifierId, ErgoTransaction]) extends MempoolReader[ErgoTransaction] {

  def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = {
    readOnlyPool.get(modifierId)
  }

  override def contains(id: ModifierId): Boolean = {
    readOnlyPool.contains(id)
  }

  def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = {
    ids.flatMap(modifierById)
  }

  def size: Int = {
    readOnlyPool.size
  }

  def take(limit: Int): Iterable[ErgoTransaction] = {
    readOnlyPool.values.toSeq.take(limit)
  }

  def unconfirmedTransactions: Iterable[ErgoTransaction] = {
    readOnlyPool.values
  }

}
