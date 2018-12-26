package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool.WeightedTxId
import scorex.core.transaction.MempoolReader
import scorex.util.ModifierId

import scala.collection.immutable.TreeMap

trait ErgoMemPoolReader extends MempoolReader[ErgoTransaction] {

  val unconfirmed: TreeMap[WeightedTxId, ErgoTransaction]

  override def contains(id: ModifierId): Boolean = unconfirmed.keys.exists(_.id == id)

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ids.flatMap(modifierById)

  override def size: Int = unconfirmed.size

  def getAll: Seq[ErgoTransaction] = unconfirmed.values.toSeq

  /** Returns all transactions resided in pool sorted by priority */
  def getAllPrioritized: Seq[ErgoTransaction] = unconfirmed.values.toList.reverse

  def take(limit: Int): Iterable[ErgoTransaction] = unconfirmed.values.toSeq.take(limit)

  def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = unconfirmed.values.find(_.id == modifierId)

}
