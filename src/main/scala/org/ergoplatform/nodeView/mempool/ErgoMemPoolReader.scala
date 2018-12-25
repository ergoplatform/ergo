package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.transaction.MempoolReader
import scorex.util.ModifierId

import scala.collection.immutable.TreeMap

trait ErgoMemPoolReader extends MempoolReader[ErgoTransaction] {

  val unconfirmed: TreeMap[ModifierId, (ErgoTransaction, Long)]

  override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = unconfirmed.get(modifierId).map(_._1)

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ids.flatMap(modifierById)

  override def size: Int = unconfirmed.size

  /** Returns all transactions resided in pool sorted by priority */
  def getAll: Seq[ErgoTransaction] = unconfirmed.values.map(_._1).toSeq.view.reverse

  def take(limit: Int): Iterable[ErgoTransaction] = unconfirmed.values.toSeq.take(limit).map(_._1)

}
