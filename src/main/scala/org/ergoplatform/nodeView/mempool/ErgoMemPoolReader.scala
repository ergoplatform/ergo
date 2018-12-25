package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.transaction.MempoolReader
import scorex.util.ModifierId

import scala.collection.immutable.TreeMap

trait ErgoMemPoolReader extends MempoolReader[ErgoTransaction] {

  val unconfirmed: TreeMap[ModifierId, ErgoTransaction]

  val weights: TreeMap[Long, ModifierId]

  override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = unconfirmed.get(modifierId)

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ids.flatMap(modifierById)

  override def size: Int = unconfirmed.size

  def getAll: Seq[ErgoTransaction] = unconfirmed.values.toSeq

  /** Returns all transactions resided in pool sorted by priority */
  def getAllPrioritized: Seq[ErgoTransaction] = weights.toSeq.view.reverse.flatMap(x => modifierById(x._2))

  def take(limit: Int): Iterable[ErgoTransaction] = unconfirmed.values.toSeq.take(limit)

}
