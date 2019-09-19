package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.transaction.MempoolReader
import scorex.util.ModifierId

trait ErgoMemPoolReader extends MempoolReader[ErgoTransaction] {

  override def contains(id: ModifierId): Boolean

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction]

  override def size: Int

  def getAll: Seq[ErgoTransaction]

  /** Returns all transactions resided in pool sorted by priority
    */
  def getAllPrioritized: Seq[ErgoTransaction]

  def take(limit: Int): Iterable[ErgoTransaction]

  def modifierById(modifierId: ModifierId): Option[ErgoTransaction]

  /** Returns a sequence of randomly selected transactions.
    * @param txsNum - number of transactions in the digest
    */
  def randomDigest(txsNum: Int): Seq[ErgoTransaction]

}
