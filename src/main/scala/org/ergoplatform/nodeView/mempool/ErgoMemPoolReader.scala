package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.NodeViewComponent
import scorex.core.consensus.ContainsModifiers
import scorex.util.ModifierId

trait ErgoMemPoolReader extends NodeViewComponent with ContainsModifiers[ErgoTransaction] {

  /**
    * @param id -  transaction id
    * @return `true` if mempool holds this transaction or it was invalidated earlier, `false` otherwise
    */
  def contains(id: ModifierId): Boolean

  def getAll(ids: Seq[ModifierId]): Seq[UnconfirmedTransaction]

  def size: Int

  /**
    * @return inputs spent by the mempool transactions
    */
  def spentInputs: Iterator[BoxId]

  def getAll: Seq[UnconfirmedTransaction]

  /**
    * Returns all transactions resided in pool sorted by weight in descending order
    */
  def getAllPrioritized: Seq[UnconfirmedTransaction]


  /**
    * Returns given number of transactions resided in pool sorted by weight in descending order
    */
  def take(limit: Int): Iterable[UnconfirmedTransaction]

  /**
    * Returns up to given number of transactions randomly
    */
  def random(limit: Int): Iterable[UnconfirmedTransaction]

  def modifierById(modifierId: ModifierId): Option[ErgoTransaction]

  /**
    * Returns an array of tuples representing transaction creation times with weights. Weight depends on a fee a transaction is paying.
    * Resulting array is are sorted by weight in descending order.
    *
    * @return an ordered sequence of transaction ids with weights
    */
  def txTimesAndWeights: Seq[(Long,Long)]

  /**
    * Get expected wait time for the transaction with specified fee and size
    * @param txFee transaction fee
    * @param txSize size of transaction (in bytes)
    * @return average time in milliseconds for this transaction to be placed in block
    */
  def getExpectedWaitTime(txFee: Long, txSize: Int): Long

  /**
    * Get recommended fee for transaction with specified size to be placed in pool within specified interval of time
    * @param expectedWaitTimeMinutes maximum delay for transaction to get out of the mempool
    * @param txSize size of transaction (in bytes)
    * @return recommended fee value for transaction to be proceeded in specified time
    */
  def getRecommendedFee(expectedWaitTimeMinutes: Int, txSize: Int) : Long

}
