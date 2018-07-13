package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool._
import scorex.core.ModifierId
import scorex.core.transaction.MempoolReader

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Success

trait ErgoMemPoolReader extends MempoolReader[ErgoTransaction] {

  val unconfirmed: TrieMap[ModifierId, ErgoTransaction]

  /**
    * Map stores current state of waiting for query building
    * value - promise of result and set of all transactions of request
    * key - set of transactions that are waiting for the assembly
    */
  protected[mempool] var waitedForAssembly: Map[Set[ModifierId], (Promise[MemPoolResponse], Seq[ModifierId])] = Map.empty

  override def getById(id: ModifierId): Option[ErgoTransaction] = unconfirmed.get(id)

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ids.flatMap(getById)

  override def size: Int = unconfirmed.size

  def take(limit: Int): Iterable[ErgoTransaction] =
    unconfirmed.values.toSeq.take(limit)

  //TODO rework option.get
  protected def completeAssembly(txs: Iterable[ErgoTransaction]): Unit = synchronized {
    val txsIds = txs.map(_.id)
    val newMap = waitedForAssembly.flatMap(p => {
      val ids = p._1
      val newKey = ids -- txsIds
      // filtering fully-built queries and completing of a promise
      if (newKey.isEmpty) {
        val (promise, allIds) = p._2
        promise complete Success(allIds.map(id => getById(id).get))
        None
      } else {
        Some(newKey -> p._2)
      }
    })
    waitedForAssembly = newMap
  }

  def waitForAll(ids: MemPoolRequest): Future[MemPoolResponse] = synchronized {
    val promise = Promise[Seq[ErgoTransaction]]
    waitedForAssembly = waitedForAssembly.updated(ids.toSet, (promise, ids))
    promise.future
  }
}
