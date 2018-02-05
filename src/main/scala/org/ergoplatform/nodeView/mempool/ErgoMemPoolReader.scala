package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool._
import scorex.core.ModifierId
import scorex.core.transaction.MempoolReader

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Success

trait ErgoMemPoolReader extends MempoolReader[AnyoneCanSpendTransaction] {

  val unconfirmed: TrieMap[TxKey, AnyoneCanSpendTransaction]

  /**
    * Map stores current state of waiting for query building
    * value - promise of result and set of all transactions of request
    * key - set of transactions that are waiting for the assembly
    */
  protected[mempool] var waitedForAssembly: Map[Set[TxKey], (Promise[MemPoolResponse], Seq[ModifierId])] = Map.empty

  protected def key(id: ModifierId): TxKey = new mutable.WrappedArray.ofByte(id)

  override def getById(id: ModifierId): Option[AnyoneCanSpendTransaction] = unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[AnyoneCanSpendTransaction] = ids.flatMap(getById)

  override def size: Int = unconfirmed.size

  def take(limit: Int): Iterable[AnyoneCanSpendTransaction] =
    unconfirmed.values.toSeq.take(limit)

  //TODO rework option.get
  protected def completeAssembly(txs: Iterable[AnyoneCanSpendTransaction]): Unit = synchronized {
    val txsIds = txs.map(tx => key(tx.id))
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
    val promise = Promise[Seq[AnyoneCanSpendTransaction]]
    waitedForAssembly = waitedForAssembly.updated(ids.map(id => key(id)).toSet, (promise, ids))
    promise.future
  }

}