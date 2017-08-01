package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool._
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.{Success, Try}

class ErgoMemPool private[mempool](val unconfirmed: TrieMap[TxKey, AnyoneCanSpendTransaction])
  extends MemoryPool[AnyoneCanSpendTransaction, ErgoMemPool] {

  override type NVCT = ErgoMemPool

  /**
    * Map stores current state of waiting for query building
    * value - promise of result and set of all transactions of request
    * key - set of transactions that are waiting for the assembly
    */
  private[mempool] var waitedForAssembly: Map[Set[TxKey], (Promise[MemPoolResponse], Seq[ModifierId])] = Map.empty

  private def key(id: Array[Byte]): TxKey = new mutable.WrappedArray.ofByte(id)

  override def getById(id: ModifierId): Option[AnyoneCanSpendTransaction] = unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[AnyoneCanSpendTransaction] = ids.flatMap(getById)

  override def put(tx: AnyoneCanSpendTransaction): Try[ErgoMemPool] = put(Seq(tx))

  override def put(txs: Iterable[AnyoneCanSpendTransaction]): Try[ErgoMemPool] = Try {
    txs.foreach(tx => require(!unconfirmed.contains(key(tx.id))))
    //todo check validity
    putWithoutCheck(txs)
  }

  override def putWithoutCheck(txs: Iterable[AnyoneCanSpendTransaction]): ErgoMemPool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    completeAssembly(txs)
    //todo cleanup?
    this
  }

  override def remove(tx: AnyoneCanSpendTransaction): ErgoMemPool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  override def take(limit: Int): Iterable[AnyoneCanSpendTransaction] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: (AnyoneCanSpendTransaction) => Boolean): ErgoMemPool = {
    unconfirmed.retain { (k, v) =>
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size

  private def completeAssembly(txs: Iterable[AnyoneCanSpendTransaction]): Unit = synchronized {
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

object ErgoMemPool {
  type TxKey = scala.collection.mutable.WrappedArray.ofByte

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[AnyoneCanSpendTransaction]

  def empty: ErgoMemPool = new ErgoMemPool(TrieMap.empty)
}
