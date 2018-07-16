package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool._
import scorex.core.ModifierId
import scorex.core.transaction.MemoryPool

import scala.collection.concurrent.TrieMap
import scala.util.Try

/**
  *
  * TODO This is simplified implementation of Memory pool.
  * MemPool should:
  * - have limited size
  * - replace transactions with the lowest fee if size limit is reached
  * - validate transactions when put (is called)
  * - clean transactions, that become invalid
  */
class ErgoMemPool private[mempool](val unconfirmed: TrieMap[TxKey, ErgoTransaction])
  extends MemoryPool[ErgoTransaction, ErgoMemPool] with ErgoMemPoolReader {

  override type NVCT = ErgoMemPool

  override def put(tx: ErgoTransaction): Try[ErgoMemPool] = put(Seq(tx))

  override def put(txs: Iterable[ErgoTransaction]): Try[ErgoMemPool] = Try {
    putWithoutCheck(txs.filterNot(tx => unconfirmed.contains(key(tx.id))))
  }

  override def putWithoutCheck(txs: Iterable[ErgoTransaction]): ErgoMemPool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    completeAssembly(txs)
    this
  }

  override def remove(tx: ErgoTransaction): ErgoMemPool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  override def filter(condition: (ErgoTransaction) => Boolean): ErgoMemPool = {
    unconfirmed.retain {case (k, v) =>
      condition(v)
    }
    this
  }
}


object ErgoMemPool {
  type TxKey = scala.collection.mutable.WrappedArray.ofByte

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[ErgoTransaction]

  def empty: ErgoMemPool = new ErgoMemPool(TrieMap.empty)
}
