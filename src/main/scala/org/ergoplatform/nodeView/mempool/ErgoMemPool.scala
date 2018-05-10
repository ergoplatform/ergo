package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool._
import scorex.core.ModifierId
import scorex.core.transaction.MemoryPool

import scala.collection.concurrent.TrieMap
import scala.util.Try

class ErgoMemPool private[mempool](val unconfirmed: TrieMap[TxKey, AnyoneCanSpendTransaction])
  extends MemoryPool[AnyoneCanSpendTransaction, ErgoMemPool] with ErgoMemPoolReader {

  override type NVCT = ErgoMemPool

  override def put(tx: AnyoneCanSpendTransaction): Try[ErgoMemPool] = put(Seq(tx))

  override def put(txs: Iterable[AnyoneCanSpendTransaction]): Try[ErgoMemPool] = Try {
    //todo check validity
    putWithoutCheck(txs.filterNot(tx => unconfirmed.contains(key(tx.id))))
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

  override def filter(condition: (AnyoneCanSpendTransaction) => Boolean): ErgoMemPool = {
    unconfirmed.retain { (k, v) =>
      condition(v)
    }
    this
  }

}

object ErgoMemPool {
  type TxKey = scala.collection.mutable.WrappedArray.ofByte

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[AnyoneCanSpendTransaction]

  def empty: ErgoMemPool = new ErgoMemPool(TrieMap.empty)
}
