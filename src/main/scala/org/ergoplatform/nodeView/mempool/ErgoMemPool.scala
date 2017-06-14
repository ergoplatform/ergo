package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool._
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.core.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.Try

case class ErgoMemPool(unconfirmed: TrieMap[TxKey, AnyoneCanSpendTransaction])
  extends MemoryPool[AnyoneCanSpendTransaction, ErgoMemPool] with ScorexLogging {
  override type NVCT = ErgoMemPool

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
}

object ErgoMemPool {
  type TxKey = scala.collection.mutable.WrappedArray.ofByte
}