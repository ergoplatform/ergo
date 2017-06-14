package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool

import scala.util.Try

class ErgoMemPool extends MemoryPool[AnyoneCanSpendTransaction, ErgoMemPool] {
  override def getById(id: ModifierId): Option[AnyoneCanSpendTransaction] = ???

  override def contains(id: ModifierId): Boolean = ???

  override def getAll(ids: Seq[ModifierId]): Seq[AnyoneCanSpendTransaction] = ???

  override def put(tx: AnyoneCanSpendTransaction): Try[ErgoMemPool] = ???

  override def put(txs: Iterable[AnyoneCanSpendTransaction]): Try[ErgoMemPool] = ???

  override def putWithoutCheck(txs: Iterable[AnyoneCanSpendTransaction]): ErgoMemPool = ???

  override def remove(tx: AnyoneCanSpendTransaction): ErgoMemPool = ???

  override def take(limit: Int): Iterable[AnyoneCanSpendTransaction] = ???

  override def filter(condition: (AnyoneCanSpendTransaction) => Boolean): ErgoMemPool = ???

  override def size: Int = ???

  override type NVCT = this.type
}
