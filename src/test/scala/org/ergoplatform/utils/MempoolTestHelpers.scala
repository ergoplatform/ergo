package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.{ErgoMemPoolReader, OrderedTxPool}
import scorex.util.ModifierId

trait MempoolTestHelpers {

  // mempool reader stub specifically for this test only take is defined as only this method is used in rebroadcasting
  class FakeMempool(txs: Seq[ErgoTransaction]) extends ErgoMemPoolReader {

    override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = ???

    override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ???

    override def size: Int = ???

    override def weightedTransactionIds(limit: Int): Seq[OrderedTxPool.WeightedTxId] = ???

    override def getAll: Seq[ErgoTransaction] = ???

    override def getAllPrioritized: Seq[ErgoTransaction] = txs

    override def take(limit: Int): Iterable[ErgoTransaction] = txs.take(limit)

    override def random(limit: Int): Iterable[ErgoTransaction] = take(limit)

    override def spentInputs: Iterator[BoxId] = txs.flatMap(_.inputs).map(_.boxId).toIterator

    override def getRecommendedFee(expectedWaitTimeMinutes: Int, txSize: Int) : Long = 0

    override def getExpectedWaitTime(txFee: Long, txSize: Int): Long = 0

  }

}


