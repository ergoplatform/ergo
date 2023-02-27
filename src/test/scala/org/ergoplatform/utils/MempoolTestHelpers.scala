package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.{ErgoMemPoolReader, UnconfirmedTransaction}
import scorex.util.ModifierId

trait MempoolTestHelpers {

  // mempool reader stub specifically for this test only take is defined as only this method is used in rebroadcasting
  class FakeMempool(txs: Seq[UnconfirmedTransaction]) extends ErgoMemPoolReader {

    override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = ???

    override def getAll(ids: Seq[ModifierId]): Seq[UnconfirmedTransaction] = ???

    override def size: Int = ???

    override def txTimesAndWeights: Seq[(Long,Long)] = ???

    override def getAll: Seq[UnconfirmedTransaction] = ???

    override def getAllPrioritized: Seq[UnconfirmedTransaction] = txs

    override def take(limit: Int): Iterable[UnconfirmedTransaction] = txs.take(limit)

    override def random(limit: Int): Iterable[UnconfirmedTransaction] = take(limit)

    override def spentInputs: Iterator[BoxId] = txs.map(_.transaction).flatMap(_.inputs).map(_.boxId).toIterator

    override def getRecommendedFee(expectedWaitTimeMinutes: Int, txSize: Int) : Long = 0

    override def getExpectedWaitTime(txFee: Long, txSize: Int): Long = 0

  }

}


