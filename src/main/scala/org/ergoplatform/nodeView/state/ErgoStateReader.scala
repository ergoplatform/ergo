package org.ergoplatform.nodeView.state

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.{ErgoBox, ErgoTransaction}
import scorex.core.transaction.state._
import scorex.crypto.authds.{ADDigest, ADKey}

trait ErgoStateReader extends StateReader {

  def rootHash: ADDigest

  /**
    * Extract ordered sequence of operations on UTXO set from set of transactions
    */
  def boxChanges(txs: Seq[ErgoTransaction]): BoxStateChanges[AnyoneCanSpendProposition.type, ErgoBox] = {
    val opened: Seq[ADKey] = txs.flatMap(t => t.boxIdsToOpen)
    val openedSet: Set[ByteArrayWrapper] = opened.map(o => ByteArrayWrapper(o)).toSet
    val inserted: Seq[ErgoBox] = txs.flatMap(t => t.newBoxes)
    val insertedSet: Set[ByteArrayWrapper] = inserted.map(b => ByteArrayWrapper(b.id)).toSet
    val both: Set[ByteArrayWrapper] = insertedSet.intersect(openedSet)
    val toRemove = opened.filterNot(s => both.contains(ByteArrayWrapper(s)))
      .map(id => Removal[AnyoneCanSpendProposition.type, ErgoBox](id))
    val toInsert = inserted.filterNot(s => both.contains(ByteArrayWrapper(s.id)))
      .map(b => Insertion[AnyoneCanSpendProposition.type, ErgoBox](b))
    BoxStateChanges[AnyoneCanSpendProposition.type, ErgoBox](toRemove ++ toInsert)
  }
}
