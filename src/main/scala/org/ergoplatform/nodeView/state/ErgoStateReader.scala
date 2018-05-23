package org.ergoplatform.nodeView.state

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import scorex.core.transaction.state._
import scorex.crypto.authds.{ADDigest, ADKey}

trait ErgoStateReader extends StateReader {

  def rootHash: ADDigest

  /**
    * Extract ordered sequence of operations on UTXO set from set of transactions
    */
  def boxChanges(txs: Seq[AnyoneCanSpendTransaction]): BoxStateChanges[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox] = {
    val opened: Seq[ADKey] = txs.flatMap(t => t.boxIdsToOpen)
    val openedSet: Set[ByteArrayWrapper] = opened.map(o => ByteArrayWrapper(o)).toSet
    val inserted: Seq[AnyoneCanSpendNoncedBox] = txs.flatMap(t => t.newBoxes)
    val insertedSet: Set[ByteArrayWrapper] = inserted.map(b => ByteArrayWrapper(b.id)).toSet
    val both: Set[ByteArrayWrapper] = insertedSet.intersect(openedSet)
    val toRemove = opened.filterNot(s => both.contains(ByteArrayWrapper(s)))
      .map(id => Removal[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](id))
    val toInsert = inserted.filterNot(s => both.contains(ByteArrayWrapper(s.id)))
      .map(b => Insertion[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](b))
    BoxStateChanges[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](toRemove ++ toInsert)
  }
}
