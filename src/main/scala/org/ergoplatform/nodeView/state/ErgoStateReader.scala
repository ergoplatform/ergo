package org.ergoplatform.nodeView.state

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.state.{Insertion, Removal, StateChanges}
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.transaction.state.StateReader
import scorex.crypto.authds.{ADDigest, ADKey}

trait ErgoStateReader extends StateReader {

  def rootHash: ADDigest

  /**
    * Extract ordered sequence of operations on UTXO set from set of transactions
    */
  protected def boxChanges(txs: Seq[ErgoTransaction]): StateChanges = {
    val opened: Seq[ADKey] = txs.flatMap(t => t.inputs.map(_.boxId))
    val openedSet: Set[ByteArrayWrapper] = opened.map(o => ByteArrayWrapper(o)).toSet
    val inserted: Seq[ErgoBox] = txs.flatMap(t => t.outputs)
    val insertedSet: Set[ByteArrayWrapper] = inserted.map(b => ByteArrayWrapper(b.id)).toSet
    val both: Set[ByteArrayWrapper] = insertedSet.intersect(openedSet)
    val toRemove = opened.filterNot(s => both.contains(ByteArrayWrapper(s)))
      .map(id => Removal(id))
    val toInsert = inserted.filterNot(s => both.contains(ByteArrayWrapper(s.id)))
      .map(b => Insertion(b))
    StateChanges(toRemove ++ toInsert)
  }
}
