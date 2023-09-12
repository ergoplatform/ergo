package org.ergoplatform.modifiers.state

import scorex.crypto.authds.avltree.batch.{Insert, Lookup, Operation, Remove}


case class StateChanges(toRemove: IndexedSeq[Remove], toAppend: IndexedSeq[Insert], toLookup: IndexedSeq[Lookup]) {

  /**
    * First lookup for all leafs required by data inputs (never fails, but may return proof-of-non-existence),
    * then remove all leafs that should be removed,
    * then add new leafs
    */
  val operations: Seq[Operation] = toLookup ++ toRemove ++ toAppend

}
