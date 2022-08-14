package org.ergoplatform.modifiers.state

import scorex.crypto.authds.avltree.batch.{Insert, Lookup, Operation, Remove}
import scorex.util.ModifierId

case class StateChanges(
  toRemove: IndexedSeq[(ModifierId, Remove)],
  toAppend: IndexedSeq[(ModifierId, Insert)],
  toLookup: IndexedSeq[(ModifierId, Lookup)]
) {

  /**
    * First lookup for all leafs required by data inputs (never fails, but may return proof-of-non-existence),
    * then remove all leafs that should be removed, then add new leafs
    */
  val operations: IndexedSeq[(ModifierId, Operation)] = toLookup ++ toRemove ++ toAppend

}
