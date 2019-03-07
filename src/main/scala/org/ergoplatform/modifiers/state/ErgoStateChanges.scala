package org.ergoplatform.modifiers.state

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey


abstract class StateChangeOperation

case class Lookup(boxId: ADKey) extends StateChangeOperation {
  override def equals(that: Any): Boolean = that match {
    case that: Removal => java.util.Arrays.equals(boxId, that.boxId)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(boxId)

  override def toString: String = s"Lookup(id: ${Algos.encode(boxId)})"
}

case class Removal(boxId: ADKey) extends StateChangeOperation {
  override def equals(that: Any): Boolean = that match {
    case that: Removal => java.util.Arrays.equals(boxId, that.boxId)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(boxId)

  override def toString: String = s"Removal(id: ${Algos.encode(boxId)})"
}

case class Insertion(box: ErgoBox) extends StateChangeOperation {
  override def equals(that: Any): Boolean = that match {
    case that: Insertion => box == that.box
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(box.id)

  override def toString: String = s"Insertion(id: ${Algos.encode(box.id)})"
}

case class StateChanges(toRemove: Seq[Removal], toAppend: Seq[Insertion], toLookup: Seq[Lookup]) {

  /**
    * First lookup for all leafs required by data inputs (never fails, but may return proof-of-non-existence),
    * then remove all leafs that should be removed,
    * then add new leafs
    */
  val operations: Seq[StateChangeOperation] = toLookup ++ toRemove ++ toAppend

}
