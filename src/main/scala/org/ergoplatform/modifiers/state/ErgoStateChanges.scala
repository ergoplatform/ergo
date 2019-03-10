package org.ergoplatform.modifiers.state

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey

/**
  * Operations that can be performed over state boxes.
  */
sealed trait StateChangeOperation {
  val boxId: ADKey

  override def hashCode(): Int = Ints.fromByteArray(boxId)

  protected def sameType(that: StateChangeOperation): Boolean

  override def equals(that: Any): Boolean = that match {
    case that: StateChangeOperation if sameType(that) => java.util.Arrays.equals(boxId, that.boxId)
    case _ => false
  }

}

case class Lookup(boxId: ADKey) extends StateChangeOperation {

  override protected def sameType(that: StateChangeOperation): Boolean = that.isInstanceOf[Lookup]

  override def toString: String = s"Lookup(id: ${Algos.encode(boxId)})"
}

case class Removal(boxId: ADKey) extends StateChangeOperation {

  override protected def sameType(that: StateChangeOperation): Boolean = that.isInstanceOf[Removal]

  override def toString: String = s"Removal(id: ${Algos.encode(boxId)})"

}

case class Insertion(box: ErgoBox) extends StateChangeOperation {

  override protected def sameType(that: StateChangeOperation): Boolean = that.isInstanceOf[Insertion]

  override val boxId: ADKey = box.id

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
