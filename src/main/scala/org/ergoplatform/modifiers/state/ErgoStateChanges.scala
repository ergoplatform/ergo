package org.ergoplatform.modifiers.state

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey


abstract class StateChangeOperation

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

case class StateChanges(toRemove: Seq[Removal], toAppend: Seq[Insertion]) {
  val operations: Seq[StateChangeOperation] = toRemove ++ toAppend
}
