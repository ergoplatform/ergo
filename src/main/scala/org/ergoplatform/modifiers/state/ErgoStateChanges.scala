package org.ergoplatform.modifiers.state

import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey


abstract class StateChangeOperation {


  override def equals(that: scala.Any): Boolean = (this, that) match {
    case (Removal(i1), Removal(i2)) => i1 sameElements i2
    case (Insertion(b1), Insertion(b2)) => b1 == b2
    case _ => false
  }
}

case class Removal(boxId: ADKey) extends StateChangeOperation {
  override def toString: String = s"Removal(id: ${Algos.encode(boxId)})"
}

case class Insertion(box: ErgoBox) extends StateChangeOperation {
  override def toString: String = s"Insertion(id: ${Algos.encode(box.id)})"
}

case class StateChanges(operations: Seq[StateChangeOperation]) {
  lazy val toAppend: Seq[Insertion] = operations.filter { op =>
    op match {
      case _: Insertion => true
      case _ => false
    }
  }.asInstanceOf[Seq[Insertion]]

  lazy val toRemove: Seq[Removal] = operations.filter { op =>
    op match {
      case _: Removal => true
      case _ => false
    }
  }.asInstanceOf[Seq[Removal]]
}
