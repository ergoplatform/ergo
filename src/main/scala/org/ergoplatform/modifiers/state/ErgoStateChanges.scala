package org.ergoplatform.modifiers.state

import org.ergoplatform.ErgoBox
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58


abstract class StateChangeOperation

case class Removal(boxId: ADKey) extends StateChangeOperation{
  override def toString: String = s"Removal(id: ${Base58.encode(boxId)})"
}

case class Insertion(box: ErgoBox) extends StateChangeOperation

case class StateChanges(operations: Seq[StateChangeOperation]){
  lazy val toAppend: Seq[Insertion] = operations.filter {op =>
    op match {
      case _: Insertion => true
      case _ => false
    }
  }.asInstanceOf[Seq[Insertion]]

  lazy val toRemove: Seq[Removal] = operations.filter {op =>
    op match {
      case _: Removal => true
      case _ => false
    }
  }.asInstanceOf[Seq[Removal]]
}
