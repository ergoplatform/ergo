package org.ergoplatform.modifiers

import scorex.util.{ModifierId, bytesToId}

trait ErgoNodeViewModifier {

  lazy val id: ModifierId = bytesToId(serializedId)

  val sizeOpt: Option[Int]

  def serializedId: Array[Byte]

}
