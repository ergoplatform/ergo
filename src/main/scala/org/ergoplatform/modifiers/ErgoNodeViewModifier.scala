package org.ergoplatform.modifiers

import scorex.core.{ModifierId, bytesToId}

trait ErgoNodeViewModifier {

  lazy val id: ModifierId = bytesToId(serializedId)

  val size: Option[Int]

  def serializedId: Array[Byte]

}
