package org.ergoplatform.modifiers

import scorex.core.serialization.BytesSerializable
import scorex.core.{ModifierId, bytesToId}

trait ErgoNodeViewModifier { self: BytesSerializable =>

  lazy val id: ModifierId = bytesToId(serializedId)

  val sizeOpt: Option[Int]

  lazy val size: Int = sizeOpt.getOrElse(bytes.length)

  def serializedId: Array[Byte]

}
