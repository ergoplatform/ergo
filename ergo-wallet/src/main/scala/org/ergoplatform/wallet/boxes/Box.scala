package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBox
import scorex.util.{bytesToId, idToBytes, ModifierId}

trait Box {

  def value: Long

  def assets: Map[ModifierId, Long]

}

final case class GenericBox(
  val value: Long,
  val assets: Map[ModifierId, Long]
) extends Box

object GenericBox {

  def apply(value: Long): GenericBox = GenericBox(value, Map())
}
