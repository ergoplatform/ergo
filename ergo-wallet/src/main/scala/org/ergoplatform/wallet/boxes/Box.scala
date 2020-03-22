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

  private def assets(box: ErgoBox): Map[ModifierId, Long] =
    box.additionalTokens.toArray.map {
      case (id, amt) => bytesToId(id) -> amt
    }.toMap

  // TODO: remove
  def apply(ergoBox: ErgoBox): GenericBox =
    GenericBox(ergoBox.value, assets(ergoBox))

  def apply(value: Long): GenericBox = GenericBox(value, Map())
}
