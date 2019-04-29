package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.ErgoBox.BoxId
import scorex.core.serialization.ScorexSerializer
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}

final case class RegistryIndex(balance: Long, tokensBalance: Seq[(ModifierId, Long)], uncertainBoxes: Seq[BoxId])

object RegistryIndexSerializer extends ScorexSerializer[RegistryIndex] {

  override def serialize(obj: RegistryIndex, w: Writer): Unit = ???

  override def parse(r: Reader): RegistryIndex = ???

}
