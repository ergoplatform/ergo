package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.wallet.boxes.TrackedBox
import scorex.util.{ModifierId, bytesToId}

case class Balance(id: BoxId,
                   value: Long,
                   assets: Map[ModifierId, Long])

object Balance {
  def apply(tb: TrackedBox): Balance = Balance(tb.box.id, tb.box.value,
    tb.box.additionalTokens.toArray.map(x => (bytesToId(x._1), x._2)).toMap)
}
