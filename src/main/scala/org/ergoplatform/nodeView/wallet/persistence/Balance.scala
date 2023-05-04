package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.wallet.boxes.TrackedBox

case class Balance(id: BoxId,
                   value: Long,
                   assets: Map[List[Byte], Long])

object Balance {
  def apply(tb: TrackedBox): Balance = Balance(tb.box.id, tb.box.value,
    tb.box.additionalTokens.toArray.map(x => (x._1.toList, x._2)).toMap)
}
