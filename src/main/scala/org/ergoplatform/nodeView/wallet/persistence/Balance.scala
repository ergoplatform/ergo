package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedBoxId, encodedBoxId}
import org.ergoplatform.wallet.boxes.TrackedBox
import scorex.util.ModifierId

case class Balance(id: EncodedBoxId,
                   value: Long,
                   assets: Map[ModifierId, Long])

object Balance {
  def apply(tb: TrackedBox): Balance = Balance(encodedBoxId(tb.box.id), tb.box.value, tb.tokens)
}
