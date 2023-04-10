package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.ErgoBox.{BoxId, TokenId}
import org.ergoplatform.wallet.boxes.TrackedBox

case class Balance(id: BoxId,
                   value: Long,
                   assets: Map[TokenId, Long])

object Balance {
  def apply(tb: TrackedBox): Balance = Balance(tb.box.id, tb.box.value,
    tb.box.additionalTokens.toArray.toMap)
}
