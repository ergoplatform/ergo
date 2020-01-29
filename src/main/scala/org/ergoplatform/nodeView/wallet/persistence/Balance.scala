package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.wallet.boxes.TrackedBox

case class Balance(id: EncodedBoxId,
                   value: Long,
                   assets: Map[EncodedTokenId, Long])

object Balance {
  def apply(tb: TrackedBox): Balance = Balance(encodedBoxId(tb.box.id), tb.box.value,
    tb.box.additionalTokens.toArray.map(x => encodedTokenId(x._1) -> x._2).toMap)
}
