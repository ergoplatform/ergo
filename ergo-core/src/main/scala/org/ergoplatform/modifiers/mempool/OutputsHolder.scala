package org.ergoplatform.modifiers.mempool

import org.ergoplatform.ErgoBox

trait OutputsHolder {
  def outputs: IndexedSeq[ErgoBox]
}
