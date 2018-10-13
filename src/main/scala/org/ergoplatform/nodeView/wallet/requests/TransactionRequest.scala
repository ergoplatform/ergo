package org.ergoplatform.nodeView.wallet.requests

import org.ergoplatform.ErgoBoxCandidate

trait TransactionRequest {

  def toBoxCandidate: ErgoBoxCandidate
}
