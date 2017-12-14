package org.ergoplatform.nodeView.state

import scorex.core.transaction.state.StateReader
import scorex.crypto.authds.ADDigest

trait ErgoStateReader extends StateReader {

  def rootHash: ADDigest

}