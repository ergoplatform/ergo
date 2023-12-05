package org.ergoplatform.nodeView.state

import org.ergoplatform.nodeView.state.StateType.{Digest, Utxo}

object StateTypeErgo {

  val values: Seq[StateType] = Seq(Utxo, Digest)

}
