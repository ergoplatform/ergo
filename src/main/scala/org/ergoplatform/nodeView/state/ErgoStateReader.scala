package org.ergoplatform.nodeView.state

import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.transaction.state.StateReader
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest

trait ErgoStateReader extends StateReader  with ScorexLogging {

  def rootHash: ADDigest
  val store: Store

  lazy val stateContext: ErgoStateContext = store.get(ByteArrayWrapper(UtxoState.ContextKey))
    .flatMap(b => ErgoStateContextSerializer.parseBytes(b.data).toOption)
    .getOrElse {
      log.warn("Unable to parse state context, situation is only valid on empty state")
      ErgoStateContext(Seq(), rootHash)
    }

}
