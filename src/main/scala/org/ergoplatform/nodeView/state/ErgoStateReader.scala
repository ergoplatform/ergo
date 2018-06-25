package org.ergoplatform.nodeView.state

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.settings.Algos
import scorex.core.transaction.state.StateReader
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest

trait ErgoStateReader extends StateReader  with ScorexLogging {

  def rootHash: ADDigest
  val store: Store

  def stateContext: ErgoStateContext = store.get(ByteArrayWrapper(ErgoStateReader.ContextKey))
    .flatMap(b => ErgoStateContextSerializer.parseBytes(b.data).toOption)
    .getOrElse {
      log.warn("Unable to parse state context, situation is only valid on empty state")
      ErgoStateContext(0, rootHash)
    }

}

object ErgoStateReader {
  val ContextKey = Algos.hash("current state context")
}