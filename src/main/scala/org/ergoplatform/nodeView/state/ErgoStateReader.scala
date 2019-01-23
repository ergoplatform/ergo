package org.ergoplatform.nodeView.state

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.settings.{Algos, VotingSettings}
import scorex.core.transaction.state.StateReader
import scorex.crypto.authds.ADDigest
import scorex.util.ScorexLogging

trait ErgoStateReader extends StateReader with ScorexLogging {

  def rootHash: ADDigest
  val store: Store
  val constants: StateConstants

  protected lazy val votingSettings: VotingSettings = constants.settings.chainSettings.voting

  def stateContext: ErgoStateContext = store.get(ByteArrayWrapper(ErgoStateReader.ContextKey))
    .flatMap(b => ErgoStateContextSerializer(votingSettings).parseBytesTry(b.data).toOption)
    .getOrElse {
      log.warn("Unable to parse state context, situation is only valid on empty state")
      ErgoStateContext.empty(constants)
    }
}

object ErgoStateReader {
  val ContextKey = Algos.hash("current state context")
}
