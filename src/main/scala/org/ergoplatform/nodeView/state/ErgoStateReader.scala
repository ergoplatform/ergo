package org.ergoplatform.nodeView.state

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.settings.{Algos, LaunchParameters}
import scorex.core.transaction.state.StateReader
import scorex.util.ScorexLogging
import scorex.crypto.authds.ADDigest

trait ErgoStateReader extends StateReader with ScorexLogging {

  def rootHash: ADDigest
  val store: Store
  val constants: StateConstants

  private lazy val VotingEpochLength = constants.settings.chainSettings.votingLength

  protected def votingStarts(height: Int) = (height % VotingEpochLength == 0 && height > 0)

  def stateContext: ErgoStateContext = store.get(ByteArrayWrapper(ErgoStateReader.ContextKey))
    .flatMap(b => ErgoStateContextSerializer.parseBytes(b.data).toOption)
    .getOrElse {
      log.warn("Unable to parse state context, situation is only valid on empty state")
      ErgoStateContext.empty(constants.emission.settings.afterGenesisStateDigest)
    }
}

object ErgoStateReader {
  val ContextKey = Algos.hash("current state context")
}
