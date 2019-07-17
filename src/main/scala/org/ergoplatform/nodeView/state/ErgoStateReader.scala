package org.ergoplatform.nodeView.state

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.{Algos, VotingSettings}
import scorex.core.transaction.state.StateReader
import scorex.crypto.authds.ADDigest
import scorex.util.ScorexLogging

trait ErgoStateReader extends StateReader with ScorexLogging {

  def rootHash: ADDigest
  val store: Store
  val constants: StateConstants

  private lazy val chainSettings = constants.settings.chainSettings

  protected lazy val votingSettings: VotingSettings = chainSettings.voting

  def stateContext: ErgoStateContext = ErgoStateReader.storageStateContext(store, constants)

  def genesisBoxes: Seq[ErgoBox] = ErgoState.genesisBoxes(chainSettings)
}

object ErgoStateReader {

  val ContextKey = Algos.hash("current state context")

  def storageStateContext(store: Store, constants: StateConstants): ErgoStateContext = {
    store.get(ByteArrayWrapper(ErgoStateReader.ContextKey))
      .flatMap(b => ErgoStateContextSerializer(constants.votingSettings).parseBytesTry(b.data).toOption)
      .getOrElse(ErgoStateContext.empty(constants))
  }

}
