package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.{Algos, VotingSettings}
import scorex.core.transaction.state.StateReader
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.db.LDBVersionedStore
import scorex.util.ScorexLogging

trait ErgoStateReader extends StateReader with ScorexLogging {

  def rootHash: ADDigest
  val store: LDBVersionedStore
  val constants: StateConstants

  private lazy val chainSettings = constants.settings.chainSettings

  protected lazy val votingSettings: VotingSettings = chainSettings.voting

  def stateContext: ErgoStateContext = ErgoStateReader.storageStateContext(store, constants)

  def genesisBoxes: Seq[ErgoBox] = ErgoState.genesisBoxes(chainSettings)
}

object ErgoStateReader {

  val ContextKey: Digest32 = Algos.hash("current state context")

  def storageStateContext(store: LDBVersionedStore, constants: StateConstants): ErgoStateContext = {
    store.get(ErgoStateReader.ContextKey)
      .flatMap(b => ErgoStateContextSerializer(constants.votingSettings).parseBytesTry(b).toOption)
      .getOrElse(ErgoStateContext.empty(constants))
  }

}
