package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.{Algos, LaunchParameters, Parameters, VotingSettings}
import scorex.core.{NodeViewComponent, VersionTag}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.db.LDBVersionedStore
import scorex.util.ScorexLogging

trait ErgoStateReader extends NodeViewComponent with ScorexLogging {

  /**
   * Root hash and height of AVL+ tree authenticating UTXO set
   */
  def rootHash: ADDigest

  /**
    * Current version of the state
    * Must be ID of last block applied
    */
  def version: VersionTag

  val store: LDBVersionedStore
  val constants: StateConstants

  private lazy val chainSettings = constants.settings.chainSettings

  protected lazy val votingSettings: VotingSettings = chainSettings.voting

  /**
    * If the state is in its genesis version (before genesis block)
    */
  def isGenesis: Boolean = {
    rootHash.sameElements(constants.settings.chainSettings.genesisStateDigest)
  }

  def stateContext: ErgoStateContext = ErgoStateReader.storageStateContext(store, constants)

  /**
    * @return current network parameters used in transaction and block validation (block cost and size limits etc)
    */
  def parameters: Parameters = stateContext.currentParameters

  def genesisBoxes: Seq[ErgoBox] = ErgoState.genesisBoxes(chainSettings)

  def closeStorage(): Unit = {
    log.warn("Closing state's store.")
    store.close()
  }

}

object ErgoStateReader extends ScorexLogging {

  val ContextKey: Digest32 = Algos.hash("current state context")

  def storageStateContext(store: LDBVersionedStore, constants: StateConstants): ErgoStateContext = {
    store.get(ErgoStateReader.ContextKey)
      .flatMap(b => ErgoStateContextSerializer(constants.settings).parseBytesTry(b).toOption)
      .getOrElse {
        log.warn("Can't read blockchain parameters from database")
        ErgoStateContext.empty(constants, LaunchParameters)
      }
  }

}
