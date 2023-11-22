package org.ergoplatform.nodeView.state

import org.ergoplatform.{ErgoBox, NodeViewComponent}
import org.ergoplatform.nodeView.history.ErgoHistoryConstants.Height
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings, LaunchParameters, Parameters}
import org.ergoplatform.core.VersionTag
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.db.LDBVersionedStore
import scorex.util.ScorexLogging

import scala.util.{Failure, Success, Try}

/**
  * State-related data and functions related to any state implementation ("utxo" or "digest") which are
  * not modifying the state (so only reading it)
  */
trait ErgoStateReader extends NodeViewComponent with ScorexLogging {

  /**
   * Root hash and height of AVL+ tree authenticating UTXO set
   */
  def rootDigest: ADDigest

  /**
    * Current version of the state
    * Must be ID of last block applied
    */
  def version: VersionTag

  val store: LDBVersionedStore

  protected def ergoSettings: ErgoSettings

  /**
    * If the state is in its genesis version (before genesis block)
    */
  def isGenesis: Boolean = {
    rootDigest.sameElements(ergoSettings.chainSettings.genesisStateDigest)
  }

  /**
    * Blockchain-derived context used in scripts validation. It changes from block to block.
    */
  def stateContext: ErgoStateContext = ErgoStateReader.storageStateContext(store, ergoSettings)

  /**
    * @return current network parameters used in transaction and block validation (block cost and size limits etc)
    */
  def parameters: Parameters = stateContext.currentParameters

  /**
    * Genesis state boxes, see `ErgoState.genesisBoxes` for details
    */
  def genesisBoxes: Seq[ErgoBox] = ErgoState.genesisBoxes(ergoSettings.chainSettings)

}

object ErgoStateReader extends ScorexLogging {

  val ContextKey: Digest32 = Algos.hash("current state context")

  /**
    * Read blockchain-related state context from `store` database
    */
  def storageStateContext(store: LDBVersionedStore, settings: ErgoSettings): ErgoStateContext = {
    store.get(ErgoStateReader.ContextKey)
      .flatMap(b => ErgoStateContextSerializer(settings.chainSettings).parseBytesTry(b).toOption)
      .getOrElse {
        log.warn("Can't read blockchain parameters from database")
        ErgoStateContext.empty(settings.chainSettings, LaunchParameters)
      }
  }

  /**
    * Method to reconstruct state context (used in scripts execution) corresponding to last block of a voting epoch,
    * except of voting-defined blockchain parameters. Basically, this method is setting proper last headers.
    * Then the first block of a new epoch will set the parameters.
    * @param historyReader - history reader to get heights from
    * @param height - height for which state context will be reconstructed
    * @param settings - chain and node settings
    */
  def reconstructStateContextBeforeEpoch(historyReader: ErgoHistoryReader,
                                         height: Height,
                                         settings: ErgoSettings): Try[ErgoStateContext] = {
    val epochLength = settings.chainSettings.voting.votingLength
    if (height % epochLength != epochLength - 1) {
      Failure(new Exception(s"Wrong height provided in reconstructStateContextBeforeEpoch, height $height, epoch length $epochLength"))
    } else {
      val lastHeaders = height.until(height - Constants.LastHeadersInContext, -1).flatMap { h =>
        historyReader.bestHeaderAtHeight(h)
      }
      if (lastHeaders.size != Constants.LastHeadersInContext) {
        Failure(new Exception(s"Only ${lastHeaders.size} headers found in reconstructStateContextBeforeEpoch"))
      } else {
        val empty = ErgoStateContext.empty(settings.chainSettings, LaunchParameters)
        val esc = new ErgoStateContext( lastHeaders,
                                        None,
                                        empty.genesisStateDigest,
                                        empty.currentParameters,
                                        empty.validationSettings,
                                        empty.votingData)(settings.chainSettings)
        Success(esc)
      }
    }
  }

}
