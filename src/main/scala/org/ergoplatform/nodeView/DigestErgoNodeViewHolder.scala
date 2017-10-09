package org.ergoplatform.nodeView

import java.io.File

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings


class DigestErgoNodeViewHolder(settings: ErgoSettings) extends ErgoNodeViewHolder[DigestState](settings) {

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (ErgoHistory, DigestState, ErgoWallet, ErgoMemPool) = {
    val dir = new File(settings.directory)
    dir.mkdirs()

    val digestState = ErgoState.generateGenesisDigestState(dir)

    //todo: ensure that history is in certain mode
    val history = ErgoHistory.readOrGenerate(settings)

    val wallet = ErgoWallet.readOrGenerate(settings)

    val memPool = ErgoMemPool.empty

    (history, digestState, wallet, memPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState(): Option[(ErgoHistory, DigestState, ErgoWallet, ErgoMemPool)] = {
    ErgoState.readOrGenerate(settings).map {
      case ds: DigestState =>
        //todo: ensure that history is in certain mode
        val history = ErgoHistory.readOrGenerate(settings)

        val wallet = ErgoWallet.readOrGenerate(settings)

        val memPool = ErgoMemPool.empty

        (history, ds, wallet, memPool)
      case _ => ??? //shouldn't be here
    }
  }
}
