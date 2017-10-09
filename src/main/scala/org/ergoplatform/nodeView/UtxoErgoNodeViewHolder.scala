package org.ergoplatform.nodeView

import java.io.File

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings


class UtxoErgoNodeViewHolder(settings: ErgoSettings) extends ErgoNodeViewHolder[UtxoState](settings) {

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool) = {
    val dir = new File(settings.directory)
    dir.mkdirs()

    val utxoState = ErgoState.generateGenesisUtxoState(dir)._1

    val history = ErgoHistory.readOrGenerate(settings)

    val wallet = ErgoWallet.readOrGenerate(settings)

    val memPool = ErgoMemPool.empty

    (history, utxoState, wallet, memPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState(): Option[(ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool)] = {
    ErgoState.readOrGenerate(settings).map {
      case us: UtxoState =>
        //todo: ensure that history is in certain mode
        val history = ErgoHistory.readOrGenerate(settings)

        val wallet = ErgoWallet.readOrGenerate(settings)

        val memPool = ErgoMemPool.empty

        (history, us, wallet, memPool)

      case _ => ??? //shouldn't be here
    }
  }
}