package org.ergoplatform.nodeView

import java.io.File

import org.ergoplatform.modifiers.history.EquihashPowScheme
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.modifiers.mempool.{AnyoneCanSpendTransaction, AnyoneCanSpendTransactionSerializer}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.core.{NodeViewHolder, NodeViewModifier}


abstract class ErgoNodeViewHolder(settings: ErgoSettings)
  extends NodeViewHolder[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction, ErgoPersistentModifier] {

  override lazy val networkChunkSize: Int = settings.scorexSettings.networkChunkSize

  override type SI = ErgoSyncInfo
  override type HIS = ErgoHistory
  override type VL = ErgoWallet
  override type MP = ErgoMemPool

  //todo: complete this
  override lazy val modifierCompanions: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
  Map(???,
    Transaction.ModifierTypeId -> AnyoneCanSpendTransactionSerializer)

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100) // this actor shouldn't be restarted at all so kill the whole app if that happened
  }
}

class UtxoErgoNodeViewHolder(settings: ErgoSettings) extends ErgoNodeViewHolder(settings) {
  override type MS = UtxoState

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool) = ???

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState(): Option[(ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool)] = ???
}


class DigestErgoNodeViewHolder(settings: ErgoSettings) extends ErgoNodeViewHolder(settings) {
  override type MS = DigestState

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (ErgoHistory, DigestState, ErgoWallet, ErgoMemPool) = {
    val dir = new File(settings.directory)
    dir.mkdirs()

    val digestState = ErgoState.generateGenesisDigestState(dir)

//    val pow = new EquihashPowScheme(n = 96, k = 5)
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
    //val pow = new EquihashPowScheme(n = 96, k = 5)

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