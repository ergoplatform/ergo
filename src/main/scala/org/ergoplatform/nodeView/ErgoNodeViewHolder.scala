package org.ergoplatform.nodeView

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.modifiers.mempool.{AnyoneCanSpendTransaction, AnyoneCanSpendTransactionSerializer}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.core.{ModifierTypeId, NodeViewHolder, NodeViewModifier}


abstract class ErgoNodeViewHolder[StateType <: ErgoState[StateType]](settings: ErgoSettings)
  extends NodeViewHolder[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction, ErgoPersistentModifier] {

  override lazy val networkChunkSize: Int = settings.scorexSettings.network.networkChunkSize

  override type MS = StateType
  override type SI = ErgoSyncInfo
  override type HIS = ErgoHistory
  override type VL = ErgoWallet
  override type MP = ErgoMemPool

  override lazy val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(Header.modifierTypeId -> HeaderSerializer,
      BlockTransactions.modifierTypeId -> BlockTransactionsSerializer,
      ADProofs.modifierTypeId -> ADProofSerializer,
      Transaction.ModifierTypeId -> AnyoneCanSpendTransactionSerializer)

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100) // this actor shouldn't be restarted at all so kill the whole app if that happened
  }

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (ErgoHistory, MS, ErgoWallet, ErgoMemPool) = {
    val dir = ErgoState.stateDir(settings)
    require(dir.mkdirs() || dir.listFiles().isEmpty)

    val state = (
      if (settings.nodeSettings.ADState) ErgoState.generateGenesisDigestState(dir)
      else ErgoState.generateGenesisUtxoState(dir)._1
      ).asInstanceOf[MS]

    //todo: ensure that history is in certain mode
    val history = ErgoHistory.readOrGenerate(settings)

    val wallet = ErgoWallet.readOrGenerate(settings)

    val memPool = ErgoMemPool.empty

    (history, state, wallet, memPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState: Option[NodeView] = {
    ErgoState.readOrGenerate(settings).map { state =>
      //todo: ensure that history is in certain mode
      val history = ErgoHistory.readOrGenerate(settings)
      val wallet = ErgoWallet.readOrGenerate(settings)
      val memPool = ErgoMemPool.empty
      (history, state.asInstanceOf[MS], wallet, memPool)
    }
  }

}

private[nodeView] class DigestErgoNodeViewHolder(settings: ErgoSettings)
  extends ErgoNodeViewHolder[DigestState](settings)

private[nodeView] class UtxoErgoNodeViewHolder(settings: ErgoSettings)
  extends ErgoNodeViewHolder[UtxoState](settings)

object ErgoNodeViewHolder {
  def createActor(system: ActorSystem, settings: ErgoSettings): ActorRef = {
    if (settings.nodeSettings.ADState) system.actorOf(Props.create(classOf[DigestErgoNodeViewHolder], settings))
    else system.actorOf(Props.create(classOf[UtxoErgoNodeViewHolder], settings))
  }
}