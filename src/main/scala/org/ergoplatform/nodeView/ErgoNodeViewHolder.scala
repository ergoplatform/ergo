package org.ergoplatform.nodeView

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoApp
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

import scala.util.{Failure, Success}


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
    val dir = ErgoState.stateDir(settings).ensuring(d => d.mkdirs() || d.listFiles().isEmpty)

    val state = (
      if (settings.nodeSettings.ADState) ErgoState.generateGenesisDigestState(dir, settings.nodeSettings)
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
    ErgoState.readOrGenerate(settings).map { stateIn =>
      //todo: ensure that history is in certain mode
      val history = ErgoHistory.readOrGenerate(settings)
      val wallet = ErgoWallet.readOrGenerate(settings)
      val memPool = ErgoMemPool.empty
      val state = restoreConsistentState(stateIn.asInstanceOf[MS], history)
      (history, state, wallet, memPool)
    }
  }

  private def restoreConsistentState(state: StateType, history: ErgoHistory) = {
    //TODO do we need more than 1 block here?
    history.bestFullBlockOpt.map { fb =>
      if (!(state.version sameElements fb.id)) {
        state.applyModifier(fb) match {
          case Success(s) =>
            log.info(s"State and History are inconsistent on startup. Applied missed modifier ${fb.encodedId}")
            s
          case Failure(e) =>
            //TODO catch errors here
            log.error(s"Failed to apply missed modifier ${fb.encodedId}. Try to resync from genesis", e)
            ErgoApp.forceStopApplication(500)
            state
        }
      } else {
        state
      }
    }.getOrElse(state)
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