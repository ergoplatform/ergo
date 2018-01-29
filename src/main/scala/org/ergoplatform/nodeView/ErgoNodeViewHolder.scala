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
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core._
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.core.utils.NetworkTimeProvider

import scala.util.Try


abstract class ErgoNodeViewHolder[StateType <: ErgoState[StateType]](settings: ErgoSettings,
                                                                     timeProvider: NetworkTimeProvider)
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

    val state = stateRollbackedToGenesis

    //todo: ensure that history is in certain mode
    val history = ErgoHistory.readOrGenerate(settings, timeProvider)

    val wallet = ErgoWallet.readOrGenerate(settings)

    val memPool = ErgoMemPool.empty

    (history, state, wallet, memPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState: Option[NodeView] = if (ErgoHistory.historyDir(settings).listFiles().isEmpty) {
    None
  } else {
    val history = ErgoHistory.readOrGenerate(settings, timeProvider)
    val wallet = ErgoWallet.readOrGenerate(settings)
    val memPool = ErgoMemPool.empty
    val state = restoreConsistentState(ErgoState.readOrGenerate(settings, Some(self)).asInstanceOf[MS], history)
    Some((history, state, wallet, memPool))
  }

  private def stateRollbackedToGenesis: StateType = {
    val dir = ErgoState.stateDir(settings)
    dir.mkdirs()
    for (file <- dir.listFiles) file.delete

    ErgoState.readOrGenerate(settings, Some(self)).asInstanceOf[MS]
      .ensuring(_.rootHash sameElements ErgoState.afterGenesisStateDigest, "State root is incorrect")
  }

  private def restoreConsistentState(stateIn: StateType, history: ErgoHistory): StateType = Try {
    (stateIn.version, history.bestFullBlockOpt, stateIn) match {
      case (stateId, None, _) if stateId sameElements ErgoState.genesisStateVersion =>
        log.debug("State and history are both empty on startup")
        stateIn
      case (stateId, Some(block), _) if stateId sameElements block.id =>
        log.debug(s"State and history have the same version ${Algos.encode(stateId)}, no recovery needed.")
        stateIn
      case (_, None, state) =>
        stateRollbackedToGenesis
      case (_, Some(bestFullBlock), state: DigestState) =>
        // Just update state root hash
        ???
      case (stateId, Some(historyBestBlock), state: StateType) =>
        val stateBestHeaderOpt = history.typedModifierById[Header](ModifierId @@ stateId)
        val (rollbackId, newChain) = history.chainToHeader(stateBestHeaderOpt, historyBestBlock.header)
        val startState = rollbackId.map(id => state.rollbackTo(VersionTag @@ id).get)
          .getOrElse(stateRollbackedToGenesis)
        val toApply = newChain.headers.map(h => history.getFullBlock(h).get)
        toApply.foldLeft(startState) { (s, m) =>
          s.applyModifier(m).get
        }
    }
  }.recoverWith { case e =>
    log.error("Failed to recover state, try to resync from genesis manually", e)
    ErgoApp.forceStopApplication(500)
  }.get

}

private[nodeView] class DigestErgoNodeViewHolder(settings: ErgoSettings, timeProvider: NetworkTimeProvider)
  extends ErgoNodeViewHolder[DigestState](settings, timeProvider)

private[nodeView] class UtxoErgoNodeViewHolder(settings: ErgoSettings, timeProvider: NetworkTimeProvider)
  extends ErgoNodeViewHolder[UtxoState](settings, timeProvider)

object ErgoNodeViewHolder {
  def createActor(system: ActorSystem, settings: ErgoSettings, timeProvider: NetworkTimeProvider): ActorRef = {
    if (settings.nodeSettings.ADState) system.actorOf(Props.create(classOf[DigestErgoNodeViewHolder], settings, timeProvider))
    else system.actorOf(Props.create(classOf[UtxoErgoNodeViewHolder], settings, timeProvider))
  }
}