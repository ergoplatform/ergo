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
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, ModifierTypeId, NodeViewHolder, NodeViewModifier}

import scala.util.{Failure, Success}


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
    val dir = ErgoState.stateDir(settings)
    dir.mkdir()
    assert(dir.listFiles().isEmpty, s"Genesis directory $dir should always be empty")

    val state = ErgoState.readOrGenerate(settings, Some(self)).asInstanceOf[MS]
      .ensuring(_.rootHash sameElements ErgoState.afterGenesisStateDigest, "State root is incorrect")

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
  override def restoreState: Option[NodeView] = if (Option(ErgoHistory.historyDir(settings).listFiles()).isEmpty) {
    None
  } else {
    val history = ErgoHistory.readOrGenerate(settings, timeProvider)
    val wallet = ErgoWallet.readOrGenerate(settings)
    val memPool = ErgoMemPool.empty
    val state = restoreConsistentState(ErgoState.readOrGenerate(settings, Some(self)).asInstanceOf[MS], history)
    Some((history, state, wallet, memPool))
  }

  private def restoreConsistentState(state: StateType, history: ErgoHistory): StateType = {
    if (history.bestFullBlockIdOpt.isEmpty) {
      state
    } else {
      val stateBestBlockId = if (state.version sameElements ErgoState.genesisStateVersion) None else Some(state.version)
      val hFrom = stateBestBlockId.flatMap(id => history.typedModifierById[Header](ModifierId @@ id))
      val fbFrom = hFrom.flatMap(h => history.getFullBlock(h))
      history.fullBlocksAfter(fbFrom).map { toApply =>
        if (toApply.nonEmpty) {
          log.info(s"State and History are inconsistent on startup. Going to apply ${toApply.length} modifiers")
        } else {
          assert(stateBestBlockId.get sameElements history.bestFullBlockIdOpt.get,
            "State version should always equal to best full block id.")
          log.info(s"State and History are consistent on startup.")
        }
        toApply.foldLeft(state) { (s, m) =>
          s.applyModifier(m) match {
            case Success(newState) =>
              newState
            case Failure(e) =>
              throw new Error(s"Failed to apply missed modifier ${m.encodedId}")
          }
        }
      }.recoverWith { case e =>
        log.error("Failed to recover state, try to resync from genesis manually", e)
        ErgoApp.forceStopApplication(500)
      }.get
    }
  }
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