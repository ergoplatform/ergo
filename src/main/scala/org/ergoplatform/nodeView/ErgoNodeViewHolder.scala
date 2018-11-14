package org.ergoplatform.nodeView

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoApp
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.UtxoSnapshot
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import scorex.core._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest

import scala.util.Try

abstract class ErgoNodeViewHolder[State <: ErgoState[State]](settings: ErgoSettings,
                                                             timeProvider: NetworkTimeProvider)
  extends NodeViewHolder[ErgoTransaction, ErgoPersistentModifier] {

  private implicit lazy val actorSystem: ActorSystem = context.system

  override val scorexSettings: ScorexSettings = settings.scorexSettings

  override type MS = State
  override type SI = ErgoSyncInfo
  override type HIS = ErgoHistory
  override type VL = ErgoWallet
  override type MP = ErgoMemPool

  override protected lazy val modifiersCache =
    new ErgoModifiersCache(settings.scorexSettings.network.maxModifiersCacheSize)

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100) // this actor shouldn't be restarted at all so kill the whole app if that happened
  }

  override def postStop(): Unit = {
    log.warn("Stopping ErgoNodeViewHolder")
    history().closeStorage()
    minimalState().closeStorage()
  }

  override def receive: Receive =
    processRemoteModifiers orElse
      processLocallyGeneratedModifiers orElse
      processNewTransactions orElse
      getCurrentInfo orElse
      getNodeViewChanges orElse
      successfulModifier orElse
      unknownMessage

  private def successfulModifier: Receive = {
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock)
      if mod.header.height % settings.nodeSettings.snapshotCreationInterval == 0 =>
      createStateSnapshot(mod.header)
  }

  private def unknownMessage: Receive = {
    case other => log.error("Strange input: " + other)
  }

  private def createStateSnapshot(lastHeader: Header): Unit = {
    minimalState().getReader match {
      case r: UtxoStateReader =>
        val (manifest, chunks) = r.takeSnapshot
        val snapshot = UtxoSnapshot(manifest, chunks, Seq(lastHeader))
        history().append(snapshot)
    }
  }

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (ErgoHistory, MS, ErgoWallet, ErgoMemPool) = {

    val state = recreatedState()

    val history = ErgoHistory.readOrGenerate(settings, timeProvider)

    val wallet = ErgoWallet.readOrGenerate(history.getReader.asInstanceOf[ErgoHistoryReader], settings)

    val memPool = ErgoMemPool.empty

    (history, state, wallet, memPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  @SuppressWarnings(Array("AsInstanceOf"))
  override def restoreState: Option[NodeView] = if (ErgoHistory.historyDir(settings).listFiles().isEmpty) {
    None
  } else {
    val history = ErgoHistory.readOrGenerate(settings, timeProvider)
    val wallet = ErgoWallet.readOrGenerate(history.getReader.asInstanceOf[ErgoHistoryReader], settings)
    val memPool = ErgoMemPool.empty
    val constants = StateConstants(Some(self), settings)
    val state = restoreConsistentState(ErgoState.readOrGenerate(settings, constants).asInstanceOf[MS], history)
    Some((history, state, wallet, memPool))
  }

  @SuppressWarnings(Array("AsInstanceOf"))
  private def recreatedState(version: Option[VersionTag] = None, digest: Option[ADDigest] = None): State = {
    val dir = ErgoState.stateDir(settings)
    dir.mkdirs()
    for (file <- dir.listFiles) file.delete

    {
      (version, digest, settings.nodeSettings.stateType) match {
        case (Some(_), Some(_), StateType.Digest) =>
          DigestState.create(version, digest, dir, settings)
        case _ =>
          ErgoState.readOrGenerate(settings, StateConstants(Some(self), settings))
      }
    }.asInstanceOf[State]
      .ensuring(t => java.util.Arrays.equals(t.rootHash, digest.getOrElse(settings.chainSettings.monetary.afterGenesisStateDigest)),
        "State root is incorrect")
  }

  @SuppressWarnings(Array("TryGet"))
  private def restoreConsistentState(stateIn: State, history: ErgoHistory): State = Try {
    (stateIn.version, history.bestFullBlockOpt, stateIn) match {
      case (ErgoState.genesisStateVersion, None, _) =>
        log.info("State and history are both empty on startup")
        stateIn
      case (stateId, Some(block), _) if stateId == block.id =>
        log.info(s"State and history have the same version ${encoder.encode(stateId)}, no recovery needed.")
        stateIn
      case (_, None, _) =>
        log.info("State and history are inconsistent. History is empty on startup, rollback state to genesis.")
        recreatedState()
      case (_, Some(bestFullBlock), _: DigestState) =>
        // Just update state root hash
        log.info(s"State and history are inconsistent. Going to switch state to version ${bestFullBlock.encodedId}")
        recreatedState(Some(idToVersion(bestFullBlock.id)), Some(bestFullBlock.header.stateRoot))
      case (stateId, Some(historyBestBlock), state) =>
        val stateBestHeaderOpt = history.typedModifierById[Header](versionToId(stateId))
        val (rollbackId, newChain) = history.chainToHeader(stateBestHeaderOpt, historyBestBlock.header)
        log.info(s"State and history are inconsistent. Going to rollback to ${rollbackId.map(Algos.encode)} and " +
          s"apply ${newChain.length} modifiers")
        val startState = rollbackId.map(id => state.rollbackTo(idToVersion(id)).get)
          .getOrElse(recreatedState())
        val toApply = newChain.headers.map { h =>
          history.getFullBlock(h) match {
            case Some(fb) => fb
            case None => throw new Exception(s"Failed to get full block for header $h")
          }
        }
        toApply.foldLeft(startState) { (s, m) =>
          s.applyModifier(m).get
        }
    }
  }.recoverWith { case e =>
    log.error("Failed to recover state, try to resync from genesis manually", e)
    ErgoApp.forceStopApplication(500)
  }.get

}

private[nodeView] class DigestNodeViewHolder(settings: ErgoSettings,
                                             timeProvider: NetworkTimeProvider)
  extends ErgoNodeViewHolder[DigestState](settings, timeProvider)

private[nodeView] class UtxoNodeViewHolder(settings: ErgoSettings,
                                           timeProvider: NetworkTimeProvider)
  extends ErgoNodeViewHolder[UtxoState](settings, timeProvider)


/** This class guarantees to its inheritors the creation of correct instance of `ErgoNodeViewHolder`
  * for the given instance of `StateType`
  */
sealed abstract class ErgoNodeViewProps[ST <: StateType, S <: ErgoState[S], N <: ErgoNodeViewHolder[S]]
(implicit ev: StateType.Evidence[ST, S]) {
  def apply(settings: ErgoSettings, timeProvider: NetworkTimeProvider, digestType: ST): Props
}

object DigestNodeViewProps extends ErgoNodeViewProps[StateType.DigestType, DigestState, DigestNodeViewHolder] {
  def apply(settings: ErgoSettings,
            timeProvider: NetworkTimeProvider,
            digestType: StateType.DigestType): Props =
    Props.create(classOf[DigestNodeViewHolder], settings, timeProvider)
}

object UtxoNodeViewProps extends ErgoNodeViewProps[StateType.UtxoType, UtxoState, UtxoNodeViewHolder] {
  def apply(settings: ErgoSettings,
            timeProvider: NetworkTimeProvider,
            digestType: StateType.UtxoType): Props =
    Props.create(classOf[UtxoNodeViewHolder], settings, timeProvider)
}

object ErgoNodeViewRef {

  def props(settings: ErgoSettings,
            timeProvider: NetworkTimeProvider): Props =
    settings.nodeSettings.stateType match {
      case digestType@StateType.Digest => DigestNodeViewProps(settings, timeProvider, digestType)
      case utxoType@StateType.Utxo => UtxoNodeViewProps(settings, timeProvider, utxoType)
    }

  def apply(settings: ErgoSettings,
            timeProvider: NetworkTimeProvider)(implicit system: ActorSystem): ActorRef =
    system.actorOf(props(settings, timeProvider))
}
