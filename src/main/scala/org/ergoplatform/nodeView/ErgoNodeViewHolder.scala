package org.ergoplatform.nodeView

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoApp
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.UtxoSnapshot
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallyFailedModification, SemanticallySuccessfulModifier}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest

import scala.util.{Failure, Success, Try}

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

  /**
    * Tries to apply state to nodeView if possible.
    */
  override protected def applyState(history: ErgoHistory,
                                    stateToApply: State,
                                    suffixTrimmed: IndexedSeq[ErgoPersistentModifier],
                                    progressInfo: ProgressInfo[ErgoPersistentModifier]): UpdateInformation = {
    val updateInfoSample = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
    progressInfo.toApply.foldLeft(updateInfoSample) { case (updateInfo, modToApply) =>
      updateInfo.failedMod.fold {
        updateInfo.state.applyModifier(modToApply) match {
          case Success(stateAfterApply) =>
            val newHistory = history.reportModifierIsValid(modToApply)
            context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
            modToApply match {
              case block: ErgoFullBlock if block.header.height % settings.nodeSettings.snapshotCreationInterval == 0 &&
                settings.nodeSettings.keepLastSnapshots != 0 =>
                createStateSnapshot(block.header, stateAfterApply)
              case _ => // do nothing.
            }
            UpdateInformation(newHistory, stateAfterApply, None, None, updateInfo.suffix :+ modToApply)
          case Failure(e) =>
            val (newHistory, newProgressInfo) = history.reportModifierIsInvalid(modToApply, progressInfo)
            context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
            UpdateInformation(newHistory, updateInfo.state, Some(modToApply), Some(newProgressInfo), updateInfo.suffix)
        }
      }(_ => updateInfo)
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

  private def createStateSnapshot(lastHeader: Header, state: State): Unit = {
    state match {
      case utxoReader: UtxoStateReader =>
        log.info(s"Creating state snapshot at height ${lastHeader.height} after header ${lastHeader.encodedId}")
        val (manifest, chunks) = utxoReader.takeSnapshot
        val snapshot = UtxoSnapshot(manifest, chunks, Seq(lastHeader))
        self ! LocallyGeneratedModifier(snapshot)
      case _ =>
        log.warn("Attempting to create state snapshot in unsupported state mode")
    }
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
  } match {
    case Failure(e) =>
      log.error("Failed to recover state, try to resync from genesis manually", e)
      ErgoApp.forceStopApplication(500)
    case Success(state) =>
      state
  }

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
