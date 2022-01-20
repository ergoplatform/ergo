package org.ergoplatform.nodeView

import akka.actor.SupervisorStrategy.Escalate
import java.io.File

import akka.actor.{Actor, ActorRef, ActorSystem, OneForOneStrategy, Props}
import org.ergoplatform.ErgoApp
import org.ergoplatform.ErgoApp.CriticalSystemException
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages._
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages._
import org.ergoplatform.nodeView.ErgoNodeViewHolder.{CurrentView, DownloadRequest}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.BlockAppliedTransactions
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.mempool.ErgoMemPool.ProcessingOutcome
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform.wallet.utils.FileUtils
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings, Parameters}
import scorex.core._
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages._
import org.ergoplatform.nodeView.ErgoNodeViewHolder.{BlockAppliedTransactions, CurrentView, DownloadRequest}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.settings.ScorexSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding}
import scorex.util.ScorexLogging
import spire.syntax.all.cfor

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * Composite local view of the node
  *
  * Contains instances for History, ErgoState, Vault, MemoryPool.
  * The instances are read-only for external world.
  * Updates of the composite view instances are to be performed atomically.
  *
  */
abstract class ErgoNodeViewHolder[State <: ErgoState[State]](settings: ErgoSettings,
                                                             timeProvider: NetworkTimeProvider,
                                                             parameters: Parameters)
  extends Actor with ScorexLogging with ScorexEncoding with FileUtils {

  private implicit lazy val actorSystem: ActorSystem = context.system

  type NodeView = (ErgoHistory, State, ErgoWallet, ErgoMemPool)

  case class UpdateInformation(history: ErgoHistory,
                               state: State,
                               failedMod: Option[ErgoPersistentModifier],
                               alternativeProgressInfo: Option[ProgressInfo[ErgoPersistentModifier]],
                               suffix: IndexedSeq[ErgoPersistentModifier])

  val scorexSettings: ScorexSettings = settings.scorexSettings

  /**
    * Cache for modifiers. If modifiers are coming out-of-order, they are to be stored in this cache.
    */
  protected lazy val modifiersCache =
    new ErgoModifiersCache(settings.scorexSettings.network.maxModifiersCacheSize)

  /**
    * The main data structure a node software is taking care about, a node view consists
    * of four elements to be updated atomically: history (log of persistent modifiers),
    * state (result of log's modifiers application to pre-historical(genesis) state,
    * user-specific information stored in vault (it could be e.g. a wallet), and a memory pool.
    */
  private var nodeView: NodeView = restoreState().getOrElse(genesisState)

  /** Tracking last modifier and header & block heights in time, being periodically checked for possible stuck */
  private var chainProgress: Option[ChainProgress] = None

  protected def history(): ErgoHistory = nodeView._1

  protected def minimalState(): State = nodeView._2

  protected def vault(): ErgoWallet = nodeView._3

  protected def memoryPool(): ErgoMemPool = nodeView._4

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy() {
      case e: Throwable =>
        log.error(s"NodeViewHolder failed, killing whole application ...", e)
        Escalate
    }

  override def postStop(): Unit = {
    log.warn("Stopping ErgoNodeViewHolder")
    history().closeStorage()
    minimalState().closeStorage()
  }

  /**
    * Update NodeView with new components and notify subscribers of changed components
    *
    * @param updatedHistory
    * @param updatedState
    * @param updatedVault
    * @param updatedMempool
    */
  protected def updateNodeView(updatedHistory: Option[ErgoHistory] = None,
                               updatedState: Option[State] = None,
                               updatedVault: Option[ErgoWallet] = None,
                               updatedMempool: Option[ErgoMemPool] = None): Unit = {
    val newNodeView = (updatedHistory.getOrElse(history()),
      updatedState.getOrElse(minimalState()),
      updatedVault.getOrElse(vault()),
      updatedMempool.getOrElse(memoryPool()))
    if (updatedHistory.nonEmpty) {
      context.system.eventStream.publish(ChangedHistory(newNodeView._1.getReader))
    }
    if (updatedState.nonEmpty) {
      context.system.eventStream.publish(ChangedState(newNodeView._2.getReader))
    }
    if (updatedVault.nonEmpty) {
      context.system.eventStream.publish(ChangedVault(newNodeView._3.getReader))
    }
    if (updatedMempool.nonEmpty) {
      context.system.eventStream.publish(ChangedMempool(newNodeView._4.getReader))
    }
    nodeView = newNodeView
  }

  protected def extractTransactions(mod: ErgoPersistentModifier): Seq[ErgoTransaction] = mod match {
    case tcm: TransactionsCarryingPersistentNodeViewModifier => tcm.transactions
    case _ => Seq()
  }


  protected def requestDownloads(pi: ProgressInfo[ErgoPersistentModifier]): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      context.system.eventStream.publish(DownloadRequest(tid, id))
    }

  private def trimChainSuffix(suffix: IndexedSeq[ErgoPersistentModifier],
                              rollbackPoint: scorex.util.ModifierId): IndexedSeq[ErgoPersistentModifier] = {
    val idx = suffix.indexWhere(_.id == rollbackPoint)
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }

  /**

  Assume that history knows the following blocktree:

           G
          / \
    *   G
        /     \
    *       G

    where path with G-s is about canonical chain (G means semantically valid modifier), path with * is sidechain (* means
    that semantic validity is unknown). New modifier is coming to the sidechain, it sends rollback to the root +
    application of the sidechain to the state. Assume that state is finding that some modifier in the sidechain is
    incorrect:

           G
          / \
         G   G
        /     \
       B       G
      /
    *

  In this case history should be informed about the bad modifier and it should retarget state

    //todo: improve the comment below

    We assume that we apply modifiers sequentially (on a single modifier coming from the network or generated locally),
    and in case of failed application of some modifier in a progressInfo, rollback point in an alternative should be not
    earlier than a rollback point of an initial progressInfo.
    **/

  @tailrec
  protected final def updateState(history: ErgoHistory,
                                  state: State,
                                  progressInfo: ProgressInfo[ErgoPersistentModifier],
                                  suffixApplied: IndexedSeq[ErgoPersistentModifier]): (ErgoHistory, Try[State], Seq[ErgoPersistentModifier]) = {
    requestDownloads(progressInfo)

    val (stateToApplyTry: Try[State], suffixTrimmed: IndexedSeq[ErgoPersistentModifier]) = if (progressInfo.chainSwitchingNeeded) {
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val branchingPoint = progressInfo.branchPoint.get //todo: .get
      if (state.version != branchingPoint) {
        state.rollbackTo(idToVersion(branchingPoint)) -> trimChainSuffix(suffixApplied, branchingPoint)
      } else Success(state) -> IndexedSeq()
    } else Success(state) -> suffixApplied

    stateToApplyTry match {
      case Success(stateToApply) =>
        applyState(history, stateToApply, suffixTrimmed, progressInfo) match {
          case Success(stateUpdateInfo) =>
            stateUpdateInfo.failedMod match {
              case Some(_) =>
                @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
                val alternativeProgressInfo = stateUpdateInfo.alternativeProgressInfo.get
                updateState(stateUpdateInfo.history, stateUpdateInfo.state, alternativeProgressInfo, stateUpdateInfo.suffix)
              case None =>
                (stateUpdateInfo.history, Success(stateUpdateInfo.state), stateUpdateInfo.suffix)
            }
          case Failure(ex) =>
            (history, Failure(ex), suffixTrimmed)
        }
      case Failure(e) =>
        log.error("Rollback failed: ", e)
        context.system.eventStream.publish(RollbackFailed)
        //todo: what to return here? the situation is totally wrong
        ???
    }
  }

  private def applyState(history: ErgoHistory,
                         stateToApply: State,
                         suffixTrimmed: IndexedSeq[ErgoPersistentModifier],
                         progressInfo: ProgressInfo[ErgoPersistentModifier]): Try[UpdateInformation] = {
    val updateInfoSample = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
    progressInfo.toApply.foldLeft[Try[UpdateInformation]](Success(updateInfoSample)) {
      case (f@Failure(ex), _) =>
        log.error("Reporting modifier failed", ex)
        f
      case (success@Success(updateInfo), modToApply) =>
        if (updateInfo.failedMod.isEmpty) {
          updateInfo.state.applyModifier(modToApply)(lm => pmodModify(lm.pmod, local = true)) match {
            case Success(stateAfterApply) =>
              history.reportModifierIsValid(modToApply).map { newHis =>
                context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
                UpdateInformation(newHis, stateAfterApply, None, None, updateInfo.suffix :+ modToApply)
              }
            case Failure(e) =>
              history.reportModifierIsInvalid(modToApply, progressInfo).map { case (newHis, newProgressInfo) =>
                context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
                UpdateInformation(newHis, updateInfo.state, Some(modToApply), Some(newProgressInfo), updateInfo.suffix)
              }
          }
        } else success
    }
  }

  protected def txModify(tx: ErgoTransaction): Unit = {
    memoryPool().process(tx, minimalState()) match {
      case (newPool, ProcessingOutcome.Accepted) =>
        log.debug(s"Unconfirmed transaction $tx added to the memory pool")
        val newVault = vault().scanOffchain(tx)
        updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
        context.system.eventStream.publish(SuccessfulTransaction(tx))
      case (newPool, ProcessingOutcome.Invalidated(e)) =>
        log.debug(s"Transaction $tx invalidated. Cause: ${e.getMessage}")
        updateNodeView(updatedMempool = Some(newPool))
        context.system.eventStream.publish(FailedTransaction(tx.id, e, immediateFailure = true))
      case (_, ProcessingOutcome.DoubleSpendingLoser(winnerTxs)) => // do nothing
        log.debug(s"Transaction $tx declined, as other transactions $winnerTxs are paying more")
      case (_, ProcessingOutcome.Declined(e)) => // do nothing
        log.debug(s"Transaction $tx declined, reason: ${e.getMessage}")
    }
  }

  /**
    * Process new modifiers from remote.
    * Put all candidates to modifiersCache and then try to apply as much modifiers from cache as possible.
    * Clear cache if it's size exceeds size limit.
    * Publish `ModifiersProcessingResult` message with all just applied and removed from cache modifiers.
    */
  protected def processRemoteModifiers: Receive = {
    case ModifiersFromRemote(mods: Seq[ErgoPersistentModifier]@unchecked) =>
      @tailrec
      def applyFromCacheLoop(applied: Seq[ErgoPersistentModifier]): Seq[ErgoPersistentModifier] = {
        modifiersCache.popCandidate(history()) match {
          case Some(mod) =>
            pmodModify(mod, local = false)
            applyFromCacheLoop(mod +: applied)
          case None =>
            applied
        }
      }

      mods.headOption match {
        case Some(h) if h.isInstanceOf[Header] => // modifiers are always of the same type
          val sorted = mods.sortBy(_.asInstanceOf[Header].height)

          val applied0 = if (sorted.head.asInstanceOf[Header].height == history().headersHeight + 1) {

            // we apply sorted headers while headers sequence is not broken
            val appliedBuffer = mutable.Buffer[Header]()
            var expectedHeight = history().headersHeight + 1
            var linkBroken = false

            cfor(0)(_ < sorted.length, _ + 1) { idx =>
              val header = sorted(idx).asInstanceOf[Header]
              if (!linkBroken && header.height == expectedHeight) {
                pmodModify(header, local = false)
                header +=: appliedBuffer // prepend header, to be consistent with applyFromCacheLoop
                expectedHeight += 1
              } else {
                if (!linkBroken) {
                  linkBroken = true
                }
                // put into cache headers not applied
                modifiersCache.put(header.id, header)
              }
            }
            appliedBuffer
          } else {
            mods.foreach(h => modifiersCache.put(h.id, h))
            Seq.empty
          }

          val applied = applyFromCacheLoop(applied0)

          val cleared = modifiersCache.cleanOverfull()
          context.system.eventStream.publish(ModifiersProcessingResult(applied, cleared))
          log.debug(s"Cache size after: ${modifiersCache.size}")
        case _ =>
          mods.foreach(m => modifiersCache.put(m.id, m))

          log.debug(s"Cache size before: ${modifiersCache.size}")

          val applied = applyFromCacheLoop(Seq())
          val cleared = modifiersCache.cleanOverfull()

          context.system.eventStream.publish(ModifiersProcessingResult(applied, cleared))
          log.debug(s"Cache size after: ${modifiersCache.size}")
      }
  }

  /**
    * Performs mempool update after a block application, transactions
    * from rolled back block are to be returned to the pool, and transactions
    * included in applied block are to be removed.
    */
  protected def updateMemPool(blocksRemoved: Seq[ErgoPersistentModifier],
                                       blocksApplied: Seq[ErgoPersistentModifier],
                                       memPool: ErgoMemPool,
                                       state: State): ErgoMemPool = {
    val rolledBackTxs = blocksRemoved.flatMap(extractTransactions)
    val appliedTxs = blocksApplied.flatMap(extractTransactions)
    context.system.eventStream.publish(BlockAppliedTransactions(appliedTxs.map(_.id)))
    memPool.putWithoutCheck(rolledBackTxs).filter(tx => !appliedTxs.exists(_.id == tx.id))
  }

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  protected def genesisState: (ErgoHistory, State, ErgoWallet, ErgoMemPool) = {

    val state = recreatedState()

    val history = ErgoHistory.readOrGenerate(settings, timeProvider)

    val wallet = ErgoWallet.readOrGenerate(
      history.getReader.asInstanceOf[ErgoHistoryReader],
      settings,
      parameters)

    val memPool = ErgoMemPool.empty(settings)

    (history, state, wallet, memPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  @SuppressWarnings(Array("AsInstanceOf"))
  def restoreState(): Option[NodeView] = if (ErgoHistory.historyDir(settings).listFiles().isEmpty) {
    None
  } else {
    val history = ErgoHistory.readOrGenerate(settings, timeProvider)
    log.info("History database read")
    val memPool = ErgoMemPool.empty(settings)
    val constants = StateConstants(settings)
    restoreConsistentState(ErgoState.readOrGenerate(settings, constants, parameters).asInstanceOf[State], history) match {
      case Success(state) =>
        log.info("State database read, state synchronized")
        val wallet = ErgoWallet.readOrGenerate(
          history.getReader.asInstanceOf[ErgoHistoryReader],
          settings,
          parameters)
        log.info("Wallet database read")
        Some((history, state, wallet, memPool))
      case Failure(ex) =>
        log.error("Failed to recover state, try to resync from genesis manually", ex)
        ErgoApp.shutdownSystem()(context.system)
        None
    }
  }

  //todo: update state in async way?
  /**
    * Remote and local persistent modifiers need to be appended to history, applied to state
    * which also needs to be propagated to mempool and wallet
    * @param pmod Remote or local persistent modifier
    */
  protected def pmodModify(pmod: ErgoPersistentModifier, local: Boolean): Unit =
    if (!history().contains(pmod.id)) {
      context.system.eventStream.publish(StartingPersistentModifierApplication(pmod))

      log.info(s"Apply modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} to nodeViewHolder")

      history().append(pmod) match {
        case Success((historyBeforeStUpdate, progressInfo)) =>
          log.debug(s"Going to apply modifications to the state: $progressInfo")
          context.system.eventStream.publish(SyntacticallySuccessfulModifier(pmod))

          if (progressInfo.toApply.nonEmpty) {
            val (newHistory, newStateTry, blocksApplied) =
              updateState(historyBeforeStUpdate, minimalState(), progressInfo, IndexedSeq())

            newStateTry match {
              case Success(newMinState) =>
                val newMemPool = updateMemPool(progressInfo.toRemove, blocksApplied, memoryPool(), newMinState)

                @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
                val v = vault()
                val newVault = if (progressInfo.chainSwitchingNeeded) {
                  v.rollback(idToVersion(progressInfo.branchPoint.get)) match {
                    case Success(nv) => nv
                    case Failure(e) => log.warn("Wallet rollback failed: ", e); v
                  }
                } else {
                  v
                }

                // we assume that wallet scan may be started if fullblocks-chain is no more
                // than 20 blocks behind headers-chain
                val almostSyncedGap = 20

                val headersHeight = newHistory.headersHeight
                val fullBlockHeight = newHistory.fullBlockHeight
                if((headersHeight - fullBlockHeight) < almostSyncedGap) {
                  blocksApplied.foreach(newVault.scanPersistent)
                }

                log.info(s"Persistent modifier ${pmod.encodedId} applied successfully")
                updateNodeView(Some(newHistory), Some(newMinState), Some(newVault), Some(newMemPool))
                chainProgress =
                  Some(ChainProgress(pmod, headersHeight, fullBlockHeight, System.currentTimeMillis()))
              case Failure(e) =>
                log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to minimal state", e)
                updateNodeView(updatedHistory = Some(newHistory))
                context.system.eventStream.publish(SemanticallyFailedModification(pmod, e))
            }
          } else {
            requestDownloads(progressInfo)
            updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
          }
        case Failure(CriticalSystemException(error)) =>
          log.error(error)
          ErgoApp.shutdownSystem()(context.system)
        case Failure(e) =>
          log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to history", e)
          context.system.eventStream.publish(SyntacticallyFailedModification(pmod, e))
      }
    } else {
      log.warn(s"Trying to apply modifier ${pmod.encodedId} that's already in history")
    }

  @SuppressWarnings(Array("AsInstanceOf"))
  private def recreatedState(): State = {
    val dir = stateDir(settings)
    deleteRecursive(dir)

    val constants = StateConstants(settings)
    ErgoState.readOrGenerate(settings, constants, parameters)
      .asInstanceOf[State]
      .ensuring(
        state => java.util.Arrays.equals(state.rootHash, settings.chainSettings.genesisStateDigest),
        "State root is incorrect"
      )
  }

  private def restoreConsistentState(stateIn: State, history: ErgoHistory): Try[State] = {
    (stateIn.version, history.bestFullBlockOpt, stateIn) match {
      case (ErgoState.genesisStateVersion, None, _) =>
        log.info("State and history are both empty on startup")
        Success(stateIn)
      case (stateId, Some(block), _) if stateId == block.id =>
        log.info(s"State and history have the same version ${encoder.encode(stateId)}, no recovery needed.")
        Success(stateIn)
      case (_, None, _) =>
        log.info("State and history are inconsistent. History is empty on startup, rollback state to genesis.")
        Success(recreatedState())
      case (_, Some(bestFullBlock), _: DigestState) =>
        log.info(s"State and history are inconsistent. Going to switch state to version ${bestFullBlock.encodedId}")
        recoverDigestState(bestFullBlock, history).map(_.asInstanceOf[State])
      case (stateId, Some(historyBestBlock), state) =>
        val stateBestHeaderOpt = history.typedModifierById[Header](versionToId(stateId))
        val (rollbackId, newChain) = history.chainToHeader(stateBestHeaderOpt, historyBestBlock.header)
        log.info(s"State and history are inconsistent. Going to rollback to ${rollbackId.map(Algos.encode)} and " +
          s"apply ${newChain.length} modifiers")
        val initState = rollbackId
          .map(id => state.rollbackTo(idToVersion(id)).get)
          .getOrElse(recreatedState())
        val toApply = newChain.headers.map { h =>
          history.getFullBlock(h)
            .fold(throw new Error(s"Failed to get full block for header $h"))(fb => fb)
        }
        toApply.foldLeft[Try[State]](Success(initState)) { case (acc, m) =>
          log.info(s"Applying modifier during node start-up to restore consistent state: ${m.id}")
          acc.flatMap(_.applyModifier(m)(lm => pmodModify(lm.pmod, local = true)))
        }
    }
  }

  /**
    * Recovers digest state from history.
    */
  private def recoverDigestState(bestFullBlock: ErgoFullBlock, history: ErgoHistory): Try[DigestState] = {
    val constants = StateConstants(settings)
    val votingLength = settings.chainSettings.voting.votingLength
    val bestHeight = bestFullBlock.header.height
    val newEpochHeadersQty = bestHeight % votingLength // how many blocks current epoch lasts
    val headersQtyToAcquire = newEpochHeadersQty + Constants.LastHeadersInContext
    val acquiredChain = history.headerChainBack(headersQtyToAcquire, bestFullBlock.header, _ => false).headers
    val (lastHeaders, chainToApply) = acquiredChain.splitAt(Constants.LastHeadersInContext)
    val firstExtensionOpt = lastHeaders.lastOption // last extension in the prev epoch to recover from
      .flatMap(h => history.typedModifierById[Extension](h.extensionId))

    val recoveredStateTry = firstExtensionOpt
      .fold[Try[ErgoStateContext]](Failure(new Exception("Could not find extension to recover from"))
      )(ext => ErgoStateContext.recover(constants.genesisStateDigest, ext, lastHeaders)(settings))
      .flatMap { ctx =>
        val recoverVersion = idToVersion(lastHeaders.last.id)
        val recoverRoot = bestFullBlock.header.stateRoot
        DigestState.recover(recoverVersion, recoverRoot, ctx, stateDir(settings), constants, parameters)
      }

    recoveredStateTry match {
      case Success(state) =>
        log.info("Recovering state using current epoch")
        chainToApply.foldLeft[Try[DigestState]](Success(state)) { case (acc, m) =>
          acc.flatMap(_.applyModifier(m)(lm => pmodModify(lm.pmod, local = true)))
        }
      case Failure(exception) => // recover using whole headers chain
        log.warn(s"Failed to recover state from current epoch, using whole chain: ${exception.getMessage}")
        val wholeChain = history.headerChainBack(Int.MaxValue, bestFullBlock.header, _.isGenesis).headers
        val genesisState = DigestState.create(None, None, stateDir(settings), constants, parameters)
        wholeChain.foldLeft[Try[DigestState]](Success(genesisState)) { case (acc, m) =>
          acc.flatMap(_.applyModifier(m)(lm => pmodModify(lm.pmod, local = true)))
        }
    }
  }

  private def stateDir(settings: ErgoSettings): File = {
    val dir = ErgoState.stateDir(settings)
    dir.mkdirs()
    dir
  }

  protected def transactionsProcessing: Receive = {
    case newTxs: NewTransactions =>
      newTxs.txs.foreach(txModify)
    case EliminateTransactions(ids) =>
      val updatedPool = memoryPool().filter(tx => !ids.contains(tx.id))
      updateNodeView(updatedMempool = Some(updatedPool))
      ids.foreach { id =>
        val e = new Exception("Became invalid")
        context.system.eventStream.publish(FailedTransaction(id, e, immediateFailure = false))
      }
  }

  protected def processLocallyGeneratedModifiers: Receive = {
    case lm: LocallyGeneratedModifier =>
      log.info(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      pmodModify(lm.pmod, local = true)
  }

  protected def getCurrentInfo: Receive = {
    case GetDataFromCurrentView(f) =>
      sender() ! f(CurrentView(history(), minimalState(), vault(), memoryPool()))
  }

  protected def getNodeViewChanges: Receive = {
    case GetNodeViewChanges(history, state, vault, mempool) =>
      if (history) sender() ! ChangedHistory(nodeView._1.getReader)
      if (state) sender() ! ChangedState(nodeView._2.getReader)
      if (vault) sender() ! ChangedVault(nodeView._3.getReader)
      if (mempool) sender() ! ChangedMempool(nodeView._4.getReader)
  }

  protected def handleHealthCheck: Receive = {
    case IsChainHealthy =>
      val healthCheckReply = chainProgress.map { progress =>
        ErgoNodeViewHolder.checkChainIsHealthy(progress, history(), settings)
      }.getOrElse(ChainIsHealthy)
      sender() ! healthCheckReply
  }

  override def receive: Receive =
    processRemoteModifiers orElse
      processLocallyGeneratedModifiers orElse
      transactionsProcessing orElse
      getCurrentInfo orElse
      getNodeViewChanges orElse
      handleHealthCheck orElse {
        case a: Any => log.error("Strange input: " + a)
      }

}


object ErgoNodeViewHolder {

  object ReceivableMessages {
    // Tracking last modifier and header & block heights in time, being periodically checked for possible stuck
    case class ChainProgress(lastMod: ErgoPersistentModifier, headersHeight: Int, blockHeight: Int, lastUpdate: Long)

    // Explicit request of NodeViewChange events of certain types.
    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean, mempool: Boolean)

    case class GetDataFromCurrentView[State, A](f: CurrentView[State] => A)

    // Modifiers received from the remote peer with new elements in it
    case class ModifiersFromRemote(modifiers: Iterable[ErgoPersistentModifier])

    sealed trait NewTransactions{
      val txs: Iterable[ErgoTransaction]
    }

    case class LocallyGeneratedTransaction(tx: ErgoTransaction) extends NewTransactions {
      override val txs: Iterable[ErgoTransaction] = Iterable(tx)
    }

    case class TransactionsFromRemote(override val txs: Iterable[ErgoTransaction]) extends NewTransactions

    case class LocallyGeneratedModifier(pmod: ErgoPersistentModifier)

    case class EliminateTransactions(ids: Seq[scorex.util.ModifierId])

    case object IsChainHealthy
    sealed trait HealthCheckResult
    case object ChainIsHealthy extends HealthCheckResult
    case class ChainIsStuck(reason: String) extends HealthCheckResult
  }

  case class BlockAppliedTransactions(txs: Seq[scorex.util.ModifierId]) extends NodeViewHolderEvent

  case class DownloadRequest(modifierTypeId: ModifierTypeId,
                             modifierId: scorex.util.ModifierId) extends NodeViewHolderEvent

  case class CurrentView[State](history: ErgoHistory, state: State, vault: ErgoWallet, pool: ErgoMemPool)

  /**
    * Checks whether chain got stuck by comparing timestamp of bestFullBlock or last time a modifier was applied to history.
    * @param progress metadata of last chain update
    * @return ChainIsHealthy if chain is healthy and ChainIsStuck(error) with details if it got stuck
    */
  def checkChainIsHealthy(
      progress: ChainProgress,
      history: ErgoHistory,
      settings: ErgoSettings): HealthCheckResult = {
    val ChainProgress(lastMod, headersHeight, blockHeight, lastUpdate) = progress
    val chainUpdateDelay = System.currentTimeMillis() - lastUpdate
    val acceptableChainUpdateDelay = settings.nodeSettings.acceptableChainUpdateDelay
    def chainUpdateDelayed = chainUpdateDelay > acceptableChainUpdateDelay.toMillis
    def blockUpdateDelayed =
      history.bestFullBlockOpt
        .map(b => System.currentTimeMillis() - b.header.timestamp)
        .exists(blockUpdateDelay => blockUpdateDelay > acceptableChainUpdateDelay.toMillis)

    def chainSynced =
      history.bestFullBlockOpt.map(_.id) == history.bestHeaderOpt.map(_.id)

    if (chainUpdateDelayed || blockUpdateDelayed) {
      val bestFullBlockOpt =
        history.bestFullBlockOpt
          .filter(_.id != lastMod.id)
          .fold("")(fb => s"\n best full block: $fb")
      val repairNeeded = ErgoHistory.repairIfNeeded(history)
      ChainIsStuck(s"Chain not modified for $chainUpdateDelay ms, headers-height: $headersHeight, " +
        s"block-height $blockHeight, chain synced: $chainSynced, repair needed: $repairNeeded, " +
        s"last modifier applied: $lastMod $bestFullBlockOpt")
    } else {
      ChainIsHealthy
    }
  }
}

private[nodeView] class DigestNodeViewHolder(settings: ErgoSettings,
                                             timeProvider: NetworkTimeProvider,
                                             parameters: Parameters)
  extends ErgoNodeViewHolder[DigestState](settings, timeProvider, parameters)

private[nodeView] class UtxoNodeViewHolder(settings: ErgoSettings,
                                           timeProvider: NetworkTimeProvider,
                                           parameters: Parameters)
  extends ErgoNodeViewHolder[UtxoState](settings, timeProvider, parameters)



object ErgoNodeViewRef {

  private def digestProps(settings: ErgoSettings,
                          timeProvider: NetworkTimeProvider,
                          parameters: Parameters): Props =
    Props.create(classOf[DigestNodeViewHolder], settings, timeProvider, parameters)

  private def utxoProps(settings: ErgoSettings,
                        timeProvider: NetworkTimeProvider,
                        parameters: Parameters): Props =
    Props.create(classOf[UtxoNodeViewHolder], settings, timeProvider, parameters)

  def props(settings: ErgoSettings,
            timeProvider: NetworkTimeProvider,
            parameters: Parameters): Props =
    settings.nodeSettings.stateType match {
      case StateType.Digest => digestProps(settings, timeProvider, parameters)
      case StateType.Utxo => utxoProps(settings, timeProvider, parameters)
    }

  def apply(settings: ErgoSettings,
            timeProvider: NetworkTimeProvider,
            parameters: Parameters)(implicit system: ActorSystem): ActorRef =
    system.actorOf(props(settings, timeProvider, parameters))
  
}
