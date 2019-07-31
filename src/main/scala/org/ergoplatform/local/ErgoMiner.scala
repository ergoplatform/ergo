package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, PoisonPill, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.google.common.primitives.Longs
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform._
import org.ergoplatform.mining._
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.PoPowAlgos._
import org.ergoplatform.modifiers.history.{Extension, Header, PoPowAlgos}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, ErgoMemPoolReader}
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{ErgoSettings, ErgoValidationSettingsUpdate}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import scorex.core.NodeViewHolder.ReceivableMessages.{EliminateTransactions, GetDataFromCurrentView, LocallyGeneratedModifier}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.utils.NetworkTimeProvider
import scorex.core.validation.ValidationSettings
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging}
import sigmastate.SType.ErgoBoxRType
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.interpreter.{ContextExtension, ProverResult}
import special.collection.Coll
import special.sigma._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class ErgoMiner(ergoSettings: ErgoSettings,
                viewHolderRef: ActorRef,
                readersHolderRef: ActorRef,
                timeProvider: NetworkTimeProvider,
                inSecretKeyOpt: Option[DLogProverInput]) extends Actor with ScorexLogging {

  import ErgoMiner._

  private implicit val timeout: Timeout = 5.seconds

  private val desiredUpdate = ergoSettings.votingTargets.desiredUpdate

  private val votingSettings = ergoSettings.chainSettings.voting
  private val votingEpochLength = votingSettings.votingLength
  private val protocolVersion = ergoSettings.chainSettings.protocolVersion
  private val powScheme = ergoSettings.chainSettings.powScheme
  private val externalMinerMode = ergoSettings.nodeSettings.useExternalMiner
  private val maxTransactionComplexity: Int = ergoSettings.nodeSettings.maxTransactionComplexity

  // shared mutable state
  private var isMining = false
  private var candidateOpt: Option[CandidateBlock] = None
  private val miningThreads: mutable.Buffer[ActorRef] = new ArrayBuffer[ActorRef]()
  // cost of a transaction collecting emission box

  private var secretKeyOpt: Option[DLogProverInput] = inSecretKeyOpt
  private var publicKeyOpt: Option[ProveDlog] = ergoSettings.miningPubKey
    .orElse(inSecretKeyOpt.map(_.publicImage))

  override def preStart(): Unit = {
    // in external miner mode key from wallet is used if `publicKeyOpt` is not set
    if ((publicKeyOpt.isEmpty && externalMinerMode) || (secretKeyOpt.isEmpty && !externalMinerMode)) {
      log.info("Trying to use key from wallet for mining")
      self ! QueryWallet
    }
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def postStop(): Unit = {
    log.warn("Stopping miner's threads.")
    miningThreads.foreach(_ ! PoisonPill)
    miningThreads.clear()
  }

  private def unknownMessage: Receive = {
    case _: scala.runtime.BoxedUnit =>
    // ignore, this message is caused by way of interaction with NVH.
    case m =>
      log.warn(s"Unexpected message $m of class: ${m.getClass}")
  }

  private def keysManagement: Receive = {
    case UpdateSecret(s) =>
      secretKeyOpt = Some(s)
      publicKeyOpt = Some(s.publicImage)
    case ReadMinerPk =>
      sender() ! publicKeyOpt
  }

  private def queryWallet: Receive = {
    case QueryWallet =>
      val callback = self
      viewHolderRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Unit] { v =>
        v.vault.firstSecret.onComplete(
          _.foreach {
            _.fold(
              _ => {
                log.warn("Failed to load key from wallet. Wallet is locked.")
                context.system.scheduler.scheduleOnce(4.seconds, self, QueryWallet)(context.system.dispatcher)
              },
              r => callback ! UpdateSecret(r)
            )
          }
        )
      }
  }

  private def startMining: Receive = {
    case StartMining if candidateOpt.nonEmpty && !isMining && ergoSettings.nodeSettings.mining =>
      candidateOpt.foreach { candidate =>
        publicKeyOpt match {
          case Some(_) =>
            isMining = true
            if (!externalMinerMode) {
              log.info("Starting native miner")
              runMiningThreads(candidate)
            } else {
              log.info("Ready to serve external miner")
              // Refresh candidate block if it was formed before NVH state restore in response to API requests
              requestCandidate()
            }
          case None =>
            log.warn("Got start mining command while public key is not ready")
        }
      }
    case StartMining if candidateOpt.isEmpty =>
      if (secretKeyOpt.isDefined || externalMinerMode) requestCandidate()
      context.system.scheduler.scheduleOnce(1.seconds, self, StartMining)(context.system.dispatcher)
  }

  private def runMiningThreads(candidateBlock: CandidateBlock): Unit = {
    secretKeyOpt match {
      case Some(sk) =>
        miningThreads += ErgoMiningThread(ergoSettings, self, candidateBlock, sk.w, timeProvider)(context)
        miningThreads.foreach(_ ! candidateBlock)
      case None =>
        log.warn("Trying to start native miner while secret key is not ready")
    }
  }

  private def needNewCandidate(b: ErgoFullBlock): Boolean = {
    val parentHeaderIdOpt = candidateOpt.flatMap(_.parentOpt).map(_.id)
    !parentHeaderIdOpt.contains(b.header.id)
  }

  private def shouldStartMine(b: ErgoFullBlock): Boolean = {
    ergoSettings.nodeSettings.mining && b.header.isNew(timeProvider, ergoSettings.chainSettings.blockInterval * 2)
  }

  private def receiveSemanticallySuccessfulModifier: Receive = {
    /**
      * Case when we are already mining by the time modifier arrives and
      * get block from node view that has header's id which isn't equals to our candidate's parent id.
      * That means that our candidate is outdated. Should produce new candidate for ourselves.
      * Stop all current threads and re-run them with newly produced candidate.
      */
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock) if isMining && needNewCandidate(mod) => requestCandidate()

    /**
      * Non obvious but case when mining is enabled, but miner doesn't started yet. Initialization case.
      * We've received block that been generated by somebody else or genesis while we doesn't start.
      * And this block was generated after our miner had been started. That means that we are ready
      * to start mining.
      * This block could be either genesis or generated by another node.
      */
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock) if shouldStartMine(mod) => self ! StartMining

    /**
      * Just ignore all other modifiers.
      */
    case SemanticallySuccessfulModifier(_) =>
  }

  override def receive: Receive =
    receiveSemanticallySuccessfulModifier orElse
      startMining orElse
      onReaders orElse
      keysManagement orElse
      mining orElse
      queryWallet orElse
      unknownMessage

  private def onReaders: Receive = {
    case Readers(h, s, m, _) if s.isInstanceOf[UtxoStateReader] =>
      publicKeyOpt.foreach { minerProp =>
        createCandidate(minerProp, h, m, desiredUpdate, s.asInstanceOf[UtxoStateReader]) match {
          case Success(candidate) =>
            val candidateMsg = powScheme.msgByHeader(AutolykosPowScheme.deriveUnprovedHeader(candidate))
            log.info(s"New candidate with msg ${Base16.encode(candidateMsg)} generated")
            procCandidateBlock(candidate)
          case Failure(e) => log.warn("Failed to produce candidate block.", e)
        }
      }
  }

  private def mining: Receive = {
    case PrepareCandidate if !ergoSettings.nodeSettings.mining =>
      sender() ! Future.failed(
        new Exception("Candidate creation is not supported when mining is disabled"))

    case PrepareCandidate if candidateOpt.isDefined =>
      sender() ! candidateOpt
        .flatMap { c =>
          publicKeyOpt.map(powScheme.deriveExternalCandidate(c, _))
        }
        .fold[Future[ExternalCandidateBlock]](
        Future.failed(new Exception("Failed to create candidate")))(Future.successful)

    case PrepareCandidate =>
      val readersR = (readersHolderRef ? GetReaders).mapTo[Readers]
      sender() ! readersR.flatMap {
        case Readers(h, s, m, _) if s.isInstanceOf[UtxoStateReader] =>
          publicKeyOpt
            .flatMap { pk =>
              createCandidate(pk, h, m, desiredUpdate, s.asInstanceOf[UtxoStateReader])
                .map { c =>
                  candidateOpt = Some(c)
                  powScheme.deriveExternalCandidate(c, pk)
                }
                .toOption
            }
            .fold[Future[ExternalCandidateBlock]](
            Future.failed(new Exception("Failed to create candidate")))(Future.successful)
        case _ =>
          Future.failed(new Exception("Invalid readers state"))
      }

    case solution: AutolykosSolution =>
      val result: Future[Unit] = candidateOpt.map { c =>
        val newBlock = powScheme.completeBlock(c, solution)
        powScheme.validate(newBlock.header).map(_ => newBlock)
      } match {
        case Some(Success(newBlock)) =>
          sendToNodeView(newBlock)
          Future.successful(())
        case Some(Failure(exception)) =>
          Future.failed(exception)
        case None =>
          Future.failed(new Exception("Invalid miner state"))
      }
      log.debug(s"Processed solution $solution with the result result $result")
      if (externalMinerMode) sender() ! result
  }

  private def sendToNodeView(newBlock: ErgoFullBlock): Unit = {
    log.info(s"New block ${newBlock.id} at nonce ${Longs.fromByteArray(newBlock.header.powSolution.n)}")
    viewHolderRef ! LocallyGeneratedModifier(newBlock.header)
    val sectionsToApply = if (ergoSettings.nodeSettings.stateType == StateType.Digest) {
      newBlock.blockSections
    } else {
      newBlock.mandatoryBlockSections
    }
    sectionsToApply.foreach(viewHolderRef ! LocallyGeneratedModifier(_))
  }

  private def procCandidateBlock(c: CandidateBlock): Unit = {
    log.debug(s"Got candidate block at height ${ErgoHistory.heightOf(c.parentOpt) + 1}" +
      s" with ${c.transactions.size} transactions")
    candidateOpt = Some(c)
    if (!externalMinerMode) miningThreads.foreach(_ ! c)
  }

  private def createCandidate(minerPk: ProveDlog,
                              history: ErgoHistoryReader,
                              pool: ErgoMemPoolReader,
                              proposedUpdate: ErgoValidationSettingsUpdate,
                              state: UtxoStateReader): Try[CandidateBlock] = Try {
    val bestHeaderOpt: Option[Header] = history.bestFullBlockOpt.map(_.header)
    val bestExtensionOpt: Option[Extension] = bestHeaderOpt
      .flatMap(h => history.typedModifierById[Extension](h.extensionId))
    val timestamp = Math.max(timeProvider.time(), bestHeaderOpt.map(_.timestamp + 1).getOrElse(0L))
    val stateContext = state.stateContext
    val nBits: Long = bestHeaderOpt
      .map(parent => history.requiredDifficultyAfter(parent))
      .map(d => RequiredDifficulty.encodeCompactBits(d))
      .getOrElse(ergoSettings.chainSettings.initialNBits)
    val interlinksExtension = PoPowAlgos.interlinksToExtension(updateInterlinks(bestHeaderOpt, bestExtensionOpt))

    val (extensionCandidate, votes: Array[Byte], version: Byte) = bestHeaderOpt.map { header =>
      val newHeight = header.height + 1
      val currentParams = stateContext.currentParameters
      val betterVersion = protocolVersion > header.version
      val votingFinishHeight: Option[Height] = currentParams.softForkStartingHeight
        .map(_ + votingSettings.votingLength * votingSettings.softForkEpochs)
      val forkVotingAllowed = votingFinishHeight.forall(fh => newHeight < fh)
      val forkOrdered = ergoSettings.votingTargets.softFork != 0
      val voteForFork = betterVersion && forkOrdered && forkVotingAllowed

      if (newHeight % votingEpochLength == 0 && newHeight > 0) {
        val (newParams, activatedUpdate) = currentParams.update(newHeight, voteForFork, stateContext.votingData.epochVotes, proposedUpdate, votingSettings)
        val newValidationSettings = stateContext.validationSettings.updated(activatedUpdate)
        (newParams.toExtensionCandidate ++ interlinksExtension ++ newValidationSettings.toExtensionCandidate,
          newParams.suggestVotes(ergoSettings.votingTargets.targets, voteForFork),
          newParams.blockVersion)
      } else {
        (interlinksExtension,
          currentParams.vote(ergoSettings.votingTargets.targets, stateContext.votingData.epochVotes, voteForFork),
          currentParams.blockVersion)
      }
    }.getOrElse((interlinksExtension, Array(0: Byte, 0: Byte, 0: Byte), Header.CurrentVersion))

    val upcomingContext = state.stateContext.upcoming(minerPk.h, timestamp, nBits, votes, proposedUpdate, version)
    //only transactions valid from against the current utxo state we take from the mem pool
    val emissionTxOpt = ErgoMiner.collectEmission(state, minerPk, ergoSettings.chainSettings.emissionRules).map { tx =>
      implicit val verifier: ErgoInterpreter = ErgoInterpreter(state.stateContext.currentParameters)
      val cost = state.validateWithCost(tx, Some(upcomingContext), Int.MaxValue).get
      tx -> cost
    }

    val (txs, toEliminate) = ErgoMiner.collectTxs(minerPk,
      state.stateContext.currentParameters.maxBlockCost,
      state.stateContext.currentParameters.maxBlockSize,
      maxTransactionComplexity,
      state,
      upcomingContext,
      pool.getAllPrioritized,
      emissionTxOpt.toSeq)(state.stateContext.validationSettings)

    // remove transactions which turned out to be invalid
    if (toEliminate.nonEmpty) viewHolderRef ! EliminateTransactions(toEliminate)

    state.proofsForTransactions(txs).map { case (adProof, adDigest) =>
      CandidateBlock(bestHeaderOpt, version, nBits, adDigest, adProof, txs, timestamp, extensionCandidate, votes)
    }
  }.flatten

  def requestCandidate(): Unit = {
    log.info("Requesting candidate")
    readersHolderRef ! GetReaders
  }

}


object ErgoMiner extends ScorexLogging {

  //TODO move ErgoMiner to mining package and make `collectTxs` and `fixTxsConflicts` private[mining]

  def collectEmission(state: UtxoStateReader,
                      minerPk: ProveDlog,
                      emission: EmissionRules): Option[ErgoTransaction] = {
    collectRewards(state.emissionBoxOpt, state.stateContext.currentHeight, Seq.empty, minerPk, emission, Colls.emptyColl)
      .headOption
  }


  def collectFees(currentHeight: Int,
                  txs: Seq[ErgoTransaction],
                  minerPk: ProveDlog,
                  emission: EmissionRules): Option[ErgoTransaction] = {
    collectRewards(None, currentHeight, txs, minerPk, emission, Colls.emptyColl).headOption
  }

  /**
    * Generate from 0 to 2 transaction that collecting rewards from fee boxes in block transactions `txs` and
    * emission box `emissionBoxOpt`
    */
  def collectRewards(emissionBoxOpt: Option[ErgoBox],
                     currentHeight: Int,
                     txs: Seq[ErgoTransaction],
                     minerPk: ProveDlog,
                     emission: EmissionRules,
                     assets: Coll[(TokenId, Long)] = Colls.emptyColl): Seq[ErgoTransaction] = {
    val propositionBytes = emission.settings.feePropositionBytes

    val inputs = txs.flatMap(_.inputs)
    val feeBoxes: Seq[ErgoBox] = ErgoState.boxChanges(txs)._2
      .filter(b => java.util.Arrays.equals(b.propositionBytes, propositionBytes))
      .filter(b => !inputs.exists(i => java.util.Arrays.equals(i.boxId, b.id)))
    val nextHeight = currentHeight + 1
    val minerProp = ErgoScriptPredef.rewardOutputScript(emission.settings.minerRewardDelay, minerPk)

    val emissionTxOpt: Option[ErgoTransaction] = emissionBoxOpt.map { emissionBox =>
      val prop = emissionBox.ergoTree
      val emissionAmount = emission.minersRewardAtHeight(nextHeight)
      val newEmissionBox: ErgoBoxCandidate = new ErgoBoxCandidate(
        emissionBox.value - emissionAmount, prop, nextHeight)
      val inputs = IndexedSeq(new Input(emissionBox.id, ProverResult(Array.emptyByteArray, ContextExtension.empty)))

      val minerBox = new ErgoBoxCandidate(emissionAmount, minerProp, nextHeight, assets)

      ErgoTransaction(
        inputs,
        IndexedSeq(),
        IndexedSeq(newEmissionBox, minerBox)
      )
    }
    val feeTxOpt: Option[ErgoTransaction] = if (feeBoxes.nonEmpty) {
      val feeAmount = feeBoxes.map(_.value).sum
      val feeAssets = feeBoxes.toColl.flatMap(_.additionalTokens).take(ErgoBox.MaxTokens - 1)
      val inputs = feeBoxes.map(b => new Input(b.id, ProverResult(Array.emptyByteArray, ContextExtension.empty)))
      val minerBox = new ErgoBoxCandidate(feeAmount, minerProp, nextHeight, feeAssets, Map())
      Some(ErgoTransaction(inputs.toIndexedSeq, IndexedSeq(), IndexedSeq(minerBox)))
    } else {
      None
    }
    Seq(emissionTxOpt, feeTxOpt).flatten
  }

  /**
    * Collects valid non-conflicting transactions from `mempoolTxsIn` and adds a transaction collecting fees from
    * them to `minerPk`.
    *
    * Resulting transactions total cost does not exceeds `maxBlockCost`, total size does not exceeds `maxBlockSize`,
    * and the miner's transaction is correct.
    *
    * @return - transactions to include to the block, transaction ids turned out to be invalid.
    */
  def collectTxs(minerPk: ProveDlog,
                 maxBlockCost: Long,
                 maxBlockSize: Long,
                 maxTransactionComplexity: Int,
                 us: UtxoStateReader,
                 upcomingContext: ErgoStateContext,
                 mempoolTxsIn: Iterable[ErgoTransaction],
                 startTransactions: Seq[(ErgoTransaction, Long)])
                (implicit vs: ValidationSettings): (Seq[ErgoTransaction], Seq[ModifierId]) = {

    def correctLimits(blockTxs: Seq[(ErgoTransaction, Long)]): Boolean = {
      blockTxs.map(_._2).sum < maxBlockCost && blockTxs.map(_._1.size).sum < maxBlockSize
    }

    @tailrec
    def loop(mempoolTxs: Iterable[ErgoTransaction],
             acc: Seq[(ErgoTransaction, Long)],
             lastFeeTx: Option[(ErgoTransaction, Long)],
             invalidTxs: Seq[ModifierId]): (Seq[ErgoTransaction], Seq[ModifierId]) = {
      // transactions from mempool and fee txs from the previous step
      def current: Seq[ErgoTransaction] = (acc ++ lastFeeTx).map(_._1)

      mempoolTxs.headOption match {
        case Some(tx) =>
          implicit val verifier: ErgoInterpreter = ErgoInterpreter(us.stateContext.currentParameters)
          // check validity and calculate transaction cost
          us.validateWithCost(tx, Some(upcomingContext), maxTransactionComplexity) match {
            case Success(costConsumed) =>
              val newTxs = fixTxsConflicts((tx, costConsumed) +: acc)
              val newBoxes = newTxs.flatMap(_._1.outputs)
              val emissionRules = us.constants.settings.chainSettings.emissionRules

              ErgoMiner.collectFees(us.stateContext.currentHeight, newTxs.map(_._1), minerPk, emissionRules) match {
                case Some(feeTx) =>
                  val boxesToSpend = feeTx.inputs.flatMap(i => newBoxes.find(b => java.util.Arrays.equals(b.id, i.boxId)))
                  feeTx.statefulValidity(boxesToSpend, IndexedSeq(), upcomingContext) match {
                    case Success(cost) =>
                      val blockTxs: Seq[(ErgoTransaction, Long)] = (feeTx -> cost) +: newTxs
                      if (correctLimits(blockTxs)) {
                        loop(mempoolTxs.tail, newTxs, Some(feeTx -> cost), invalidTxs)
                      } else {
                        current -> invalidTxs
                      }
                    case Failure(e) =>
                      log.debug(s"Fee collecting tx is invalid, return current: ${e.getMessage} from ${us.stateContext}")
                      current -> invalidTxs
                  }
                case None =>
                  log.debug(s"No fee proposition found in txs ${newTxs.map(_._1.id)} ")
                  val blockTxs: Seq[(ErgoTransaction, Long)] = newTxs ++ lastFeeTx.toSeq
                  if (correctLimits(blockTxs)) {
                    loop(mempoolTxs.tail, blockTxs, lastFeeTx, invalidTxs)
                  } else {
                    current -> invalidTxs
                  }
              }
            case Failure(e) =>
              log.debug(s"Do not include transaction ${tx.id} due to ${e.getMessage}")
              loop(mempoolTxs.tail, acc, lastFeeTx, invalidTxs :+ tx.id)
          }
        case _ => // mempool is empty
          current -> invalidTxs
      }
    }

    loop(mempoolTxsIn, startTransactions, None, Seq.empty)
  }

  def fixTxsConflicts(txs: Seq[(ErgoTransaction, Long)]): Seq[(ErgoTransaction, Long)] = {
    txs.foldLeft((Seq.empty[(ErgoTransaction, Long)], Set.empty[ByteArrayWrapper])) { case ((s, keys), (tx, cost)) =>
      val bxsBaw = tx.inputs.map(_.boxId).map(ByteArrayWrapper.apply)
      if (bxsBaw.forall(k => !keys.contains(k)) && bxsBaw.size == bxsBaw.toSet.size) {
        (s :+ (tx, cost)) -> (keys ++ bxsBaw)
      } else {
        (s, keys)
      }
    }._1
  }

  case object StartMining

  case object QueryWallet

  case object PrepareCandidate

  case object ReadMinerPk

  case class UpdateSecret(s: DLogProverInput)

}

object ErgoMinerRef {

  def props(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            timeProvider: NetworkTimeProvider,
            skOpt: Option[DLogProverInput] = None): Props =
    Props(new ErgoMiner(ergoSettings, viewHolderRef, readersHolderRef, timeProvider, skOpt))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            timeProvider: NetworkTimeProvider,
            skOpt: Option[DLogProverInput] = None)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, readersHolderRef, timeProvider, skOpt))

}
