package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorRefFactory, PoisonPill, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.google.common.primitives.Longs
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform._
import org.ergoplatform.mining.AutolykosPowScheme.derivedHeaderFields
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.PoPowAlgos._
import org.ergoplatform.modifiers.history._
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
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging}
import sigmastate.SType.ErgoBoxRType
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.interpreter.{ContextExtension, ProverResult}
import special.collection.Coll

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Actor which feeds external miners with needed data, or controls in-built miner.
  *
  * This class needs for access to a secret used in mining if "miningPubKeyHex" is not set in the config
  * (`inSecretKeyOpt` carrying it if test mnemonic is used, otherwise, the actor is asking the wallet for a key).
  */
class ErgoMiner(ergoSettings: ErgoSettings,
                viewHolderRef: ActorRef,
                readersHolderRef: ActorRef,
                timeProvider: NetworkTimeProvider,
                inSecretKeyOpt: Option[DLogProverInput]) extends Actor with ScorexLogging {

  import ErgoMiner._

  private implicit val timeout: Timeout = 5.seconds

  // votes for parameters and soft-fork provided in the config
  private val desiredUpdate = ergoSettings.votingTargets.desiredUpdate

  private val votingSettings = ergoSettings.chainSettings.voting
  private val votingEpochLength = votingSettings.votingLength
  private val protocolVersion = ergoSettings.chainSettings.protocolVersion
  private val powScheme = ergoSettings.chainSettings.powScheme
  private val externalMinerMode = ergoSettings.nodeSettings.useExternalMiner
  private val maxTransactionComplexity: Int = ergoSettings.nodeSettings.maxTransactionComplexity

  // shared mutable state

  // flag which is set once when first block candidate if formed
  private var isMining = false

  // cached block candidate
  private var candidateOpt: Option[CandidateCache] = None

  // internal miner threads, if internal mining is chosen
  private val miningThreads: mutable.Buffer[ActorRef] = new ArrayBuffer[ActorRef]()

  private var secretKeyOpt: Option[DLogProverInput] = inSecretKeyOpt

  // "miningPubkeyHex" setting in config has preference over wallet's secret key
  private var publicKeyOpt: Option[ProveDlog] = ergoSettings.miningPubKey.orElse(inSecretKeyOpt.map(_.publicImage))

  override def preStart(): Unit = {
    // in external miner mode key from wallet is used if `publicKeyOpt` is not set
    if ((publicKeyOpt.isEmpty && externalMinerMode) || (secretKeyOpt.isEmpty && !externalMinerMode)) {
      log.info("Trying to use key from wallet for mining")
      self ! QueryWallet
    }
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def postStop(): Unit = {
    log.info("Stopping miner's threads.")
    miningThreads.foreach(_ ! PoisonPill)
    miningThreads.clear()
  }

  private def unknownMessage: Receive = {
    case _: scala.runtime.BoxedUnit =>
      // ignore, this message is caused by way of interaction with NodeViewHolder.
    case m =>
      log.warn(s"Unexpected message $m of class: ${m.getClass}")
  }

  private def keysManagement: Receive = {
    case UpdateSecret(s) =>
      secretKeyOpt = Some(s)
      publicKeyOpt = Some(s.publicImage)

    // used in /mining/rewardAddress aPI method
    case ReadMinerPk =>
      sender() ! publicKeyOpt
  }

  private def queryWallet: Receive = {
    case QueryWallet =>
      val callback = self
      viewHolderRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Unit] { v =>
        v.vault.firstSecret.onComplete {
          _.flatten match {
            case Failure(t) =>
              log.warn(s"Miner can't load key from wallet: ${t.getMessage} ")
              context.system.scheduler.scheduleOnce(4.seconds, self, QueryWallet)(context.system.dispatcher)
            case Success(proverInput: DLogProverInput) =>
              callback ! UpdateSecret(proverInput)
          }
        }
      }
  }

  private def startMining: Receive = {
    case StartMining if candidateOpt.isEmpty =>
      if (secretKeyOpt.isDefined || externalMinerMode) requestCandidate()
      context.system.scheduler.scheduleOnce(1.seconds, self, StartMining)(context.system.dispatcher)

    case StartMining if candidateOpt.nonEmpty && !isMining && ergoSettings.nodeSettings.mining =>
      candidateOpt.foreach { candidate =>
        publicKeyOpt match {
          case Some(_) =>
            isMining = true
            if (!externalMinerMode) {
              log.info("Starting native miner")
              startInternalMiner(candidate.candidateBlock)
            } else {
              log.info("Ready to serve external miner")
              // Refresh candidate block if it was formed before NVH state restore in response to API requests
              requestCandidate()
            }
          case None =>
            log.warn("Got start mining command while public key is not ready")
        }
      }
  }

  // Start internal miner's threads. Called once.
  private def startInternalMiner(candidateBlock: CandidateBlock): Unit = {
    secretKeyOpt match {
      case Some(sk) =>
        miningThreads += ErgoMiningThread(ergoSettings, self, candidateBlock, sk.w, timeProvider)(context)
        miningThreads.foreach(_ ! candidateBlock)
      case None =>
        log.warn("Trying to start native miner while secret key is not ready")
    }
  }

  private def needNewCandidate(b: ErgoFullBlock): Boolean = {
    val parentHeaderIdOpt = candidateOpt.map(_.candidateBlock).flatMap(_.parentOpt).map(_.id)
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
      * Non obvious but case when mining is enabled, but miner isn't started yet. Initialization case.
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

  // checks that current candidate block contains `txs`
  private[mining] def cachedFor(txs: Seq[ErgoTransaction]): Boolean = {
    candidateOpt.isDefined && candidateOpt.exists { c =>
      txs.isEmpty || (txs.size == c.txsToInclude.size && txs.forall(c.txsToInclude.contains))
    }
  }

  // helper method to update cached candidate block and corresponding message for external miners
  private def updateCandidate(candidate: CandidateBlock,
                              pk: ProveDlog,
                              txsToInclude: Seq[ErgoTransaction]): WorkMessage = {
    val ext = powScheme.deriveExternalCandidate(candidate, pk, txsToInclude.map(_.id))
    log.info(s"New candidate with msg ${Base16.encode(ext.msg)} generated")
    log.debug(s"Got candidate block at height ${ErgoHistory.heightOf(candidate.parentOpt) + 1}" +
      s" with ${candidate.transactions.size} transactions")
    candidateOpt = Some(CandidateCache(candidate, ext, txsToInclude))
    if (!externalMinerMode) miningThreads.foreach(_ ! candidate)
    ext
  }

  private def onReaders: Receive = {
    // Miner's node can produce block candidate only if it is working in the UTXO regime
    case Readers(h, s: UtxoStateReader, m, _) =>
      //mandatory transactions to include into next block taken from the previous candidate
      val txsToInclude = candidateOpt.map(_.txsToInclude).getOrElse(Seq.empty).filter { tx =>
        inputsNotSpent(tx, s)
      }

      publicKeyOpt.foreach { pk =>
        createCandidate(pk, h, m, desiredUpdate, s, txsToInclude) match {
          case Success(candidate) =>
            updateCandidate(candidate, pk, txsToInclude)
          case Failure(e) =>
            log.warn("Failed to produce candidate block.", e)
            //candidate cleared, included mandatory transactions to include
            candidateOpt = None
        }
      }
  }

  private def mining: Receive = {
    case PrepareCandidate(_) if !ergoSettings.nodeSettings.mining =>
      sender() ! Future.failed(new Exception("Candidate creation is not supported when mining is disabled"))

    // Send cached candidate if it is available and (non-empty) list of transactions to include hasn't been changed
    case PrepareCandidate(txsToInclude) =>
      if (cachedFor(txsToInclude)) {
        val candBlockFuture = candidateOpt
          .map(_.externalVersion)
          .fold[Future[WorkMessage]](
          Future.failed(new Exception("Failed to create candidate")))(Future.successful)
        sender() ! candBlockFuture
      } else {
        val readersR = (readersHolderRef ? GetReaders).mapTo[Readers]
        val candBlockFuture = readersR.flatMap {
          case Readers(h, s: UtxoStateReader, m, _) =>
            Future.fromTry(publicKeyOpt match {
              case Some(pk) =>
                val ts = txsToInclude.filter { tx => inputsNotSpent(tx, s) }
                createCandidate(pk, h, m, desiredUpdate, s, ts)
                  .map(candidate => updateCandidate(candidate, pk, ts)) //returns external candidate
              case None => Failure(new Exception("Candidate could not be generated: no public key available"))
            })
          case _ =>
            Future.failed(new Exception("Invalid readers state, mining is possible in UTXO mode only"))
        }
        sender() ! candBlockFuture
      }

    case solution: AutolykosSolution =>
      val result: Future[Unit] = candidateOpt.map { c =>
        val newBlock = completeBlock(c.candidateBlock, solution)
        powScheme.validate(newBlock.header).map(_ => newBlock)
      } match {
        case Some(Success(newBlock)) =>
          sendToNodeView(newBlock)
          Future.successful(())
        case Some(Failure(exception)) =>
          Future.failed(new Exception(s"Improper block mined: ${exception.getMessage}", exception))
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

  /**
    * Assemble correct block candidate based on
    *
    * @param minerPk - public key of the miner
    * @param history - blockchain reader (to extract parent)
    * @param pool - memory pool reader
    * @param proposedUpdate - votes for parameters and soft-fork
    * @param state - UTXO set reader
    * @param prioritizedTransactions - transactions which are going into the block in the first place
    *                                (before transactions from the pool). No guarantee of inclusion in general case.
    * @return - block candidate or an error
    */
  private def createCandidate(minerPk: ProveDlog,
                              history: ErgoHistoryReader,
                              pool: ErgoMemPoolReader,
                              proposedUpdate: ErgoValidationSettingsUpdate,
                              state: UtxoStateReader,
                              prioritizedTransactions: Seq[ErgoTransaction]): Try[CandidateBlock] = Try {
    // Extract best header and extension of a best block user their data for assembling a new block
    val bestHeaderOpt: Option[Header] = history.bestFullBlockOpt.map(_.header)
    val bestExtensionOpt: Option[Extension] = bestHeaderOpt
      .flatMap(h => history.typedModifierById[Extension](h.extensionId))

    // Make progress in time since last block.
    // If no progress is made, then, by consensus rules, the block will be rejected.
    val timestamp = Math.max(timeProvider.time(), bestHeaderOpt.map(_.timestamp + 1).getOrElse(0L))

    val stateContext = state.stateContext

    // Calculate required difficulty for the new block
    val nBits: Long = bestHeaderOpt
      .map(parent => history.requiredDifficultyAfter(parent))
      .map(d => RequiredDifficulty.encodeCompactBits(d))
      .getOrElse(ergoSettings.chainSettings.initialNBits)

    // Obtain NiPoPoW interlinks vector to pack it into the extension section
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
    val poolTxs = pool.getAllPrioritized

    val emissionTxOpt = ErgoMiner.collectEmission(state, minerPk, ergoSettings.chainSettings.emissionRules)
    val mt = emissionTxOpt.toSeq ++ prioritizedTransactions

    val (txs, toEliminate) = ErgoMiner.collectTxs(minerPk,
      state.stateContext.currentParameters.maxBlockCost,
      state.stateContext.currentParameters.maxBlockSize,
      maxTransactionComplexity,
      state,
      upcomingContext,
      mt ++ poolTxs)(state.stateContext.validationSettings)

    // remove transactions which turned out to be invalid
    if (toEliminate.nonEmpty) viewHolderRef ! EliminateTransactions(toEliminate)

    state.proofsForTransactions(txs) match {
      case Success((adProof, adDigest)) =>
        Success(CandidateBlock(bestHeaderOpt, version, nBits, adDigest, adProof, txs, timestamp, extensionCandidate, votes))
      case Failure(t: Throwable) =>
        // We can not produce a block for some reason, so print out an error
        // and collect only emission transaction if it exists.
        // We consider that emission transaction is always valid.
        emissionTxOpt match {
          case Some(emissionTx) =>
            log.error("Failed to produce proofs for transactions, but emission box is found: ", t)
            val fallbackTxs = Seq(emissionTx)
            state.proofsForTransactions(fallbackTxs).map { case (adProof, adDigest) =>
              CandidateBlock(bestHeaderOpt, version, nBits, adDigest, adProof, fallbackTxs, timestamp, extensionCandidate, votes)
            }
          case None =>
            log.error("Failed to produce proofs for transactions and no emission box available: ", t)
            Failure(t)
        }
    }
  }.flatten

  def requestCandidate(): Unit = {
    log.info("Requesting candidate")
    readersHolderRef ! GetReaders
  }

}


object ErgoMiner extends ScorexLogging {

  // helper which is checking that inputs of the transaction are not spent
  private def inputsNotSpent(tx: ErgoTransaction, s: UtxoStateReader): Boolean = {
    tx.inputs.forall(inp => s.boxById(inp.boxId).isDefined)
  }

  /**
    * Holder for both candidate block and data for external miners derived from it
    * (to avoid possibly costly recalculation)
    *
    * @param candidateBlock - block candidate
    * @param externalVersion - message for external miner
    * @param txsToInclude - transactions which were prioritized for inclusion in the block candidate
    */
  private case class CandidateCache(candidateBlock: CandidateBlock,
                                    externalVersion: WorkMessage,
                                    txsToInclude: Seq[ErgoTransaction])

  /**
    * Transaction and its cost.
    */
  case class CostedTransaction(tx: ErgoTransaction, cost: Long)


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
      val inputs = IndexedSeq(new Input(emissionBox.id, ProverResult.empty))

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
      val inputs = feeBoxes.map(b => new Input(b.id, ProverResult.empty))
      val minerBox = new ErgoBoxCandidate(feeAmount, minerProp, nextHeight, feeAssets, Map())
      Some(ErgoTransaction(inputs.toIndexedSeq, IndexedSeq(), IndexedSeq(minerBox)))
    } else {
      None
    }
    Seq(emissionTxOpt, feeTxOpt).flatten
  }

  /**
    * Helper function which decides whether transactions can fit into a block with given cost and size limits
    */
  def correctLimits(blockTxs: Seq[CostedTransaction],
                    maxBlockCost: Long,
                    maxBlockSize: Long): Boolean = {
    val totalCost = blockTxs.map(_.cost).sum
    val totalSize = blockTxs.map(_.tx.size).sum
    totalCost < maxBlockCost && totalSize < maxBlockSize
  }


  /**
    * Collects valid non-conflicting transactions from `mandatoryTxs` and then `mempoolTxsIn` and adds a transaction
    * collecting fees from them to `minerPk`.
    *
    * Resulting transactions total cost does not exceed `maxBlockCost`, total size does not exceed `maxBlockSize`,
    * and the miner's transaction is correct.
    *
    * @return - transactions to include into the block, transaction ids turned out to be invalid.
    */
  def collectTxs(minerPk: ProveDlog,
                 maxBlockCost: Long,
                 maxBlockSize: Long,
                 maxTransactionComplexity: Int,
                 us: UtxoStateReader,
                 upcomingContext: ErgoStateContext,
                 transactions: Iterable[ErgoTransaction])
                (implicit vs: ValidationSettings): (Seq[ErgoTransaction], Seq[ModifierId]) = {

    @tailrec
    def loop(mempoolTxs: Iterable[ErgoTransaction],
             acc: Seq[CostedTransaction],
             lastFeeTx: Option[CostedTransaction],
             invalidTxs: Seq[ModifierId]): (Seq[ErgoTransaction], Seq[ModifierId]) = {
      // transactions from mempool and fee txs from the previous step
      def current: Seq[ErgoTransaction] = (acc ++ lastFeeTx).map(_.tx)

      val stateWithTxs = us.withTransactions(current)

      mempoolTxs.headOption match {
        case Some(tx) =>

          if (!inputsNotSpent(tx, stateWithTxs) || doublespend(current, tx)) {
            //mark transaction as invalid if it tries to do double-spending or trying to spend outputs not present
            //do these checks before validating the scripts to save time
            current -> (invalidTxs :+ tx.id)
          } else {
            implicit val verifier: ErgoInterpreter = ErgoInterpreter(us.stateContext.currentParameters)
            // check validity and calculate transaction cost
            stateWithTxs.validateWithCost(tx, Some(upcomingContext), maxTransactionComplexity) match {
              case Success(costConsumed) =>
                val newTxs = acc :+ CostedTransaction(tx, costConsumed)
                val newBoxes = newTxs.flatMap(_.tx.outputs)

                val emissionRules = stateWithTxs.constants.settings.chainSettings.emissionRules
                val currentHeight = stateWithTxs.stateContext.currentHeight
                val feeTxOpt = ErgoMiner.collectFees(currentHeight, newTxs.map(_.tx), minerPk, emissionRules)
                feeTxOpt match {
                  case Some(feeTx) =>
                    val boxesToSpend = feeTx.inputs.flatMap(i => newBoxes.find(b => java.util.Arrays.equals(b.id, i.boxId)))
                    feeTx.statefulValidity(boxesToSpend, IndexedSeq(), upcomingContext) match {
                      case Success(cost) =>
                        val blockTxs: Seq[CostedTransaction] = CostedTransaction(feeTx, cost) +: newTxs
                        if (correctLimits(blockTxs, maxBlockCost, maxBlockSize)) {
                          loop(mempoolTxs.tail, newTxs, Some(CostedTransaction(feeTx, cost)), invalidTxs)
                        } else {
                          current -> invalidTxs
                        }
                      case Failure(e) =>
                        log.debug(s"Fee collecting tx is invalid, return current: ${e.getMessage} from ${stateWithTxs.stateContext}")
                        current -> invalidTxs
                    }
                  case None =>
                    log.debug(s"No fee proposition found in txs ${newTxs.map(_.tx.id)} ")
                    val blockTxs: Seq[CostedTransaction] = newTxs ++ lastFeeTx.toSeq
                    if (correctLimits(blockTxs, maxBlockCost, maxBlockSize)) {
                      loop(mempoolTxs.tail, blockTxs, lastFeeTx, invalidTxs)
                    } else {
                      current -> invalidTxs
                    }
                }
              case Failure(e) =>
                log.debug(s"Not included transaction ${tx.id} due to ${e.getMessage}")
                loop(mempoolTxs.tail, acc, lastFeeTx, invalidTxs :+ tx.id)
            }
          }
        case _ => // mempool is empty
          current -> invalidTxs
      }
    }

    loop(transactions, Seq.empty, None, Seq.empty)
  }

  //Checks that transaction "tx" is not spending outputs spent already by transactions "txs"
  def doublespend(txs: Seq[ErgoTransaction], tx: ErgoTransaction): Boolean = {
    val txsInputs = txs.flatMap(_.inputs.map(_.boxId))
    tx.inputs.exists(i => txsInputs.exists(_.sameElements(i.boxId)))
  }


  /**
    * Derives header without pow from [[CandidateBlock]].
    */
  def deriveUnprovenHeader(candidate: CandidateBlock): HeaderWithoutPow = {
    val (parentId, height) = derivedHeaderFields(candidate.parentOpt)
    val transactionsRoot = BlockTransactions.transactionsRoot(candidate.transactions)
    val adProofsRoot = ADProofs.proofDigest(candidate.adProofBytes)
    val extensionRoot: Digest32 = candidate.extension.digest

    HeaderWithoutPow(
      candidate.version,
      parentId,
      adProofsRoot,
      candidate.stateRoot,
      transactionsRoot,
      candidate.timestamp,
      candidate.nBits,
      height,
      extensionRoot,
      candidate.votes
    )
  }

  /**
    * Assemble `ErgoFullBlock` using candidate block and provided pow solution.
    */
  def completeBlock(candidate: CandidateBlock, solution: AutolykosSolution): ErgoFullBlock = {
    val header = deriveUnprovenHeader(candidate).toHeader(solution, None)
    val adProofs = ADProofs(header.id, candidate.adProofBytes)
    val blockTransactions = BlockTransactions(header.id, candidate.transactions)
    val extension = Extension(header.id, candidate.extension.fields)
    new ErgoFullBlock(header, blockTransactions, extension, Some(adProofs))
  }

  case object StartMining

  case object QueryWallet

  case class PrepareCandidate(toInclude: Seq[ErgoTransaction])

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
