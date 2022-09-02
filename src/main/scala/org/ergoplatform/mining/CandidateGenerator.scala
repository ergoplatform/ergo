package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.pattern.StatusReply
import com.google.common.primitives.Longs
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.mining.AutolykosPowScheme.derivedHeaderFields
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.{Header, HeaderWithoutPow}
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages
import ReceivableMessages.{ChangedHistory, ChangedMempool, ChangedState, NodeViewChange, SemanticallySuccessfulModifier}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, StateType, UtxoStateReader}
import org.ergoplatform.settings.{ErgoSettings, ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.wallet.Constants.MaxAssetsPerBox
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoScriptPredef, Input}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{EliminateTransactions, LocallyGeneratedModifier}
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging}
import sigmastate.SType.ErgoBoxRType
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.interpreter.ProverResult
import special.collection.Coll

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

/** Responsible for generating block candidates and validating solutions.
  * It is observing changes of history, utxo state, mempool and newly applied blocks
  * to generate valid block candidates when it is needed. */
class CandidateGenerator(
  minerPk: ProveDlog,
  readersHolderRef: ActorRef,
  viewHolderRef: ActorRef,
  timeProvider: NetworkTimeProvider,
  ergoSettings: ErgoSettings
) extends Actor
  with ScorexLogging {

  import org.ergoplatform.mining.CandidateGenerator._

  /** retrieve Readers once on start and then get updated by events */
  override def preStart(): Unit = {
    log.info("CandidateGenerator is starting")
    readersHolderRef ! GetReaders
  }

  /** Send solved block to local blockchain controller */
  private def sendToNodeView(newBlock: ErgoFullBlock): Unit = {
    log.info(
      s"New block ${newBlock.id} w. nonce ${Longs.fromByteArray(newBlock.header.powSolution.n)}"
    )
    viewHolderRef ! LocallyGeneratedModifier(newBlock.header)
    val sectionsToApply = if (ergoSettings.nodeSettings.stateType == StateType.Digest) {
      newBlock.blockSections
    } else {
      newBlock.mandatoryBlockSections
    }
    sectionsToApply.foreach(viewHolderRef ! LocallyGeneratedModifier(_))
  }

  override def receive: Receive = {

    /** first we need to get Readers to have some initial state to work with */
    case Readers(h, s: UtxoStateReader, m, _) =>
      val lastHeaders   = h.lastHeaders(500).headers
      val avgMiningTime = getBlockMiningTimeAvg(lastHeaders.map(_.timestamp))
      val avgTxsCount = getTxsPerBlockCountAvg(
        lastHeaders.flatMap(h.getFullBlock).map(_.transactions.size)
      )
      log.info(
        s"CandidateGenerator initialized, avgMiningTime: ${avgMiningTime.toSeconds}s, avgTxsCount: $avgTxsCount"
      )
      context.become(
        initialized(
          CandidateGeneratorState(
            cache       = None,
            solvedBlock = None,
            h,
            s,
            m,
            avgGenTime = 1000.millis
          )
        )
      )
      self ! GenerateCandidate(txsToInclude = Seq.empty, reply = false)
      context.system.eventStream
        .subscribe(self, classOf[SemanticallySuccessfulModifier])
      context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    case Readers(_, _, _, _) =>
      log.error("Invalid readers state, mining is possible in UTXO mode only")
    case m =>
      // retry until initialized
      context.system.scheduler
        .scheduleOnce(100.millis, self, m)(context.dispatcher, sender())
  }

  private def initialized(state: CandidateGeneratorState): Receive = {
    case ChangedHistory(h: ErgoHistoryReader) =>
      context.become(initialized(state.copy(hr = h)))
    case ChangedState(s: UtxoStateReader) =>
      context.become(initialized(state.copy(sr = s)))
    case ChangedMempool(mp: ErgoMemPoolReader) =>
      context.become(initialized(state.copy(mpr = mp)))
    case _: NodeViewChange =>
    // Just ignore all other NodeView Changes

    /**
      * When new block is applied, either one mined by us or received from peers isn't equal to our candidate's parent,
      * we need to generate new candidate and possibly also discard existing solution if it is also behind
      */
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock) =>
      log.info(
        s"Preparing new candidate on getting new block at ${mod.height}"
      )
      if (needNewCandidate(state.cache, mod)) {
        if (needNewSolution(state.solvedBlock, mod))
          context.become(initialized(state.copy(cache = None, solvedBlock = None)))
        else
          context.become(initialized(state.copy(cache = None)))
        self ! GenerateCandidate(txsToInclude = Seq.empty, reply = false)
      } else {
        context.become(initialized(state))
      }

    case SemanticallySuccessfulModifier(_) =>
    // Just ignore all other modifiers.

    case gen @ GenerateCandidate(txsToInclude, reply) =>
      val senderOpt = if (reply) Some(sender()) else None
      if (cachedFor(state.cache, txsToInclude)) {
        senderOpt.foreach(_ ! StatusReply.success(state.cache.get))
      } else {
        val start = System.currentTimeMillis()
        CandidateGenerator.generateCandidate(
          state.hr,
          state.sr,
          state.mpr,
          timeProvider,
          minerPk,
          txsToInclude,
          ergoSettings
        ) match {
          case Some(Failure(ex)) =>
            log.error(s"Candidate generation failed", ex)
            senderOpt.foreach(
              _ ! StatusReply.error(s"Candidate generation failed : ${ex.getMessage}")
            )
          case Some(Success((candidate, eliminatedTxs))) =>
            if (eliminatedTxs.ids.nonEmpty) viewHolderRef ! eliminatedTxs
            val generationTook = System.currentTimeMillis() - start
            log.info(s"Generated new candidate in $generationTook ms")
            context.become(
              initialized(
                state.copy(cache = Some(candidate), avgGenTime = generationTook.millis)
              )
            )
            senderOpt.foreach(_ ! StatusReply.success(candidate))
          case None =>
            log.warn(
              "Can not generate block candidate: either mempool is empty or chain is not synced (maybe last block not fully applied yet"
            )
            senderOpt.foreach { s =>
              context.system.scheduler.scheduleOnce(state.avgGenTime, self, gen)(
                context.system.dispatcher,
                s
              )
            }
        }
      }

    case preSolution: AutolykosSolution
        if state.solvedBlock.isEmpty && state.cache.nonEmpty =>
      // Inject node pk if it is not externally set (in Autolykos 2)
      val solution =
        if (preSolution.pk.isInfinity) {
          AutolykosSolution(minerPk.value, preSolution.w, preSolution.n, preSolution.d)
        } else {
          preSolution
        }
      val result: StatusReply[Unit] = {
        val newBlock = completeBlock(state.cache.get.candidateBlock, solution)
        log.info(s"New block mined, header: ${newBlock.header}")
        ergoSettings.chainSettings.powScheme
          .validate(newBlock.header)
          .map(_ => newBlock) match {
          case Success(newBlock) =>
            sendToNodeView(newBlock)
            context.become(initialized(state.copy(solvedBlock = Some(newBlock))))
            StatusReply.success(())
          case Failure(exception) =>
            log.warn(s"Removing candidate due to invalid block", exception)
            context.become(initialized(state.copy(cache = None)))
            StatusReply.error(
              new Exception(s"Invalid block mined: ${exception.getMessage}", exception)
            )
        }
      }
      log.info(s"Processed solution $solution with the result $result")
      sender() ! result

    case _: AutolykosSolution =>
      sender() ! StatusReply.error(
        s"Block already solved : ${state.solvedBlock.map(_.id)}"
      )

  }

}

object CandidateGenerator extends ScorexLogging {

  /**
    * Holder for both candidate block and data for external miners derived from it
    * (to avoid possibly costly recalculation)
    *
    * @param candidateBlock  - block candidate
    * @param externalVersion - message for external miner
    * @param txsToInclude    - transactions which were prioritized for inclusion in the block candidate
    */
  case class Candidate(
    candidateBlock: CandidateBlock,
    externalVersion: WorkMessage,
    txsToInclude: Seq[ErgoTransaction]
  )

  case class GenerateCandidate(
    txsToInclude: Seq[ErgoTransaction],
    reply: Boolean
  )

  /** Local state of candidate generator to avoid mutable vars */
  case class CandidateGeneratorState(
    cache: Option[Candidate],
    solvedBlock: Option[ErgoFullBlock],
    hr: ErgoHistoryReader,
    sr: UtxoStateReader,
    mpr: ErgoMemPoolReader,
    avgGenTime: FiniteDuration // approximation of average block generation time for more efficient retries
  )

  def apply(
    minerPk: ProveDlog,
    readersHolderRef: ActorRef,
    viewHolderRef: ActorRef,
    timeProvider: NetworkTimeProvider,
    ergoSettings: ErgoSettings
  )(implicit context: ActorRefFactory): ActorRef =
    context.actorOf(
      Props(
        new CandidateGenerator(
          minerPk,
          readersHolderRef,
          viewHolderRef,
          timeProvider,
          ergoSettings
        )
      ),
      s"CandidateGenerator-${Random.alphanumeric.take(5).mkString}"
    )

  /** checks that current candidate block is cached with given `txs` */
  def cachedFor(
    candidateOpt: Option[Candidate],
    txs: Seq[ErgoTransaction]
  ): Boolean = {
    candidateOpt.isDefined && candidateOpt.exists { c =>
      txs.isEmpty || (txs.size == c.txsToInclude.size && txs.forall(
        c.txsToInclude.contains
      ))
    }
  }

  /** we need new candidate if given block is not parent of our cached block */
  def needNewCandidate(
    cache: Option[Candidate],
    bestFullBlock: ErgoFullBlock
  ): Boolean = {
    val parentHeaderIdOpt = cache.map(_.candidateBlock).flatMap(_.parentOpt).map(_.id)
    !parentHeaderIdOpt.contains(bestFullBlock.header.id)
  }

  /** Solution is valid only if bestFullBlock on the chain is its parent */
  def needNewSolution(
    solvedBlock: Option[ErgoFullBlock],
    bestFullBlock: ErgoFullBlock
  ): Boolean =
    solvedBlock.nonEmpty && !solvedBlock.map(_.parentId).contains(bestFullBlock.id)

  /** Calculate average mining time from latest block header timestamps */
  def getBlockMiningTimeAvg(
    timestamps: IndexedSeq[Header.Timestamp]
  ): FiniteDuration = {
    val miningTimes =
      timestamps.sorted
        .sliding(2, 1)
        .map { case IndexedSeq(prev, next) => next - prev }
        .toVector
    Math.round(miningTimes.sum / miningTimes.length.toDouble).millis
  }

  /** Get average count of transactions per block */
  def getTxsPerBlockCountAvg(txsPerBlock: IndexedSeq[Int]): Long =
    Math.round(txsPerBlock.sum / txsPerBlock.length.toDouble)

  /** Helper which is checking that inputs of the transaction are not spent */
  private def inputsNotSpent(tx: ErgoTransaction, s: UtxoStateReader): Boolean =
    tx.inputs.forall(inp => s.boxById(inp.boxId).isDefined)

  /**
    * @return None if chain is not synced or Some of attempt to create candidate
    */
  def generateCandidate(
    h: ErgoHistoryReader,
    s: UtxoStateReader,
    m: ErgoMemPoolReader,
    timeProvider: NetworkTimeProvider,
    pk: ProveDlog,
    txsToInclude: Seq[ErgoTransaction],
    ergoSettings: ErgoSettings
  ): Option[Try[(Candidate, EliminateTransactions)]] = {
    //mandatory transactions to include into next block taken from the previous candidate
    lazy val unspentTxsToInclude = txsToInclude.filter { tx =>
      inputsNotSpent(tx, s)
    }

    val stateContext = s.stateContext

    //only transactions valid from against the current utxo state we take from the mem pool
    lazy val poolTransactions = m.getAllPrioritized

    lazy val emissionTxOpt =
      CandidateGenerator.collectEmission(s, pk, stateContext)

    def chainSynced =
      h.bestFullBlockOpt.map(_.id) == stateContext.lastHeaderOpt.map(_.id)

    def hasAnyMemPoolOrMinerTx =
      poolTransactions.nonEmpty || unspentTxsToInclude.nonEmpty || emissionTxOpt.nonEmpty

    if (!hasAnyMemPoolOrMinerTx) {
      log.info(s"Avoiding generation of a block without any transactions")
      None
    } else if (!chainSynced) {
      log.info(
        "Chain not synced probably due to racing condition when last block is not fully applied yet"
      )
      None
    } else {
      val desiredUpdate = ergoSettings.votingTargets.desiredUpdate
      Some(
        createCandidate(
          pk,
          h,
          desiredUpdate,
          s,
          timeProvider,
          poolTransactions,
          emissionTxOpt,
          unspentTxsToInclude,
          ergoSettings
        )
      )
    }
  }

  /**
    * Private method which suggests to vote for soft-fork (or not)
    *
    * @param ergoSettings - constant settings
    * @param currentParams - network parameters after last block mined
    * @param header - last mined header
    * @return `true` if the node should vote for soft-fork
    */
  private def forkOrdered(ergoSettings: ErgoSettings, currentParams: Parameters, header: Header): Boolean = {
    val nextHeight = header.height + 1

    val protocolVersion = ergoSettings.chainSettings.protocolVersion

    // if protocol version is 2 (node version 4.x, we still can vote for 5.0 soft-fork)
    val betterVersion = if (ergoSettings.networkType.isMainNet && protocolVersion == 2) {
      true
    } else {
      protocolVersion > header.version
    }

    val votingSettings = ergoSettings.chainSettings.voting
    val votingFinishHeight: Option[Height] = currentParams.softForkStartingHeight
      .map(_ + votingSettings.votingLength * votingSettings.softForkEpochs)
    val forkVotingAllowed = votingFinishHeight.forall(fh => nextHeight < fh)

    val nextHeightCondition = if (ergoSettings.networkType.isMainNet) {
      nextHeight >= 823297 // mainnet voting start height, first block of epoch #804
    } else {
      nextHeight >= 4096
    }

    // we automatically vote for 5.0 soft-fork in the mainnet if 120 = 0 vote not provided in settings
    val forkOrdered = if (ergoSettings.networkType.isMainNet && protocolVersion == 2) {
      ergoSettings.votingTargets.softForkOption.getOrElse(1) == 1
    } else {
      ergoSettings.votingTargets.softForkOption.getOrElse(0) == 1
    }

    //todo: remove after 5.0 soft-fork activation
    log.debug(s"betterVersion: $betterVersion, forkVotingAllowed: $forkVotingAllowed, " +
              s"forkOrdered: $forkOrdered, nextHeightCondition: $nextHeightCondition")

    betterVersion &&
      forkVotingAllowed &&
      forkOrdered &&
      nextHeightCondition
  }

  /**
    * Assemble correct block candidate based on
    *
    * @param minerPk                 - public key of the miner
    * @param history                 - blockchain reader (to extract parent)
    * @param proposedUpdate          - votes for parameters update or/and soft-fork
    * @param state                   - UTXO set reader
    * @param timeProvider            - network time provider
    * @param poolTxs                 - memory pool transactions
    * @param emissionTxOpt           - optional emission transaction
    * @param prioritizedTransactions - transactions which are going into the block in the first place
    *                                (before transactions from the pool). No guarantee of inclusion in general case.
    * @return - candidate or an error
    */
  def createCandidate(
                       minerPk: ProveDlog,
                       history: ErgoHistoryReader,
                       proposedUpdate: ErgoValidationSettingsUpdate,
                       state: UtxoStateReader,
                       timeProvider: NetworkTimeProvider,
                       poolTxs: Seq[UnconfirmedTransaction],
                       emissionTxOpt: Option[ErgoTransaction],
                       prioritizedTransactions: Seq[ErgoTransaction],
                       ergoSettings: ErgoSettings
  ): Try[(Candidate, EliminateTransactions)] =
    Try {
      val popowAlgos = new NipopowAlgos(ergoSettings.chainSettings.powScheme)
      // Extract best header and extension of a best block user their data for assembling a new block
      val bestHeaderOpt: Option[Header] = history.bestFullBlockOpt.map(_.header)
      val bestExtensionOpt: Option[Extension] = bestHeaderOpt
        .flatMap(h => history.typedModifierById[Extension](h.extensionId))

      // Make progress in time since last block.
      // If no progress is made, then, by consensus rules, the block will be rejected.
      val timestamp =
        Math.max(timeProvider.time(), bestHeaderOpt.map(_.timestamp + 1).getOrElse(0L))

      val stateContext = state.stateContext

      // Calculate required difficulty for the new block
      val nBits: Long = bestHeaderOpt
        .map(parent => history.requiredDifficultyAfter(parent))
        .map(d => RequiredDifficulty.encodeCompactBits(d))
        .getOrElse(ergoSettings.chainSettings.initialNBits)

      // Obtain NiPoPoW interlinks vector to pack it into the extension section
      val updInterlinks       = popowAlgos.updateInterlinks(bestHeaderOpt, bestExtensionOpt)
      val interlinksExtension = popowAlgos.interlinksToExtension(updInterlinks)
      val votingSettings      = ergoSettings.chainSettings.voting
      val (extensionCandidate, votes: Array[Byte], version: Byte) = bestHeaderOpt
        .map { header =>
          val newHeight     = header.height + 1
          val currentParams = stateContext.currentParameters
          val voteForSoftFork = forkOrdered(ergoSettings, currentParams, header)

          if (newHeight % votingSettings.votingLength == 0 && newHeight > 0) {
            val (newParams, activatedUpdate) = currentParams.update(
              newHeight,
              voteForSoftFork,
              stateContext.votingData.epochVotes,
              proposedUpdate,
              votingSettings
            )
            val newValidationSettings = stateContext.validationSettings.updated(activatedUpdate)
            (
              newParams.toExtensionCandidate ++ interlinksExtension ++ newValidationSettings.toExtensionCandidate,
              newParams.suggestVotes(ergoSettings.votingTargets.targets, voteForSoftFork),
              newParams.blockVersion
            )
          } else {
            (
              interlinksExtension,
              currentParams.vote(
                ergoSettings.votingTargets.targets,
                stateContext.votingData.epochVotes,
                voteForSoftFork
              ),
              currentParams.blockVersion
            )
          }
        }
        .getOrElse(
          (interlinksExtension, Array(0: Byte, 0: Byte, 0: Byte), Header.InitialVersion)
        )

      val upcomingContext = state.stateContext.upcoming(
        minerPk.h,
        timestamp,
        nBits,
        votes,
        proposedUpdate,
        version
      )

      val emissionTxs = emissionTxOpt.toSeq

      // todo: remove in 5.0
      // we allow for some gap, to avoid possible problems when different interpreter version can estimate cost
      // differently due to bugs in AOT costing
      val safeGap = if (state.stateContext.currentParameters.maxBlockCost < 1000000) {
        0
      } else if (state.stateContext.currentParameters.maxBlockCost < 5000000) {
        150000
      } else {
        1000000
      }

      val (txs, toEliminate) = collectTxs(
        minerPk,
        state.stateContext.currentParameters.maxBlockCost - safeGap,
        state.stateContext.currentParameters.maxBlockSize,
        state,
        upcomingContext,
        emissionTxs ++ prioritizedTransactions ++ poolTxs.map(_.transaction)
      )

      val eliminateTransactions = EliminateTransactions(toEliminate)

      if (txs.isEmpty) {
        throw new IllegalArgumentException(
          s"Proofs for 0 txs cannot be generated : emissionTxs: ${emissionTxs.size}, priorityTxs: ${prioritizedTransactions.size}, poolTxs: ${poolTxs.size}"
        )
      }

      def deriveWorkMessage(block: CandidateBlock) = {
        ergoSettings.chainSettings.powScheme.deriveExternalCandidate(
          block,
          minerPk,
          prioritizedTransactions.map(_.id)
        )
      }

      state.proofsForTransactions(txs) match {
        case Success((adProof, adDigest)) =>
          val candidate = CandidateBlock(
            bestHeaderOpt,
            version,
            nBits,
            adDigest,
            adProof,
            txs,
            timestamp,
            extensionCandidate,
            votes
          )
          val ext = deriveWorkMessage(candidate)
          log.info(
            s"Got candidate block at height ${ErgoHistory.heightOf(candidate.parentOpt) + 1}" +
            s" with ${candidate.transactions.size} transactions, msg ${Base16.encode(ext.msg)}"
          )
          Success(
            Candidate(candidate, ext, prioritizedTransactions) -> eliminateTransactions
          )
        case Failure(t: Throwable) =>
          // We can not produce a block for some reason, so print out an error
          // and collect only emission transaction if it exists.
          // We consider that emission transaction is always valid.
          emissionTxOpt match {
            case Some(emissionTx) =>
              log.error(
                "Failed to produce proofs for transactions, but emission box is found: ",
                t
              )
              val fallbackTxs = Seq(emissionTx)
              state.proofsForTransactions(fallbackTxs).map {
                case (adProof, adDigest) =>
                  val candidate = CandidateBlock(
                    bestHeaderOpt,
                    version,
                    nBits,
                    adDigest,
                    adProof,
                    fallbackTxs,
                    timestamp,
                    extensionCandidate,
                    votes
                  )
                  Candidate(
                    candidate,
                    deriveWorkMessage(candidate),
                    prioritizedTransactions
                  ) -> eliminateTransactions
              }
            case None =>
              log.error(
                "Failed to produce proofs for transactions and no emission box available: ",
                t
              )
              Failure(t)
          }
      }
    }.flatten

  /**
    * Transaction and its cost.
    */
  type CostedTransaction = (ErgoTransaction, Int)

  //TODO move ErgoMiner to mining package and make `collectTxs` and `fixTxsConflicts` private[mining]

  def collectEmission(
    state: UtxoStateReader,
    minerPk: ProveDlog,
    stateContext: ErgoStateContext
  ): Option[ErgoTransaction] = {
    collectRewards(
      state.emissionBoxOpt,
      state.stateContext.currentHeight,
      Seq.empty,
      minerPk,
      stateContext,
      Colls.emptyColl
    ).headOption
  }

  def collectFees(
    currentHeight: Int,
    txs: Seq[ErgoTransaction],
    minerPk: ProveDlog,
    stateContext: ErgoStateContext
  ): Option[ErgoTransaction] = {
    collectRewards(None, currentHeight, txs, minerPk, stateContext, Colls.emptyColl).headOption
  }

  /**
    * Generate from 0 to 2 transaction that collecting rewards from fee boxes in block transactions `txs` and
    * emission box `emissionBoxOpt`
    */
  def collectRewards(
    emissionBoxOpt: Option[ErgoBox],
    currentHeight: Int,
    txs: Seq[ErgoTransaction],
    minerPk: ProveDlog,
    stateContext: ErgoStateContext,
    assets: Coll[(TokenId, Long)] = Colls.emptyColl
  ): Seq[ErgoTransaction] = {
    val chainSettings = stateContext.ergoSettings.chainSettings
    val propositionBytes = chainSettings.monetary.feePropositionBytes
    val emission = chainSettings.emissionRules

    // forming transaction collecting emission
    val reemissionSettings = chainSettings.reemission
    val reemissionRules = reemissionSettings.reemissionRules

    val eip27ActivationHeight = reemissionSettings.activationHeight
    val reemissionTokenId = Digest32 @@ reemissionSettings.reemissionTokenIdBytes

    val nextHeight = currentHeight + 1
    val minerProp =
      ErgoScriptPredef.rewardOutputScript(emission.settings.minerRewardDelay, minerPk)

    val emissionTxOpt: Option[ErgoTransaction] = emissionBoxOpt.map { emissionBox =>
      val prop           = emissionBox.ergoTree
      val emissionAmount = emission.minersRewardAtHeight(nextHeight)

      // how many nanoERG should be re-emitted
      lazy val reemissionAmount = reemissionRules.reemissionForHeight(nextHeight, emission)

      val emissionBoxAssets: Coll[(TokenId, Long)] = if (nextHeight == eip27ActivationHeight) {
        // we inject emission box NFT and reemission tokens on activation height
        // see "Activation Details" section of EIP-27
        val injTokens = reemissionSettings.injectionBox.additionalTokens

        //swap tokens if emission NFT is going after reemission
        if (injTokens.apply(1)._2 == 1) {
          Colls.fromItems(injTokens.apply(1), injTokens.apply(0))
        } else {
          injTokens
        }
      } else {
        emissionBox.additionalTokens
      }

      val updEmissionAssets = if (nextHeight >= eip27ActivationHeight) {
        // deduct reemission from emission box
        val reemissionTokens = emissionBoxAssets.apply(1)._2
        val updAmount = reemissionTokens - reemissionAmount
        emissionBoxAssets.updated(1, reemissionTokenId -> updAmount)
      } else {
        emissionBoxAssets
      }

      val newEmissionBox: ErgoBoxCandidate =
        new ErgoBoxCandidate(emissionBox.value - emissionAmount, prop, nextHeight, updEmissionAssets)
      val inputs = if (nextHeight == eip27ActivationHeight) {
        // injection - second input is injection box
        IndexedSeq(
          new Input(emissionBox.id, ProverResult.empty),
          new Input(reemissionSettings.injectionBox.id, ProverResult.empty)
        )
      } else {
        IndexedSeq(new Input(emissionBox.id, ProverResult.empty))
      }

      val minerAmt = if (nextHeight == eip27ActivationHeight) {
        // injection - injection box value going to miner
        emissionAmount + reemissionSettings.injectionBox.value
      } else {
        emissionAmount
      }
      val minersAssets = if (nextHeight >= eip27ActivationHeight) {
        // miner is getting reemission tokens
        assets.append(Colls.fromItems(reemissionTokenId -> reemissionAmount))
      } else {
        assets
      }
      val minerBox = new ErgoBoxCandidate(minerAmt, minerProp, nextHeight, minersAssets)

      val emissionTx = ErgoTransaction(
        inputs,
        dataInputs = IndexedSeq.empty,
        IndexedSeq(newEmissionBox, minerBox)
      )
      log.info(s"Emission tx for nextHeight = $nextHeight: $emissionTx")
      emissionTx
    }

    // forming transaction collecting tx fees
    val inputs = txs.flatMap(_.inputs)
    val feeBoxes: Seq[ErgoBox] = ErgoState
      .newBoxes(txs)
      .filter(b => java.util.Arrays.equals(b.propositionBytes, propositionBytes) && !inputs.exists(i => java.util.Arrays.equals(i.boxId, b.id)))
    val feeTxOpt: Option[ErgoTransaction] = if (feeBoxes.nonEmpty) {
      val feeAmount = feeBoxes.map(_.value).sum
      val feeAssets =
        feeBoxes.toColl.flatMap(_.additionalTokens).take(MaxAssetsPerBox)
      val inputs = feeBoxes.map(b => new Input(b.id, ProverResult.empty))
      val minerBox =
        new ErgoBoxCandidate(feeAmount, minerProp, nextHeight, feeAssets, Map())
      Some(ErgoTransaction(inputs.toIndexedSeq, IndexedSeq(), IndexedSeq(minerBox)))
    } else {
      None
    }

    Seq(emissionTxOpt, feeTxOpt).flatten
  }

  /**
    * Helper function which decides whether transactions can fit into a block with given cost and size limits
    */
  def correctLimits(
    blockTxs: Seq[CostedTransaction],
    maxBlockCost: Long,
    maxBlockSize: Long
  ): Boolean = {
    blockTxs.map(_._2).sum < maxBlockCost && blockTxs.map(_._1.size).sum < maxBlockSize
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
  def collectTxs(
                  minerPk: ProveDlog,
                  maxBlockCost: Int,
                  maxBlockSize: Int,
                  us: UtxoStateReader,
                  upcomingContext: ErgoStateContext,
                  transactions: Seq[ErgoTransaction]
                ): (Seq[ErgoTransaction], Seq[ModifierId]) = {

    val currentHeight = us.stateContext.currentHeight
    val nextHeight = upcomingContext.currentHeight

    log.info(
      s"Assembling a block candidate for block #$nextHeight from ${transactions.length} transactions available"
    )

    val verifier: ErgoInterpreter = ErgoInterpreter(upcomingContext.currentParameters)

    @tailrec
    def loop(
              mempoolTxs: Iterable[ErgoTransaction],
              acc: Seq[CostedTransaction],
              lastFeeTx: Option[CostedTransaction],
              invalidTxs: Seq[ModifierId]
            ): (Seq[ErgoTransaction], Seq[ModifierId]) = {
      // transactions from mempool and fee txs from the previous step
      val currentCosted = acc ++ lastFeeTx
      def current: Seq[ErgoTransaction] = currentCosted.map(_._1)

      val stateWithTxs = us.withTransactions(current)

      mempoolTxs.headOption match {
        case Some(tx) =>
          if (!inputsNotSpent(tx, stateWithTxs) || doublespend(current, tx)) {
            //mark transaction as invalid if it tries to do double-spending or trying to spend outputs not present
            //do these checks before validating the scripts to save time
            log.debug(s"Transaction ${tx.id} double-spending or spending non-existing inputs")
            loop(mempoolTxs.tail, acc, lastFeeTx, invalidTxs :+ tx.id)
          } else {
            // check validity and calculate transaction cost
            stateWithTxs.validateWithCost(
              tx,
              Some(upcomingContext),
              maxBlockCost,
              Some(verifier)
            ) match {
              case Success(costConsumed) =>
                val newTxs = acc :+ (tx -> costConsumed)
                val newBoxes = newTxs.flatMap(_._1.outputs)

                collectFees(currentHeight, newTxs.map(_._1), minerPk, upcomingContext) match {
                  case Some(feeTx) =>
                    val boxesToSpend = feeTx.inputs.flatMap(i =>
                      newBoxes.find(b => java.util.Arrays.equals(b.id, i.boxId))
                    )
                    feeTx.statefulValidity(boxesToSpend, IndexedSeq(), upcomingContext)(verifier) match {
                      case Success(cost) =>
                        val blockTxs: Seq[CostedTransaction] = (feeTx -> cost) +: newTxs
                        if (correctLimits(blockTxs, maxBlockCost, maxBlockSize)) {
                          loop(mempoolTxs.tail, newTxs, Some(feeTx -> cost), invalidTxs)
                        } else {
                          log.debug(s"Finishing block assembly on limits overflow, " +
                                    s"cost is ${currentCosted.map(_._2).sum}, cost limit: $maxBlockCost")
                          current -> invalidTxs
                        }
                      case Failure(e) =>
                        log.warn(
                          s"Fee collecting tx is invalid, not including it, " +
                            s"details: ${e.getMessage} from ${stateWithTxs.stateContext}"
                        )
                        current -> invalidTxs
                    }
                  case None =>
                    log.info(s"No fee proposition found in txs ${newTxs.map(_._1.id)} ")
                    val blockTxs: Seq[CostedTransaction] = newTxs ++ lastFeeTx.toSeq
                    if (correctLimits(blockTxs, maxBlockCost, maxBlockSize)) {
                      loop(mempoolTxs.tail, blockTxs, lastFeeTx, invalidTxs)
                    } else {
                      current -> invalidTxs
                    }
                }
              case Failure(e) =>
                log.info(s"Not included transaction ${tx.id} due to ${e.getMessage}: ", e)
                loop(mempoolTxs.tail, acc, lastFeeTx, invalidTxs :+ tx.id)
            }
          }
        case None => // mempool is empty
          current -> invalidTxs
      }
    }

    val res = loop(transactions, Seq.empty, None, Seq.empty)
    log.debug(
      s"Collected ${res._1.length} transactions for block #$currentHeight, " +
        s"invalid transaction ids (total:${res._2.length}) for block #$currentHeight : ${res._2}")

    res
  }

  /** Checks that transaction "tx" is not spending outputs spent already by transactions "txs" */
  def doublespend(txs: Seq[ErgoTransaction], tx: ErgoTransaction): Boolean = {
    val txsInputs = txs.flatMap(_.inputs.map(_.boxId))
    tx.inputs.exists(i => txsInputs.exists(_.sameElements(i.boxId)))
  }

  /**
    * Derives header without pow from [[CandidateBlock]].
    */
  def deriveUnprovenHeader(candidate: CandidateBlock): HeaderWithoutPow = {
    val (parentId, height) = derivedHeaderFields(candidate.parentOpt)
    val transactionsRoot =
      BlockTransactions.transactionsRoot(candidate.transactions, candidate.version)
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
    val blockTransactions = BlockTransactions(header.id, candidate.version, candidate.transactions)
    val extension = Extension(header.id, candidate.extension.fields)
    new ErgoFullBlock(header, blockTransactions, extension, Some(adProofs))
  }

}
