package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.pattern.StatusReply
import com.google.common.primitives.Longs
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.mining.AutolykosPowScheme.derivedHeaderFields
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.{Header, HeaderWithoutPow}
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer, UnconfirmedTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.network.message.inputblocks.InputBlockTransactionsData
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.EliminateTransactions
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.{LocallyGeneratedInputBlock, LocallyGeneratedOrderingBlock}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Height
import org.ergoplatform.nodeView.history.{ErgoHistoryReader, ErgoHistoryUtils}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, UtxoStateReader}
import org.ergoplatform.sdk.wallet.Constants.MaxAssetsPerBox
import org.ergoplatform.settings.{Algos, ErgoSettings, ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.validation.SoftFieldsAccessError
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{AutolykosSolution, ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input, InputSolutionFound, OrderingSolutionFound, SolutionFound}
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}
import sigma.data.{Digest32Coll, ProveDlog}
import sigma.Extensions.ArrayOps
import sigma.ast.syntax.ErgoBoxRType
import sigma.crypto.CryptoFacade
import sigma.interpreter.ProverResult
import sigma.validation.ReplacedRule
import sigma.{Coll, Colls}

import scala.collection.mutable.{ArrayBuffer => MutableArray}
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

/** Responsible for generating block candidates and validating solutions.
  * It is observing changes of history, utxo state, mempool and newly applied blocks
  * to generate valid block candidates when it is needed. */
class CandidateGenerator(
  minerPk: ProveDlog,
  readersHolderRef: ActorRef,
  viewHolderRef: ActorRef,
  ergoSettings: ErgoSettings
) extends Actor
  with ScorexLogging {

  import org.ergoplatform.mining.CandidateGenerator._

  /** retrieve Readers once on start and then get updated by events */
  override def preStart(): Unit = {
    log.info("CandidateGenerator is starting")
    readersHolderRef ! GetReaders
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(s"Attempted candidate generator restart due to ${reason.getMessage}", reason)
    super.preRestart(reason, message)
  }

  /** Send solved ordering block to processing */
  private def sendOrderingToNodeView(newBlock: ErgoFullBlock,
                                     orderingBlockTransactions: Seq[ErgoTransaction]): Unit = {
    log.info(
      s"New ordering block ${newBlock.id} w. nonce ${Longs.fromByteArray(newBlock.header.powSolution.n)}"
    )
    // Immediately announce newly mined block to network BEFORE local application.
    // This reduces propagation latency by avoiding the wait for NodeViewHolder
    // to fully validate and apply the block.
    // Ordering block announcement to peers supporting input blocks is sent separately
    // on LocallyGeneratedOrderingBlock below.
    context.system.eventStream.publish(NewBlockMined(newBlock.header))

    viewHolderRef ! LocallyGeneratedOrderingBlock(newBlock, orderingBlockTransactions)
  }

  /** Send solved input block to processing */
  private def sendInputToNodeView(sbi: InputBlockAnnouncement, sbt: InputBlockTransactionsData): Unit = {
    log.info(
      s"New input block ${sbi.header.id} w. nonce ${Longs.fromByteArray(sbi.header.powSolution.n)}"
    )
    viewHolderRef ! LocallyGeneratedInputBlock(sbi, sbt)
  }

  /**
    * Reaction on invalidation of the block solved by us (e.g. due to a transaction which became
    * invalid after the block candidate was generated): drop the solved block along with cached
    * candidates. Mining will resume on the next external request, which will generate a fresh
    * candidate because the cached one was dropped.
    */
  private def onSolvedBlockFailed(state: CandidateGeneratorState, modId: ModifierId, error: Throwable): Unit = {
    state.solvedBlock.filter(_.toSeq.exists(_.id == modId)).foreach { block =>
      log.warn(
        s"Locally mined block ${block.id} invalidated by the node view holder, resuming mining",
        error
      )
      context.become(
        initialized(state.copy(cachedCandidate = None, cachedPreviousCandidate = None, solvedBlock = None))
      )
    }
  }

  override def receive: Receive = {

    // first we need to get Readers to have some initial state to work with
    case Readers(h, s: UtxoStateReader, m, _) =>
      log.info(s"CandidateGenerator initialized")
      context.become(
        initialized(
          CandidateGeneratorState(
            cachedCandidate       = None,
            cachedPreviousCandidate = None,
            solvedBlock = None,
            hr = h,
            sr = s,
            mpr = m,
            avgGenTime = 1000.millis,
            lastAppliedBlockTxs = None
          )
        )
      )
      self ! GenerateCandidate(txsToInclude = Seq.empty, reply = false, forced = false, optPk = None)
      context.system.eventStream
        .subscribe(self, classOf[FullBlockApplied])
      context.system.eventStream.subscribe(self, classOf[NodeViewChange])
      context.system.eventStream.subscribe(self, classOf[SemanticallyFailedModification])
      context.system.eventStream.subscribe(self, classOf[SyntacticallyFailedModification])
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

    /*
     * When new block is applied, either one mined by us or received from peers isn't equal to our candidate's parent,
     * we need to generate new candidate and possibly also discard existing solution if it is also behind
     */
    case applied: FullBlockApplied =>
      val header = applied.header
      log.info(
        s"Preparing new candidate on getting new block at ${header.height}"
      )
      val stateWithAppliedTxs =
        state.copy(lastAppliedBlockTxs = Some(header.id -> applied.txIds.toSet))
      if (needNewCandidate(state.cachedCandidate, header)) {
        if (needNewSolution(state.solvedBlock, header.id))
          context.become(initialized(stateWithAppliedTxs.copy(cachedCandidate = None, cachedPreviousCandidate = None, solvedBlock = None)))
        else
          context.become(initialized(stateWithAppliedTxs.copy(cachedCandidate = None, cachedPreviousCandidate = None)))
        self ! GenerateCandidate(txsToInclude = Seq.empty, reply = false, forced = false, optPk = None)
      } else {
        context.become(initialized(stateWithAppliedTxs))
      }

    /*
     * If a block solved by us was invalidated by the node view holder, we need to drop it along
     * with cached candidates, as otherwise mining would stall (new solutions are rejected with
     * "Block already solved" and candidate regeneration is paused while solvedBlock is set).
     */
    case SemanticallyFailedModification(_, modId, error) =>
      onSolvedBlockFailed(state, modId, error)

    case SyntacticallyFailedModification(_, modId, error) =>
      onSolvedBlockFailed(state, modId, error)

    case gen @ GenerateCandidate(txsToInclude, reply, forced, optPk) =>
      val senderOpt = if (reply) Some(sender()) else None
      val effectiveMinerPk = optPk.getOrElse(minerPk)
      val selectedInputBlockId = state.hr.bestBlocks._2.map(_.id)
      lazy val selectedInputTransactionsDigest = Algos.merkleTreeRoot(
        state.hr.getBestOrderingCollectedInputBlocksTransactions().map(tx => LeafData @@ tx.serializedId))
      if (!forced && cachedFor(state.cachedCandidate, txsToInclude, effectiveMinerPk,
        selectedInputBlockId, selectedInputTransactionsDigest)) {
        senderOpt.foreach(_ ! StatusReply.success(state.cachedCandidate.get))
      } else {
        val start = System.currentTimeMillis()
        CandidateGenerator.generateCandidate(
          state.hr,
          state.sr,
          state.mpr,
          effectiveMinerPk,
          txsToInclude,
          state.lastAppliedBlockTxs,
          ergoSettings
        ) match {
          case Some(Failure(ex)) =>
            log.error(s"Candidate generation failed", ex)
            senderOpt.foreach(
              _ ! StatusReply.error(s"Candidate generation failed : ${ex.getMessage}")
            )
          case Some(Success((candidate, eliminatedTxs))) =>
            if (eliminatedTxs.ids.nonEmpty) {
              viewHolderRef ! eliminatedTxs
            }
            val generationTook = System.currentTimeMillis() - start
            log.info(s"Generated new candidate in $generationTook ms")
            context.become(
              initialized(
                state.copy(cachedCandidate = Some(candidate), cachedPreviousCandidate = state.cachedCandidate, avgGenTime = generationTook.millis)
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

    case sf: SolutionFound
        if state.solvedBlock.isEmpty && state.cachedCandidate.nonEmpty =>
      // Inject node pk if it is not externally set (in Autolykos 2)
      val preSolution = sf.as
      val solution =
        if (CryptoFacade.isInfinityPoint(preSolution.pk)) {
          new AutolykosSolution(minerPk.value, preSolution.w, preSolution.n, preSolution.d)
        } else {
          preSolution
        }
      val result: StatusReply[Unit] = {
        sf match {
          case _: OrderingSolutionFound =>
            // Try to complete the current candidate first; if the solution does not fit it
            // (e.g. a new block arrived while we were mining), fall back to the previous candidate
            val candidateAndBlock = state.cachedCandidate
              .map(c => c -> completeOrderingBlock(c.candidateBlock, solution))
              .filter { case (_, block) =>
                ergoSettings.chainSettings.powScheme.validate(block.header).isSuccess
              }
              .orElse {
                log.info(s"Using previous candidate as a solution: ${state.cachedPreviousCandidate}")
                state.cachedPreviousCandidate.map(c => c -> completeOrderingBlock(c.candidateBlock, solution))
              }

            candidateAndBlock match {
              case Some((sourceCandidate, block)) =>
                log.info(s"New block mined, header: ${block.header}")
                sendOrderingToNodeView(block, sourceCandidate.candidateBlock.orderingBlockTransactions)
                context.become(initialized(state.copy(solvedBlock = Some(block))))
                StatusReply.success(())
              case None =>
                log.warn(s"Removing candidates due to invalid block")
                context.become(initialized(state.copy(cachedCandidate = None, cachedPreviousCandidate = None)))
                StatusReply.error(
                  new Exception(s"Invalid block mined: no candidate matches the solution")
                )
            }
          case _: InputSolutionFound =>
            val cachedCandidate = state.cachedCandidate.get
            val (sbi, sbt) = completeInputBlock(cachedCandidate.candidateBlock, solution)
            val parameters = cachedCandidate.parameters
            val powValid = ergoSettings.chainSettings.powScheme.checkInputBlockPoW(sbi.header, parameters)
            if (powValid) { // check PoW only
              log.info(s"Input-block ${sbi.id} mined @ height ${sbi.header.height}!")
              sendInputToNodeView(sbi, sbt)
              context.become(initialized(state.copy(cachedCandidate = None))) // todo: cache input block ?
              StatusReply.success(())
            } else {
              log.warn(s"Removing candidate due to invalid input block")
              context.become(initialized(state.copy(cachedCandidate = None)))
              StatusReply.error(
                new Exception(s"Invalid input block! PoW valid: $powValid")
              )
            }
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
    * @param parameters      - blockchain parameters at the time of candidate creation
    */
  case class Candidate(
    candidateBlock: CandidateBlock,
    externalVersion: WorkMessage,
    txsToInclude: Seq[ErgoTransaction],
    parameters: Parameters
  )

  case class GenerateCandidate(
    txsToInclude: Seq[ErgoTransaction],
    reply: Boolean,
    forced: Boolean,
    optPk: Option[ProveDlog] = None
  )

  /** Local state of candidate generator to avoid mutable vars */
  case class CandidateGeneratorState(
    cachedCandidate: Option[Candidate],
    cachedPreviousCandidate: Option[Candidate],
    solvedBlock: Option[ErgoFullBlock],
    hr: ErgoHistoryReader,
    sr: UtxoStateReader,
    mpr: ErgoMemPoolReader,
    avgGenTime: FiniteDuration, // approximation of average block generation time for more efficient retries
    lastAppliedBlockTxs: Option[(ModifierId, Set[ModifierId])] // header id and tx ids of the last applied block
  )

  def apply(
    minerPk: ProveDlog,
    readersHolderRef: ActorRef,
    viewHolderRef: ActorRef,
    ergoSettings: ErgoSettings
  )(implicit context: ActorRefFactory): ActorRef =
    context.actorOf(
      Props(
        new CandidateGenerator(
          minerPk,
          readersHolderRef,
          viewHolderRef,
          ergoSettings
        )
      ).withDispatcher("critical-dispatcher"),
      s"CandidateGenerator-${Random.alphanumeric.take(5).mkString}"
    )

  /**
   * Checks the request, miner key, selected input tip and processed transaction prefix.
   * A peer can advance these without this miner solving a local input block, and a body
   * can extend the processed prefix after its announcement already updated the tip.
   *
   * Note: candidate cache is a single slot keyed by `minerPk`. If multiple miner public keys
   * are used concurrently (e.g. node’s own miner and external `/mining/candidateWithTxsAndPk`
   * callers), each different `minerPk` will evict the previous cached candidate and trigger
   * full candidate generation (mempool packing + state proofs). This endpoint assumes a single
   * active miner public key at a time for optimal performance.
   */
  def cachedFor(
    candidateOpt: Option[Candidate],
    txs: Seq[ErgoTransaction],
    minerPk: ProveDlog,
    selectedInputBlockId: Option[ModifierId] = None,
    selectedInputTransactionsDigest: Digest32 = Algos.emptyMerkleTreeRoot
  ): Boolean = {
    candidateOpt.isDefined && candidateOpt.exists { c =>
      c.externalVersion.pk == minerPk &&
        c.candidateBlock.inputBlockFields.prevInputBlockId.map(bytesToId) == selectedInputBlockId &&
        (selectedInputBlockId.isEmpty || java.util.Arrays.equals(
          c.candidateBlock.inputBlockFields.prevTransactionsDigest, selectedInputTransactionsDigest)) &&
        (txs.isEmpty || (txs.size == c.txsToInclude.size && txs.forall(
          c.txsToInclude.contains
        )))
    }
  }

  /** we need new candidate if given block is not parent of our cached block */
  def needNewCandidate(
    cache: Option[Candidate],
    bestFullBlockHeader: Header
  ): Boolean = {
    val parentHeaderIdOpt = cache.map(_.candidateBlock).flatMap(_.parentOpt).map(_.id)
    !parentHeaderIdOpt.contains(bestFullBlockHeader.id)
  }

  /** Solution is valid only if bestFullBlock on the chain is its parent */
  def needNewSolution(
    solvedBlock: Option[ErgoFullBlock],
    bestFullBlockId: ModifierId
  ): Boolean = {
    solvedBlock.nonEmpty && !solvedBlock.map(_.parentId).contains(bestFullBlockId)
  }

  /** Regenerate candidate to let new transactions in, miners are polling for candidate in ~ 100ms
    * interval so they switch to it.
    * If blockCandidateGenerationInterval elapsed since last block generation,
    * then new tx in mempool is a reasonable trigger of candidate regeneration
    */
  def hasCandidateExpired(
    cachedCandidate: Option[Candidate],
    solvedBlock: Option[ErgoFullBlock],
    candidateGenInterval: FiniteDuration
   ): Boolean = {
    def candidateAge(c: Candidate): FiniteDuration =
      (System.currentTimeMillis() - c.candidateBlock.timestamp).millis
    // non-empty solved block means we wait for newly mined block to be applied
    if (solvedBlock.isDefined) {
      false
    } else {
      cachedCandidate match {
        // if current candidate is older than candidateGenInterval
        case Some(c) if candidateGenInterval.compare(candidateAge(c)) <= 0 =>
          log.info(s"Regenerating block candidate")
          true
        case _ =>
          false
      }
    }
  }

  /** Helper which is checking that inputs of the transaction are not spent */
  private def inputsNotSpent(tx: ErgoTransaction, s: UtxoStateReader): Boolean =
    tx.inputs.forall(inp => s.boxById(inp.boxId).isDefined)

  /**
    * Checks that the best full block in the history corresponds to the state.
    * Evaluated via live history storage reads, so re-checking it after candidate assembly
    * detects a block applied concurrently with the assembly.
    */
  def isChainSynced(
    bestFullBlockIdOpt: Option[ModifierId],
    stateContext: ErgoStateContext
  ): Boolean =
    bestFullBlockIdOpt == stateContext.lastHeaderOpt.map(_.id)

  /**
    * Filters out from `poolTxs` transactions included into the last applied block
    * (`lastAppliedBlockTxs`), if the block is still the best full block (`bestFullBlockIdOpt`).
    * Such transactions are removed from the mempool by the node view holder itself on block
    * application, so there is no need to validate them during candidate assembly (which logs
    * misleading double-spending messages) nor to eliminate them via EliminateTransactions.
    */
  def excludeAppliedTxs(
    poolTxs: Seq[UnconfirmedTransaction],
    lastAppliedBlockTxs: Option[(ModifierId, Set[ModifierId])],
    bestFullBlockIdOpt: Option[ModifierId]
  ): Seq[UnconfirmedTransaction] = {
    lastAppliedBlockTxs match {
      case Some((appliedHeaderId, appliedTxIds))
          if appliedTxIds.nonEmpty && bestFullBlockIdOpt.contains(appliedHeaderId) =>
        poolTxs.filterNot(tx => appliedTxIds.contains(tx.id))
      case _ =>
        poolTxs
    }
  }

  /**
    * @return None if chain is not synced or Some of attempt to create candidate
    */
  def generateCandidate(
    h: ErgoHistoryReader,
    s: UtxoStateReader,
    m: ErgoMemPoolReader,
    pk: ProveDlog,
    txsToInclude: Seq[ErgoTransaction],
    lastAppliedBlockTxs: Option[(ModifierId, Set[ModifierId])],
    ergoSettings: ErgoSettings
  ): Option[Try[(Candidate, EliminateTransactions)]] = {
    // mandatory transactions to include into next block taken from the previous candidate
    val stateWithMandatoryTxs = s.withTransactions(txsToInclude)
    lazy val unspentTxsToInclude = txsToInclude.filter { tx =>
      inputsNotSpent(tx, stateWithMandatoryTxs)
    }

    val stateContext = s.stateContext

    //only transactions valid from against the current utxo state we take from the mem pool,
    //skipping transactions already included into the last applied block
    lazy val poolTransactions =
      excludeAppliedTxs(m.getAllPrioritized, lastAppliedBlockTxs, h.bestFullBlockOpt.map(_.id))

    lazy val emissionTxOpt =
      CandidateGenerator.collectEmission(s, pk, stateContext)

    def chainSynced =
      isChainSynced(h.bestFullBlockOpt.map(_.id), stateContext)

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
      val desiredUpdate = if (stateContext.blockVersion == 3) {
        ergoSettings.votingTargets.desiredUpdate.copy(statusUpdates =
          // 1007 is needed to switch off primitive type validation to add Unsigned Big Int support
          // 1008 is needed to switch off non-primitive type validation to add Option & Header types support
          // 1011 is needed to add new methods
          Seq(1011.toShort -> ReplacedRule(1016), 1007.toShort -> ReplacedRule(1017), 1008.toShort -> ReplacedRule(1018)))
      } else {
        ergoSettings.votingTargets.desiredUpdate
      }
      val candidateAttempt = createCandidate(
        pk,
        h,
        desiredUpdate,
        s,
        poolTransactions,
        emissionTxOpt,
        unspentTxsToInclude,
        ergoSettings
      )
      if (!chainSynced) {
        log.debug(
          "Discarding block candidate as a new block was applied during its assembly, " +
          "a new candidate will be generated on FullBlockApplied"
        )
        None
      } else {
        Some(candidateAttempt)
      }
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
      nextHeight >= 1561601 // 6.0 voting starting height, first block of epoch #1525
    } else if(ergoSettings.networkType.isTestNet) {
      nextHeight >= 1548800 // testnet voting start height
    } else {
      nextHeight >= 8 // devnet voting start height
    }

    // we automatically vote for 5.0 soft-fork in the mainnet if 120 = 0 vote not provided in settings
    val forkOrdered = if (ergoSettings.networkType.isMainNet && protocolVersion == 2) {
      ergoSettings.votingTargets.softForkOption.getOrElse(1) == 1
    } else {
      ergoSettings.votingTargets.softForkOption.getOrElse(0) == 1
    }

    //todo: remove after 6.0 soft-fork activation
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
    * @param poolTxs                 - memory pool transactions
    * @param emissionTxOpt           - optional emission transaction
    * @param prioritizedTransactions - transactions which are going into the block in the first place
    *                                (before transactions from the pool). No guarantee of inclusion in general case.
    *
    * Block formed via createCandidate() should be validated in the same way as a block coming from outside.
    *
    * @return - block candidate or an error
    */
  def createCandidate(
                       minerPk: ProveDlog,
                       history: ErgoHistoryReader,
                       proposedUpdate: ErgoValidationSettingsUpdate,
                       state: UtxoStateReader,
                       poolTxs: Seq[UnconfirmedTransaction],
                       emissionTxOpt: Option[ErgoTransaction],
                       prioritizedTransactions: Seq[ErgoTransaction],
                       ergoSettings: ErgoSettings
  ): Try[(Candidate, EliminateTransactions)] =
    Try {

      val popowAlgos = new NipopowAlgos(ergoSettings.chainSettings)
      val stateContext = state.stateContext

      // Extract best header and extension of a best block for assembling a new block
      val (bestHeaderOpt, bestInputBlock) = history.bestBlocks
      val bestExtensionOpt: Option[Extension] = bestHeaderOpt
        .flatMap(h => history.typedModifierById[Extension](h.extensionId))

      // Make progress in time since last block.
      // If no progress is made, then, by consensus rules, the block will be rejected.
      val timestamp = Math.max(System.currentTimeMillis(), bestHeaderOpt.map(_.timestamp + 1).getOrElse(0L))

      // Calculate required difficulty for the new block, the same diff for subblock
      val nBits: Long = if (bestInputBlock.isDefined) {
        // just take nbits from previous input block
        bestInputBlock.get.header.nBits // .get is ok as lastSubblockOpt.exists in continueSubblock checks emptiness
      } else {
        bestHeaderOpt
          .map(parent => history.requiredDifficultyAfter(parent))
          .map(d => DifficultySerializer.encodeCompactBits(d))
          .getOrElse(ergoSettings.chainSettings.initialNBits)
      }

      // todo: do not recalculate interlink vector if subblock available

      // Obtain NiPoPoW interlinks vector to pack it into the extension section
      val updInterlinks       = popowAlgos.updateInterlinks(bestHeaderOpt, bestExtensionOpt)
      val interlinksExtension = popowAlgos.interlinksToExtension(updInterlinks)

      // todo: cache votes and version for a header, do not recalculate it each block
      /*
       * Calculate extension candidate without input-block specific fields, votes, and block version
       */

      val (preExtensionCandidate, votes: Array[Byte], version: Byte) = bestHeaderOpt
        .map { header =>
          val votingSettings      = ergoSettings.chainSettings.voting

          val newHeight     = header.height + 1
          val currentParams = stateContext.currentParameters
          val voteForSoftFork = forkOrdered(ergoSettings, currentParams, header)

          if (newHeight % votingSettings.votingLength == 0 && newHeight > 0) {
            // new voting epoch
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
            val votes = currentParams.vote(
              ergoSettings.votingTargets.targets,
              stateContext.votingData.epochVotes,
              voteForSoftFork
            )
            (
              interlinksExtension,
              votes,
              currentParams.blockVersion
            )
          }
        }
        .getOrElse(
          (interlinksExtension, Array(0: Byte, 0: Byte, 0: Byte), Header.InitialVersion)
        )

      // form input block related data
      val parentInputBlockIdOpt = bestInputBlock.map(bestInput => idToBytes(bestInput.id))
      val previousOrderingBlockTransactions = history.getBestOrderingCollectedInputBlocksTransactions()
      val previousOrderingBlockTransactionIds = previousOrderingBlockTransactions.map(_.id)

      /*
      * Forming transactions to get included
      */

      val upcomingContext = state.stateContext.upcoming(
        minerPk.value,
        timestamp,
        nBits,
        votes,
        proposedUpdate,
        version
      )

      // todo: could be removed after 5.0, but we still slowly decreasing it for starters
      // we allow for some gap, to avoid possible problems when different interpreter version can estimate cost
      // differently due to bugs in AOT costing
      val safeGap = if (state.stateContext.currentParameters.maxBlockCost < 1000000) {
        0
      } else if (state.stateContext.currentParameters.maxBlockCost < 5000000) {
        150000
      } else {
        500000
      }

      // new transactions coming from API (prioritizedTransactions), mempool, and also emission transaction
      // to spread to next input and ordering blocks
      // within collectTxs(), transactions from previous input blocks will be accounted in addition to the new txs
      val newTransactionCandidates = emissionTxOpt.toSeq ++ prioritizedTransactions ++ poolTxs.map(_.transaction)

      val (preInputBlockTransactions, orderingTxs, toEliminate) = collectTxs(
        minerPk,
        state.stateContext.currentParameters.maxBlockCost - safeGap,
        state.stateContext.currentParameters.maxBlockSize,
        state,
        upcomingContext,
        newTransactionCandidates,
        previousOrderingBlockTransactions
      )

      // filter out transactions included in previous input-blocks
      // todo: clear them from mempool on new best input block / add to mempool on input blocks chain forking
      val inputBlockTransactions = preInputBlockTransactions.filterNot(tx => previousOrderingBlockTransactionIds.contains(tx.id))

      val eliminateTransactions = EliminateTransactions(toEliminate)

      if (previousOrderingBlockTransactionIds.size + orderingTxs.size == 0) {
        throw new IllegalArgumentException(
          s"Proofs for 0 txs cannot be generated : " +
            s"previousOrderingBlockTransactionIds: ${previousOrderingBlockTransactionIds}, " +
            s"emissionTx: ${emissionTxOpt.isDefined}, " +
            s"priorityTxs: ${prioritizedTransactions.size}, " +
            s"poolTxs: ${poolTxs.size}"
        )
      }

      /*
       * Put input block related fields into extension section of block candidate
       */

      // digest (Merkle tree root) of new first-class transactions since last input-block
      val inputBlockTransactionsDigestValue = Algos.merkleTreeRoot(inputBlockTransactions.map(tx => LeafData @@ tx.serializedId))

      // digest (Merkle tree root) first class transactions since ordering block till last input-block
      val previousInputBlocksTransactionsDigest = Algos.merkleTreeRoot(previousOrderingBlockTransactionIds.map(id => LeafData @@ idToBytes(id)))

      val inputBlockExtCandidate = InputBlockFields.toExtensionFields(parentInputBlockIdOpt, inputBlockTransactionsDigestValue, inputBlockTransactionsDigestValue)

      val extensionCandidate = preExtensionCandidate ++ inputBlockExtCandidate

      val inputBlockFields = extensionCandidate.proofForInputBlockData match {
        case Some(inputBlockFieldsProof) =>
          new InputBlockFields(parentInputBlockIdOpt, inputBlockTransactionsDigestValue, previousInputBlocksTransactionsDigest, inputBlockFieldsProof)
        case None =>
          throw new IllegalArgumentException("Input block fields proof not available in extension candidate")
      }

      def deriveWorkMessage(block: CandidateBlock) = {
        ergoSettings.chainSettings.powScheme.deriveExternalCandidate(
          block,
          minerPk,
          prioritizedTransactions.map(_.id)
        )
      }

      val txs = previousOrderingBlockTransactions ++ orderingTxs

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
            votes,
            inputBlockFields,
            inputBlockTransactions,
            orderingTxs
          )
          val ext = deriveWorkMessage(candidate)
          log.info(
            s"Got candidate block at height ${ErgoHistoryUtils.heightOf(candidate.parentOpt) + 1}" +
            s" with ${candidate.transactions.size} transactions, msg ${Base16.encode(ext.msg)}"
          )
          Success(
            Candidate(candidate, ext, prioritizedTransactions, upcomingContext.currentParameters) -> eliminateTransactions
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
                    votes,
                    inputBlockFields = InputBlockFields.empty, // todo: recheck, likely should be not empty
                    inputBlockTransactions = inputBlockTransactions,
                    fallbackTxs
                  )
                  Candidate(
                    candidate,
                    deriveWorkMessage(candidate),
                    prioritizedTransactions,
                    upcomingContext.currentParameters
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

  val MaxFeeBoxesPerTransaction: Int = 100

  def collectFees(
    currentHeight: Int,
    txs: Seq[ErgoTransaction],
    minerPk: ProveDlog,
    stateContext: ErgoStateContext
  ): Seq[ErgoTransaction] = {
    collectRewards(None, currentHeight, txs, minerPk, stateContext, Colls.emptyColl)
  }

  /**
    * Generate at most one emission transaction, followed by fee transactions with at most
    * MaxFeeBoxesPerTransaction inputs each, preserving the complete fee payout.
    */
  def collectRewards(
    emissionBoxOpt: Option[ErgoBox],
    currentHeight: Int,
    txs: Seq[ErgoTransaction],
    minerPk: ProveDlog,
    stateContext: ErgoStateContext,
    assets: Coll[(TokenId, Long)] = Colls.emptyColl
  ): Seq[ErgoTransaction] = {
    val chainSettings = stateContext.chainSettings
    val propositionBytes = chainSettings.monetary.feePropositionBytes
    val emission = chainSettings.emissionRules

    // forming transaction collecting emission
    val reemissionSettings = chainSettings.reemission
    val reemissionRules = reemissionSettings.reemissionRules

    val eip27ActivationHeight = reemissionSettings.activationHeight
    val reemissionTokenId = Digest32Coll @@ reemissionSettings.reemissionTokenIdBytes

    val nextHeight = currentHeight + 1
    val minerProp =
      ErgoTreePredef.rewardOutputScript(emission.settings.minerRewardDelay, minerPk)

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
    val feeTxs = feeBoxes.grouped(MaxFeeBoxesPerTransaction).map { chunk =>
      val feeAmount = chunk.map(_.value).sum
      val feeAssets = chunk.toArray.toColl.flatMap(_.additionalTokens).take(MaxAssetsPerBox)
      val feeInputs = chunk.map(b => new Input(b.id, ProverResult.empty))
      val minerBox = new ErgoBoxCandidate(feeAmount, minerProp, nextHeight, feeAssets, Map())
      ErgoTransaction(feeInputs.toIndexedSeq, IndexedSeq.empty, IndexedSeq(minerBox))
    }.toSeq

    emissionTxOpt.toSeq ++ feeTxs
  }

  /**
    * Collects valid non-conflicting transactions from `mandatoryTxs` and then `mempoolTxsIn` and adds a transaction
    * collecting fees from them to `minerPk`.
    *
    * Resulting transactions total cost does not exceed `maxBlockCost`, total size does not exceed `maxBlockSize`,
    * and the miner's transaction is correct.
    *
    * The accepted input-block prefix supplies common state to both candidate payloads. Its transactions
    * are not returned again; its collectible fees belong only to the ordering payload. Each new input
    * payload must also leave the accumulated prefix within the conservative ordering budget.
    * Prefix-only rewards may remain unclaimed when the mandatory transactions consume the budget.
    *
    * @return - input block transactions to include, ordering blocks transactions to include, transaction ids turned out to be invalid.
    */
  def collectTxs(
                  minerPk: ProveDlog,
                  maxBlockCost: Int,
                  maxBlockSize: Int,
                  us: UtxoStateReader,
                  upcomingContext: ErgoStateContext,
                  transactions: Seq[ErgoTransaction],
                  inputBlockTransactions: Seq[ErgoTransaction] = Seq.empty
                ): (Seq[ErgoTransaction], Seq[ErgoTransaction], Seq[ModifierId]) = {

    val currentHeight = us.stateContext.currentHeight
    val nextHeight = upcomingContext.currentHeight
    // Use the candidate header version, including the first block of an activation epoch.
    val inputBlocksEnabled = upcomingContext.sigmaPreHeader.version >= Header.Interpreter60Version

    log.info(
      s"Assembling a block candidate for block #$nextHeight from ${transactions.length} transactions available"
    )

    val verifier: ErgoInterpreter = ErgoInterpreter(upcomingContext.currentParameters)

    val transactionSizes = scala.collection.mutable.Map.empty[(Header.Version, ModifierId, ModifierId), Int]
    val feeCosts = scala.collection.mutable.Map.empty[ModifierId, Int]

    def unsignedSize(value: Long): Int = {
      var remaining = value
      var size = 1
      while (remaining >= 128L) {
        remaining = remaining >>> 7
        size += 1
      }
      size
    }

    def correctLimits(blockTxs: Seq[CostedTransaction], costLimit: Long,
                      sizeLimit: Long, blockVersion: Header.Version): Boolean = {
      if (blockTxs.map(_._2.toLong).sum > costLimit) false
      else if (blockTxs.isEmpty) true
      else {
        // Match BlockTransactionsSerializer, including its version marker and count.
        val framing = 32L + unsignedSize(blockTxs.size.toLong) +
          (if (blockVersion > Header.InitialVersion)
            unsignedSize(BlockTransactionsSerializer.MaxTransactionsInBlock.toLong + blockVersion) else 0)
        val bytes = blockTxs.map { case (tx, _) =>
          val key = (blockVersion, tx.id, bytesToId(tx.witnessSerializedId))
          transactionSizes.getOrElseUpdate(key, {
            if (blockVersion >= sigma.VersionContext.V6SoftForkVersion) {
              sigma.VersionContext.withVersions(blockVersion, blockVersion) {
                ErgoTransactionSerializer.toBytes(tx).length
              }
            } else ErgoTransactionSerializer.toBytes(tx).length
          }).toLong
        }.sum
        framing + bytes <= sizeLimit
      }
    }

    // Mutable state for iterative transaction processing
    val version = upcomingContext.sigmaPreHeader.version
    val prefixIds = inputBlockTransactions.map(_.id).toSet
    val prefixState = us.withTransactions(inputBlockTransactions)
    val prefix = inputBlockTransactions.map { tx =>
      val cost = prefixState.validateWithCost(tx, upcomingContext,
        upcomingContext.currentParameters.maxBlockCost, Some(verifier), softFieldsAllowed = true).get
      tx -> cost
    }
    require(correctLimits(prefix, upcomingContext.currentParameters.maxBlockCost, maxBlockSize, version),
      "Selected input transactions exceed the ordering block limits")
    // A mandatory prefix can consume the conservative mining cost gap. Keep that valid prefix,
    // but give optional transactions no additional cost allowance in that case.
    val orderingCostLimit = math.max(maxBlockCost.toLong, prefix.map(_._2.toLong).sum)
    var remainingTxs = transactions.filterNot(tx => prefixIds.contains(tx.id))
    val accInput = MutableArray.empty[CostedTransaction]
    val accOrdering = MutableArray.empty[CostedTransaction]
    val deferredOutputs = scala.collection.mutable.Set.empty[ModifierId]
    var lastFeeTxs = Seq.empty[CostedTransaction]
    val invalidTxs = MutableArray.empty[ModifierId]
    var done = false

    def feesFor(ordering: Seq[CostedTransaction]): Option[Seq[CostedTransaction]] = {
      val sources = inputBlockTransactions ++ ordering.map(_._1)
      val boxes = sources.flatMap(_.outputs).map(b => bytesToId(b.id) -> b).toMap
      val costedFees = collectFees(currentHeight, sources, minerPk, upcomingContext)
        .foldLeft(Try(Vector.empty[CostedTransaction])) { (result, feeTx) =>
          result.flatMap { accumulated =>
            val checkedCost = feeCosts.get(feeTx.id) match {
              case Some(cost) => Success(cost)
              case None =>
                val inputs = feeTx.inputs.flatMap(i => boxes.get(bytesToId(i.boxId)))
                feeTx.statefulValidity(inputs, IndexedSeq.empty, upcomingContext)(verifier).map { cost =>
                  feeCosts.put(feeTx.id, cost)
                  cost
                }
            }
            checkedCost.map(cost => accumulated :+ (feeTx -> cost))
          }
        }
      costedFees match {
        case Success(allFees) =>
          val newOutputIds = ordering.flatMap(_._1.outputs).map(b => bytesToId(b.id)).toSet
          val (required, optional) = allFees.partition { case (tx, _) =>
            tx.inputs.exists(i => newOutputIds.contains(bytesToId(i.boxId)))
          }
          val base = prefix ++ ordering
          if (!correctLimits(base ++ required, orderingCostLimit, maxBlockSize, version)) {
            None
          } else {
            var selected: Seq[CostedTransaction] = required
            // Already selected input transactions remain mandatory. Optional rewards must not
            // prevent their ordering confirmation when only part of the fee collection fits.
            optional.iterator.takeWhile { feeTx =>
              if (correctLimits(base ++ selected :+ feeTx, orderingCostLimit, maxBlockSize, version)) {
                selected = selected :+ feeTx
                true
              } else false
            }.foreach(_ => ())
            Some(selected)
          }
        case Failure(error) =>
          log.warn(s"Fee collection is not selectable: ${error.getMessage}")
          None
      }
    }

    lastFeeTxs = feesFor(Seq.empty).getOrElse(Seq.empty)

    while (!done) {
      def currentInput: Seq[ErgoTransaction] = accInput.map(_._1)
      def currentOrdering: Seq[ErgoTransaction] = (accOrdering ++ lastFeeTxs).map(_._1)
      // Generated fee rewards can change as selection proceeds, so their outputs are not
      // stable dependencies for user transactions in this candidate.
      val feeOutputIds = lastFeeTxs.flatMap(_._1.outputs).map(b => bytesToId(b.id)).toSet
      val allCurrent = inputBlockTransactions ++ currentInput ++ accOrdering.map(_._1)
      val stateWithTxs = us.withTransactions(allCurrent)

      remainingTxs.headOption match {
        case Some(tx) =>
          def dependsOn(outputIds: collection.Set[ModifierId]): Boolean =
            tx.inputs.exists(i => outputIds.contains(bytesToId(i.boxId))) ||
              tx.dataInputs.exists(i => outputIds.contains(bytesToId(i.boxId)))

          def deferTx(): Unit = {
            deferredOutputs ++= tx.outputs.map(b => bytesToId(b.id))
            remainingTxs = remainingTxs.tail
          }

          if (dependsOn(deferredOutputs) || dependsOn(feeOutputIds)) {
            // Preserve descendants for a later candidate when their dependencies are available.
            deferTx()
          } else if (!inputsNotSpent(tx, stateWithTxs) || doublespend(allCurrent, tx)) {
            // Mark transaction as invalid if it tries to do double-spending or trying to spend outputs not present
            // Do these checks before validating the scripts to save time
            log.debug(s"Transaction ${tx.id} double-spending or spending non-existing inputs")
            invalidTxs += tx.id
            remainingTxs = remainingTxs.tail
          } else {

            def validateTx(softFieldsAllowed: Boolean): Try[Int] = {
              stateWithTxs.validateWithCost(
                tx,
                upcomingContext,
                maxBlockCost,
                Some(verifier),
                softFieldsAllowed)
            }

            def collectFeeAndCheckLimits(inputTx: Boolean,
                                         costConsumed: Int): Boolean = {
              val otherPartition = if (inputTx) currentOrdering else currentInput
              val otherOutputs = otherPartition.flatMap(_.outputs).map(b => bytesToId(b.id)).toSet
              if (dependsOn(otherOutputs)) {
                // Each payload must be executable without the other candidate's new outputs.
                // Deferral is a selection decision, not evidence that the transaction is invalid.
                deferTx()
                return true
              }
              val costed = tx -> costConsumed
              if (inputTx) {
                if (correctLimits(accInput :+ costed, maxBlockCost, maxBlockSize, version) &&
                  correctLimits(prefix ++ accInput :+ costed,
                    math.min(orderingCostLimit, upcomingContext.currentParameters.maxBlockCost.toLong),
                    maxBlockSize, version)) {
                  accInput += costed
                  remainingTxs = remainingTxs.tail
                  true
                } else {
                  done = true
                  false
                }
              } else {
                feesFor(accOrdering :+ costed) match {
                  case Some(fees) =>
                    accOrdering += costed
                    lastFeeTxs = fees
                    remainingTxs = remainingTxs.tail
                    true
                  case None =>
                    done = true
                    false
                }
              }
            }

            def failTx(e: Throwable): Unit = {
              log.info(s"Not included transaction ${tx.id} due to ${e.getMessage}: ", e)
              invalidTxs += tx.id
              remainingTxs = remainingTxs.tail
            }

            // Check validity and calculate transaction cost
            validateTx(softFieldsAllowed = !inputBlocksEnabled) match {
              case Success(costConsumed) =>
                collectFeeAndCheckLimits(inputTx = inputBlocksEnabled, costConsumed)
              case Failure(e) if inputBlocksEnabled && e.isInstanceOf[SoftFieldsAccessError] =>
                log.info(s"Rechecking transaction: $tx.id")
                validateTx(softFieldsAllowed = true) match {
                  case Success(costConsumed) =>
                    collectFeeAndCheckLimits(inputTx = false, costConsumed)
                  case Failure(e) =>
                    failTx(e)
                }
              case Failure(e) =>
                failTx(e)
            }
          }
        case None => // mempool is empty
          done = true
      }
    }

    val res = (accInput.map(_._1), (accOrdering ++ lastFeeTxs).map(_._1), invalidTxs)
    val feeProposition = upcomingContext.chainSettings.monetary.feePropositionBytes
    val unclaimed = ErgoState.newBoxes(inputBlockTransactions ++ res._2)
      .count(b => java.util.Arrays.equals(b.propositionBytes, feeProposition))
    if (unclaimed > 0) {
      log.warn(s"Ordering candidate leaves $unclaimed fee boxes unclaimed; they remain in the UTXO set")
    }
    log.debug(
      s"Collected ${res._1.length} input and ${res._2.length} ordering transactions for block #$currentHeight, " +
        s"invalid transaction ids (total:${res._3.length}): ${res._3}")

    res
  }

  /** Checks that transaction "tx" is not spending outputs spent already by transactions "txs" */
  def doublespend(txs: Seq[ErgoTransaction], tx: ErgoTransaction): Boolean = {
    val txsInputs = txs.flatMap(_.inputs.map(_.boxId))
    tx.inputs.exists(i => txsInputs.exists(_.sameElements(i.boxId)))
  }

  /**
    * Derives header without pow from a block candidate provided
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
      candidate.votes,
      Array.emptyByteArray
    )
  }

  /**
    * Assemble `ErgoFullBlock` using candidate block and provided pow solution.
    */
  def completeOrderingBlock(candidate: CandidateBlock, solution: AutolykosSolution): ErgoFullBlock = {
    val header = deriveUnprovenHeader(candidate).toHeader(solution, None)
    val adProofs = ADProofs(header.id, candidate.adProofBytes)
    val blockTransactions = BlockTransactions(header.id, candidate.version, candidate.transactions)
    val extension = Extension(header.id, candidate.extension.fields)
    new ErgoFullBlock(header, blockTransactions, extension, Some(adProofs))
  }

  def completeInputBlock(candidate: CandidateBlock,
                         solution: AutolykosSolution): (InputBlockAnnouncement, InputBlockTransactionsData) = {

    val header = deriveUnprovenHeader(candidate).toHeader(solution, None)
    val txs = candidate.inputBlockTransactions

    // todo: check links?
    // todo: update candidate generator state
    val prevInputBlockId: Option[Array[Byte]] = candidate.inputBlockFields.prevInputBlockId

    // todo: add
    val inputBlockTransactionsDigest: Digest32 = candidate.inputBlockFields.transactionsDigest
    val prevTransactionsDigest: Digest32 = candidate.inputBlockFields.prevTransactionsDigest
    val merkleProof: BatchMerkleProof[Digest32] = candidate.inputBlockFields.inputBlockFieldsProof

    val ibf = new InputBlockFields(prevInputBlockId, inputBlockTransactionsDigest, prevTransactionsDigest, merkleProof)

    val weakIds = txs.map(_.weakId)

    val sbi: InputBlockAnnouncement = InputBlockAnnouncement(InputBlockAnnouncement.initialMessageVersion, header, ibf, Some(weakIds))
    val sbt : InputBlockTransactionsData = InputBlockTransactionsData(sbi.header.id, txs)

    (sbi, sbt)
  }

}
