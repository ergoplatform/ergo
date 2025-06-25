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
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.network.message.inputblocks.InputBlockTransactionsData
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.EliminateTransactions
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.{LocallyGeneratedInputBlock, LocallyGeneratedOrderingBlock}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Height
import org.ergoplatform.nodeView.history.{ErgoHistoryReader, ErgoHistoryUtils}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, UtxoStateReader}
import org.ergoplatform.settings.{Algos, ErgoSettings, ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.subblocks.InputBlockInfo
import org.ergoplatform.validation.SoftFieldsAccessError
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{AutolykosSolution, ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input, InputSolutionFound, OrderingSolutionFound, SolutionFound, SubBlockAlgos}
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, idToBytes}
import sigma.data.{Digest32Coll, ProveDlog}
import sigma.crypto.CryptoFacade
import sigma.interpreter.ProverResult
import sigma.validation.ReplacedRule
import sigma.{Coll, Colls}

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

  /** Send solved ordering block to processing */
  private def sendOrderingToNodeView(newBlock: ErgoFullBlock,
                                     orderingBlockTransactions: Seq[ErgoTransaction]): Unit = {
    log.info(
      s"New ordering block ${newBlock.id} w. nonce ${Longs.fromByteArray(newBlock.header.powSolution.n)}"
    )
    viewHolderRef ! LocallyGeneratedOrderingBlock(newBlock, orderingBlockTransactions)
  }

  /** Send solved input block to processing */
  private def sendInputToNodeView(sbi: InputBlockInfo, sbt: InputBlockTransactionsData): Unit = {
    log.info(
      s"New input block ${sbi.header.id} w. nonce ${Longs.fromByteArray(sbi.header.powSolution.n)}"
    )
    viewHolderRef ! LocallyGeneratedInputBlock(sbi, sbt)
  }

  override def receive: Receive = {

    // first we need to get Readers to have some initial state to work with
    case Readers(h, s: UtxoStateReader, m, _) =>
      log.info(s"CandidateGenerator initialized")
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
        .subscribe(self, classOf[FullBlockApplied])
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

    /*
     * When new block is applied, either one mined by us or received from peers isn't equal to our candidate's parent,
     * we need to generate new candidate and possibly also discard existing solution if it is also behind
     */
    case FullBlockApplied(header) =>
      log.info(
        s"Preparing new candidate on getting new block at ${header.height}"
      )
      if (needNewCandidate(state.cache, header)) {
        if (needNewSolution(state.solvedBlock, header.id))
          context.become(initialized(state.copy(cache = None, solvedBlock = None)))
        else
          context.become(initialized(state.copy(cache = None)))
        self ! GenerateCandidate(txsToInclude = Seq.empty, reply = false)
      } else {
        context.become(initialized(state))
      }

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
            if (eliminatedTxs.ids.nonEmpty) {
              viewHolderRef ! eliminatedTxs
            }
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

    case sf: SolutionFound
        if state.solvedBlock.isEmpty && state.cache.nonEmpty =>
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
            // todo: account for input blocks
            val cachedCandidate = state.cache.get.candidateBlock
            val newBlock = completeOrderingBlock(cachedCandidate, solution)
            log.info(s"New block mined, header: ${newBlock.header}")
            ergoSettings.chainSettings.powScheme
              .validate(newBlock.header)  // check header PoW only
              .map(_ => newBlock) match {
              case Success(newBlock) =>
                sendOrderingToNodeView(newBlock, cachedCandidate.orderingBlockTransactions)
                context.become(initialized(state.copy(solvedBlock = Some(newBlock))))
                StatusReply.success(())
              case Failure(exception) =>
                log.warn(s"Removing candidate due to invalid block", exception)
                context.become(initialized(state.copy(cache = None)))
                StatusReply.error(
                  new Exception(s"Invalid block mined: ${exception.getMessage}", exception)
                )
            }
          case _: InputSolutionFound =>
            val (sbi, sbt) = completeInputBlock(state.cache.get.candidateBlock, solution)
            if (SubBlockAlgos.powScheme.checkInputBlockPoW(sbi.header)) { // check PoW only
              // todo: finish input block mining API
              log.info(s"Input-block ${sbi.id} mined @ height ${sbi.header.height}!")
              sendInputToNodeView(sbi, sbt)
              context.become(initialized(state.copy(cache = None))) // todo: cache input block ?
              StatusReply.success(())
            } else {
              log.warn(s"Removing candidate due to invalid input block")
              context.become(initialized(state.copy(cache = None)))
              StatusReply.error(
                new Exception(s"Invalid input block! PoW valid: ${SubBlockAlgos.powScheme.checkInputBlockPoW(sbi.header)}")
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

  /** Helper which is checking that inputs of the transaction are not spent */
  private def inputsNotSpent(tx: ErgoTransaction, s: UtxoStateReader): Boolean =
    tx.inputs.forall(inp => s.boxById(inp.boxId).isDefined)

  /**
    * @param txsToInclude - user-provided transactions, to be included into a block (prioritized over mempool's)
    * @return None if chain is not synced or Some of attempt to create candidate
    */
  def generateCandidate(
    h: ErgoHistoryReader,
    s: UtxoStateReader,
    m: ErgoMemPoolReader,
    pk: ProveDlog,
    txsToInclude: Seq[ErgoTransaction],
    ergoSettings: ErgoSettings): Option[Try[(Candidate, EliminateTransactions)]] = {

    // mandatory transactions to include into next block taken from the previous candidate
    val stateWithMandatoryTxs = s.withTransactions(txsToInclude)
    lazy val unspentTxsToInclude = txsToInclude.filter { tx =>
      inputsNotSpent(tx, stateWithMandatoryTxs)
    }

    val stateContext = s.stateContext

    // mempool transactions to include into a block
    lazy val poolTransactions = m.getAllPrioritized

    lazy val emissionTxOpt =
      CandidateGenerator.collectEmission(s, pk, stateContext)

    def chainSynced: Boolean =
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
      val desiredUpdate = if (stateContext.blockVersion == 3) {
        ergoSettings.votingTargets.desiredUpdate.copy(statusUpdates =
          // 1007 is needed to switch off primitive type validation to add Unsigned Big Int support
          // 1008 is needed to switch off non-primitive type validation to add Option & Header types support
          // 1011 is needed to add new methods
          Seq(1011.toShort -> ReplacedRule(1016), 1007.toShort -> ReplacedRule(1017), 1008.toShort -> ReplacedRule(1018)))
      } else {
        ergoSettings.votingTargets.desiredUpdate
      }
      Some(
        createCandidate(
          pk,
          h,
          desiredUpdate,
          s,
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
    * Block formed via createCandidate() should be validated via // todo: ref to validation procedure
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

      val (inputBlockTransactions, orderingTxs, toEliminate) = collectTxs(
        minerPk,
        state.stateContext.currentParameters.maxBlockCost - safeGap,
        state.stateContext.currentParameters.maxBlockSize,
        state,
        upcomingContext,
        newTransactionCandidates
      )

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
      val previousInputBlocksTransactionsValue = Algos.merkleTreeRoot(previousOrderingBlockTransactionIds.map(id => LeafData @@ idToBytes(id)))

      val inputBlockExtCandidate = InputBlockFields.toExtensionFields(parentInputBlockIdOpt, inputBlockTransactionsDigestValue, inputBlockTransactionsDigestValue)

      val extensionCandidate = preExtensionCandidate ++ inputBlockExtCandidate

      val inputBlockFieldsProof = extensionCandidate.proofForInputBlockData.get // todo: .get

      val inputBlockFields = new InputBlockFields(parentInputBlockIdOpt, inputBlockTransactionsDigestValue, previousInputBlocksTransactionsValue, inputBlockFieldsProof)

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
                    votes,
                    inputBlockFields = InputBlockFields.empty, // todo: recheck, likely should be not empty
                    inputBlockTransactions = inputBlockTransactions,
                    fallbackTxs
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
    val feeTxOpt: Option[ErgoTransaction] = if (feeBoxes.nonEmpty) {
      // todo: sub-blocks: fix tx fee collection , old code is commented out below for now
      /*
       import org.ergoplatform.sdk.wallet.Constants.MaxAssetsPerBox
       import sigma.ast.syntax.ErgoBoxRType
       import sigma.Extensions.ArrayOps

       val feeAmount = feeBoxes.map(_.value).sum
       val feeAssets =
        feeBoxes.toArray.toColl.flatMap(_.additionalTokens).take(MaxAssetsPerBox)
       val inputs = feeBoxes.map(b => new Input(b.id, ProverResult.empty))
       val minerBox =
        new ErgoBoxCandidate(feeAmount, minerProp, nextHeight, feeAssets, Map())
       Some(ErgoTransaction(inputs.toIndexedSeq, IndexedSeq(), IndexedSeq(minerBox)))
      */
      None
    } else {
      None
    }

    Seq(emissionTxOpt, feeTxOpt).flatten
  }

  /**
    * Helper function which decides whether transactions can fit into a block with given cost and size limits
    */
  private def correctLimits(
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
    * @return - input block transactions to include, ordering blocks transactions to include, transaction ids turned out to be invalid.
    */
  def collectTxs(
                  minerPk: ProveDlog,
                  maxBlockCost: Int,
                  maxBlockSize: Int,
                  us: UtxoStateReader,
                  upcomingContext: ErgoStateContext,
                  transactions: Seq[ErgoTransaction]
                ): (Seq[ErgoTransaction], Seq[ErgoTransaction], Seq[ModifierId]) = {

    val currentHeight = us.stateContext.currentHeight
    val nextHeight = upcomingContext.currentHeight

    log.info(
      s"Assembling a block candidate for block #$nextHeight from ${transactions.length} transactions available"
    )

    val verifier: ErgoInterpreter = ErgoInterpreter(upcomingContext.currentParameters)

    // @tailrec - todo: fix
    def loop(
              mempoolTxs: Iterable[ErgoTransaction],
              accInput: Seq[CostedTransaction],
              accOrdering: Seq[CostedTransaction],
              lastFeeTx: Option[CostedTransaction],
              invalidTxs: Seq[ModifierId]
            ): (Seq[ErgoTransaction], Seq[ErgoTransaction], Seq[ModifierId]) = {

      val acc = accInput ++ accOrdering
      // transactions from mempool and fee txs from the previous step
      //val currentCosted = acc ++ lastFeeTx

      def currentInput: Seq[ErgoTransaction] = accInput.map(_._1)
      def currentOrdering: Seq[ErgoTransaction] = (accOrdering ++ lastFeeTx).map(_._1)

      val allCurrent = currentInput ++ currentOrdering

      val stateWithTxs = us.withTransactions(allCurrent)

      mempoolTxs.headOption match {
        case Some(tx) =>
          if (!inputsNotSpent(tx, stateWithTxs) || doublespend(allCurrent, tx)) {
            //mark transaction as invalid if it tries to do double-spending or trying to spend outputs not present
            //do these checks before validating the scripts to save time
            log.debug(s"Transaction ${tx.id} double-spending or spending non-existing inputs")
            loop(mempoolTxs.tail, accInput, accOrdering, lastFeeTx, invalidTxs :+ tx.id)
          } else {

            def validateTx(softFieldsAllowed: Boolean): Try[Int] = {
              stateWithTxs.validateWithCost(
                tx,
                upcomingContext,
                maxBlockCost,
                Some(verifier),
                softFieldsAllowed)
            }

            def okTx(costConsumed: Int,
                     inputTx: Boolean): (Seq[ErgoTransaction], Seq[ErgoTransaction], Seq[ModifierId]) = {
              val newTxs = acc :+ (tx -> costConsumed)
              val newBoxes = newTxs.flatMap(_._1.outputs)

              // todo: why to collect fees on each tx?
              collectFees(currentHeight, newTxs.map(_._1), minerPk, upcomingContext) match {
                case Some(feeTx) =>
                  val boxesToSpend = feeTx.inputs.flatMap(i =>
                    newBoxes.find(b => java.util.Arrays.equals(b.id, i.boxId))
                  )
                  feeTx.statefulValidity(boxesToSpend, IndexedSeq(), upcomingContext)(verifier) match {
                    case Success(cost) =>
                      val blockTxs: Seq[CostedTransaction] = (feeTx -> cost) +: newTxs
                      if (correctLimits(blockTxs, maxBlockCost, maxBlockSize)) {
                        if (inputTx) {
                          loop(mempoolTxs.tail, accInput :+ (tx -> costConsumed), accOrdering, Some(feeTx -> cost), invalidTxs)
                        } else {
                          loop(mempoolTxs.tail, accInput, accOrdering :+ (tx -> costConsumed), Some(feeTx -> cost), invalidTxs)
                        }
                      } else {
                        lazy val totalCost = (accOrdering ++ lastFeeTx).map(_._2).sum
                        log.debug(s"Finishing block assembly on limits overflow, " +
                                  s"cost is $totalCost, cost limit: $maxBlockCost")
                        (currentInput, currentOrdering, invalidTxs)
                      }
                    case Failure(e) =>
                      log.warn(
                        s"Fee collecting tx is invalid, not including it, " +
                          s"details: ${e.getMessage} from ${stateWithTxs.stateContext}"
                      )
                      (currentInput, currentOrdering, invalidTxs)
                  }
                case None =>
                  log.info(s"No fee proposition found in txs ${newTxs.map(_._1.id)} ")
                  val blockTxs: Seq[CostedTransaction] = newTxs ++ lastFeeTx.toSeq
                  if (correctLimits(blockTxs, maxBlockCost, maxBlockSize)) {
                    if (inputTx) {
                      loop(mempoolTxs.tail, accInput :+ (tx -> costConsumed), accOrdering, lastFeeTx, invalidTxs)
                    } else {
                      loop(mempoolTxs.tail, accInput, accOrdering :+ (tx -> costConsumed), lastFeeTx, invalidTxs)
                    }
                  } else {
                    (currentInput, currentOrdering, invalidTxs)
                  }
              }
            }

            def failTx(e: Throwable): (Seq[ErgoTransaction], Seq[ErgoTransaction], Seq[ModifierId]) = {
              log.info(s"Not included transaction ${tx.id} due to ${e.getMessage}: ", e)
              loop(mempoolTxs.tail, accInput, accOrdering, lastFeeTx, invalidTxs :+ tx.id)
            }

            // check validity and calculate transaction cost
            validateTx(softFieldsAllowed = false) match {
              case Success(costConsumed) =>
                okTx(costConsumed, inputTx = true)
              case Failure(e) if e.isInstanceOf[SoftFieldsAccessError] =>
                log.info(s"Rechecking transaction: $tx.id")
                validateTx(softFieldsAllowed = true) match {
                  case Success(costConsumed) =>
                    okTx(costConsumed, inputTx = false)
                  case Failure(e) =>
                    failTx(e)
                }
              case Failure(e) =>
                failTx(e)
            }
          }
        case None => // mempool is empty
          (currentInput, currentOrdering, invalidTxs)
      }
    }

    val res = loop(transactions, Seq.empty, Seq.empty, None, Seq.empty)
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
                         solution: AutolykosSolution): (InputBlockInfo, InputBlockTransactionsData) = {

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

    val sbi: InputBlockInfo = InputBlockInfo(InputBlockInfo.initialMessageVersion, header, ibf)
    val sbt : InputBlockTransactionsData = InputBlockTransactionsData(sbi.header.id, txs)

    (sbi, sbt)
  }

}
