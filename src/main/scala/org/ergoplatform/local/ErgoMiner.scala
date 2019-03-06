package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, PoisonPill, Props}
import io.circe.Encoder
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform._
import org.ergoplatform.mining.CandidateBlock
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.PoPowAlgos._
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoInterpreter
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, ErgoMemPoolReader}
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, ErgoStateContext, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Constants, ErgoSettings, Parameters}
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class ErgoMiner(ergoSettings: ErgoSettings,
                viewHolderRef: ActorRef,
                readersHolderRef: ActorRef,
                timeProvider: NetworkTimeProvider,
                inSecretKeyOpt: Option[DLogProverInput]) extends Actor with ScorexLogging {

  import ErgoMiner._

  private val votingSettings = ergoSettings.chainSettings.voting
  private val votingEpochLength = votingSettings.votingLength
  private val protocolVersion = ergoSettings.chainSettings.protocolVersion

  // shared mutable state
  private var isMining = false
  private var candidateOpt: Option[CandidateBlock] = None
  private val miningThreads: mutable.Buffer[ActorRef] = new ArrayBuffer[ActorRef]()
  // cost of a transaction collecting emission box
  // TODO calculate current value every time? Current value is 41010 but it may change with miner votes
  private val EmissionTxCost: Long = 50000

  private var secretKeyOpt: Option[DLogProverInput] = inSecretKeyOpt

  override def preStart(): Unit = {
    if (secretKeyOpt.isEmpty) {
      val callback = self
      viewHolderRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Unit] { v =>
        v.vault.firstSecret().onComplete(_.foreach(r => callback ! UpdateSecret(r)))
      }
    }
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def postStop(): Unit = {
    log.warn("Stopping miner's threads.")
    miningThreads.foreach(_ ! PoisonPill)
    miningThreads.clear()
  }

  private def unknownMessage: Receive = {
    case m =>
      log.warn(s"Unexpected message $m of class: ${m.getClass}")
  }

  private def onUpdateSecret: Receive = {
    case UpdateSecret(s) =>
      secretKeyOpt = Some(s)
  }

  private def miningStatus: Receive = {
    case MiningStatusRequest =>
      sender ! MiningStatusResponse(isMining, candidateOpt)
  }

  private def startMining: Receive = {
    case StartMining if candidateOpt.nonEmpty && !isMining && ergoSettings.nodeSettings.mining =>
      candidateOpt.foreach { candidate =>
        secretKeyOpt match {
          case Some(sk) =>
            log.info("Starting Mining")
            isMining = true
            miningThreads += ErgoMiningThread(ergoSettings, viewHolderRef, candidate, sk.w, timeProvider)(context)
            miningThreads.foreach(_ ! candidate)
          case None =>
            log.warn("Got start mining command while secret key is not ready")
        }
      }
    case StartMining if candidateOpt.isEmpty =>
      requestCandidate()
      context.system.scheduler.scheduleOnce(1.seconds, self, StartMining)(context.system.dispatcher)
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

  override def receive: Receive = receiveSemanticallySuccessfulModifier orElse
    miningStatus orElse
    startMining orElse
    onReaders orElse
    onUpdateSecret orElse
    unknownMessage

  private def onReaders: Receive = {
    case Readers(h, s, m, _) if s.isInstanceOf[UtxoStateReader] =>
      secretKeyOpt.map(_.publicImage).foreach { minerProp =>
        createCandidate(minerProp, h, m, s.asInstanceOf[UtxoStateReader]) match {
          case Success(candidate) => procCandidateBlock(candidate)
          case Failure(e) => log.warn("Failed to produce candidate block.", e)
        }
      }
  }

  private def procCandidateBlock(c: CandidateBlock): Unit = {
    log.debug(s"Got candidate block at height ${ErgoHistory.heightOf(c.parentOpt) + 1}" +
      s" with ${c.transactions.size} transactions")
    candidateOpt = Some(c)
    miningThreads.foreach(_ ! c)
  }

  private def createCandidate(minerPk: ProveDlog,
                              history: ErgoHistoryReader,
                              pool: ErgoMemPoolReader,
                              state: UtxoStateReader): Try[CandidateBlock] = Try {
    val bestHeaderOpt: Option[Header] = history.bestFullBlockOpt.map(_.header)
    val timestamp = timeProvider.time()
    val stateContext = state.stateContext
    val nBits: Long = bestHeaderOpt
      .map(parent => history.requiredDifficultyAfter(parent))
      .map(d => RequiredDifficulty.encodeCompactBits(d))
      .getOrElse(Constants.InitialNBits)
    val interlinks = bestHeaderOpt
      .flatMap { h =>
        history.typedModifierById[Extension](h.extensionId)
          .flatMap(ext => unpackInterlinks(ext.fields).toOption)
          .map(updateInterlinks(h, _))
      }
      .getOrElse(Seq.empty)

    val (extensionCandidate, votes: Array[Byte], version: Byte) = bestHeaderOpt.map { header =>
      val newHeight = header.height + 1
      val currentParams = stateContext.currentParameters
      val betterVersion = protocolVersion > header.version
      val votingFinishHeight: Option[Height] = currentParams.softForkStartingHeight
        .map(_ + votingSettings.votingLength * votingSettings.softForkEpochs)
      val forkVotingAllowed = votingFinishHeight.forall(fh => newHeight < fh)
      val forkOrdered = ergoSettings.votingTargets.getOrElse(Parameters.SoftFork, 0) != 0
      val voteForFork = betterVersion && forkOrdered && forkVotingAllowed

      if (newHeight % votingEpochLength == 0 && newHeight > 0) {
        val newParams = currentParams.update(newHeight, voteForFork, stateContext.votingData.epochVotes, votingSettings)
        (newParams.toExtensionCandidate(packInterlinks(interlinks)),
          newParams.suggestVotes(ergoSettings.votingTargets, voteForFork),
          newParams.blockVersion)
      } else {
        (ExtensionCandidate(packInterlinks(interlinks)),
          currentParams.vote(ergoSettings.votingTargets, stateContext.votingData.epochVotes, voteForFork),
          currentParams.blockVersion)
      }
    }.getOrElse((ExtensionCandidate(packInterlinks(interlinks)), Array(0: Byte, 0: Byte, 0: Byte), Header.CurrentVersion))

    val upcomingContext = state.stateContext.upcoming(minerPk.h, timestamp, nBits, votes, version,
      ergoSettings.chainSettings.powScheme)
    //only transactions valid from against the current utxo state we take from the mem pool
    val emissionTxOpt = ErgoMiner.collectEmission(state, minerPk, ergoSettings.chainSettings.emissionRules).map(_ -> EmissionTxCost)

    val txs = ErgoMiner.collectTxs(minerPk,
      state.stateContext.currentParameters.maxBlockCost,
      state.stateContext.currentParameters.maxBlockSize,
      state,
      upcomingContext,
      pool.getAllPrioritized,
      emissionTxOpt.toSeq)

    state.proofsForTransactions(txs).map { case (adProof, adDigest) =>
      CandidateBlock(bestHeaderOpt, version, nBits, adDigest, adProof, txs, timestamp, extensionCandidate, votes)
    }
  }.flatten

  def requestCandidate(): Unit = readersHolderRef ! GetReaders

}


object ErgoMiner extends ScorexLogging {

  //TODO move ErgoMiner to mining package and make `collectTxs` and `fixTxsConflicts` private[mining]

  def collectEmission(state: UtxoStateReader,
                      minerPk: ProveDlog,
                      emission: EmissionRules): Option[ErgoTransaction] = {
    collectRewards(state.emissionBoxOpt, state.stateContext.currentHeight, Seq.empty, minerPk, emission, Seq.empty)
      .headOption
  }


  def collectFees(currentHeight: Int,
                  txs: Seq[ErgoTransaction],
                  minerPk: ProveDlog,
                  emission: EmissionRules): Option[ErgoTransaction] = {
    collectRewards(None, currentHeight, txs, minerPk, emission, Seq.empty).headOption
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
                     assets: Seq[(TokenId, Long)] = Seq()): Seq[ErgoTransaction] = {
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
      val newEmissionBox: ErgoBoxCandidate = new ErgoBoxCandidate(emissionBox.value - emissionAmount, prop,
        nextHeight, Seq(), Map())
      val inputs = IndexedSeq(new Input(emissionBox.id, ProverResult(Array.emptyByteArray, ContextExtension.empty)))

      val minerBox = new ErgoBoxCandidate(emissionAmount, minerProp, nextHeight, assets, Map())

      ErgoTransaction(
        inputs,
        IndexedSeq(newEmissionBox, minerBox)
      )
    }
    val feeTxOpt: Option[ErgoTransaction] = if (feeBoxes.nonEmpty) {
      val feeAmount = feeBoxes.map(_.value).sum
      val feeAssets = feeBoxes.flatMap(_.additionalTokens).take(ErgoBox.MaxTokens - 1)
      val inputs = feeBoxes.map(b => new Input(b.id, ProverResult(Array.emptyByteArray, ContextExtension.empty)))
      val minerBox = new ErgoBoxCandidate(feeAmount, minerProp, nextHeight, feeAssets, Map())
      Some(ErgoTransaction(inputs.toIndexedSeq, IndexedSeq(minerBox)))
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
    */
  def collectTxs(minerPk: ProveDlog,
                 maxBlockCost: Long,
                 maxBlockSize: Long,
                 us: UtxoStateReader,
                 upcomingContext: ErgoStateContext,
                 mempoolTxsIn: Iterable[ErgoTransaction],
                 startTransactions: Seq[(ErgoTransaction, Long)]): Seq[ErgoTransaction] = {
    def correctLimits(blockTxs: Seq[(ErgoTransaction, Long)]): Boolean = {
      blockTxs.map(_._2).sum < maxBlockCost && blockTxs.map(_._1.size).sum < maxBlockSize
    }

    @tailrec
    def loop(mempoolTxs: Iterable[ErgoTransaction],
             acc: Seq[(ErgoTransaction, Long)],
             lastFeeTx: Option[(ErgoTransaction, Long)]): Seq[ErgoTransaction] = {
      // transactions from mempool and fee txs from the previous step
      def current = (acc ++ lastFeeTx).map(_._1)
      mempoolTxs.headOption match {
        case Some(tx) =>
          implicit val verifier: ErgoInterpreter = ErgoInterpreter(us.stateContext.currentParameters)
          // check validity and calculate transaction cost
          us.validateWithCost(tx) match {
            case Success(costConsumed) =>
              val newTxs = fixTxsConflicts((tx, costConsumed) +: acc)
              val newBoxes = newTxs.flatMap(_._1.outputs)
              val emissionRules = us.constants.settings.chainSettings.emissionRules

              ErgoMiner.collectFees(us.stateContext.currentHeight, newTxs.map(_._1), minerPk, emissionRules) match {
                case Some(feeTx) =>
                  val boxesToSpend = feeTx.inputs.flatMap(i => newBoxes.find(b => java.util.Arrays.equals(b.id, i.boxId)))
                  feeTx.statefulValidity(boxesToSpend, upcomingContext) match {
                    case Success(cost) =>
                      val blockTxs: Seq[(ErgoTransaction, Long)] = (feeTx -> cost) +: newTxs
                      if (correctLimits(blockTxs)) loop(mempoolTxs.tail, newTxs, Some(feeTx -> cost)) else current
                    case _ => // fee collecting tx is invalid, return current
                      current
                  }
                case None =>
                  log.debug(s"No fee proposition found in txs ${newTxs.map(_._1.id)} ")
                  val blockTxs: Seq[(ErgoTransaction, Long)] = newTxs ++ lastFeeTx.toSeq
                  if (correctLimits(blockTxs)) loop(mempoolTxs.tail, blockTxs, lastFeeTx) else current
              }
            case _ =>
              loop(mempoolTxs.tail, acc, lastFeeTx)
          }
        case _ => // mempool is empty
          current
      }
    }

    loop(mempoolTxsIn, startTransactions, None)
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

  case object MiningStatusRequest

  case class UpdateSecret(s: DLogProverInput)

  case class MiningStatusResponse(isMining: Boolean, candidateBlock: Option[CandidateBlock])

  implicit val jsonEncoder: Encoder[MiningStatusResponse] = { r: MiningStatusResponse =>
    Map(
      "isMining" -> r.isMining.asJson,
      "candidateBlock" -> r.candidateBlock.asJson
    ).asJson
  }

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
