package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props, Stash}
import org.ergoplatform.mining.CandidateGenerator.GenerateCandidate
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.DigestState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class ErgoMiner(
  ergoSettings: ErgoSettings,
  viewHolderRef: ActorRef,
  readersHolderRef: ActorRef,
  timeProvider: NetworkTimeProvider,
  secretKeyOpt: Option[DLogProverInput]
) extends Actor
  with Stash
  with ScorexLogging {

  import org.ergoplatform.mining.ErgoMiner._

  override def preStart(): Unit = {
    if (!ergoSettings.nodeSettings.mining) {
      log.error("Mining is disabled")
    } else if (secretKeyOpt.isEmpty && !ergoSettings.nodeSettings.useExternalMiner) {
      log.info(
        "Trying to use secret key from wallet for mining, wallet must be unlocked."
      )
      self ! QueryWallet(secret = true)
    } else { // mining pubKey is needed in both mining modes
      ergoSettings.miningPubKey match {
        case Some(pk) =>
          log.info(s"Using public key from settings")
          onStart(secretKeyOpt, pk)
        case None =>
          log.info("Trying to use public key from wallet for mining")
          self ! QueryWallet(secret = false)
      }
    }
  }

  // initialize miner state with secrets and candidate generator
  private def onStart(
    secretKeyOpt: Option[DLogProverInput],
    publicKey: ProveDlog
  ): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
    val candidateGeneratorRef = CandidateGenerator(
      publicKey,
      readersHolderRef,
      viewHolderRef,
      timeProvider,
      ergoSettings
    )
    context.become(starting(MinerState(secretKeyOpt, publicKey, candidateGeneratorRef)))
    unstashAll() // due to StartMining message from ErgoApp
  }

  // at first try to obtain secrets and public key as we cannot mine without it
  override def receive: Receive = {
    case QueryWallet(secret) =>
      viewHolderRef ! GetDataFromCurrentView[
        ErgoHistory,
        DigestState,
        ErgoWallet,
        ErgoMemPool,
        Unit
      ] { v =>
        if (secret) {
          v.vault.firstSecret.onComplete {
            _.flatten match {
              case Failure(t) =>
                log.warn(s"Miner can't load secret key from wallet: ${t.getMessage} ")
                context.system.scheduler.scheduleOnce(
                  4.seconds,
                  self,
                  QueryWallet(secret)
                )(context.system.dispatcher)
              case Success(proverInput: DLogProverInput) =>
                self ! UpdateSecret(proverInput)
            }
          }
        } else {
          v.vault.miningPubkey.onComplete {
            case Failure(t) =>
              log.warn(s"Miner can't load public key from wallet: ${t.getMessage} ")
              context.system.scheduler.scheduleOnce(4.seconds, self, QueryWallet(secret))(
                context.system.dispatcher
              )
            case Success(None) =>
              log.info(s"Miner is waiting for wallet initialization")
              context.system.scheduler.scheduleOnce(4.seconds, self, QueryWallet(secret))(
                context.system.dispatcher
              )
            case Success(Some(miningPubKey)) =>
              self ! UpdatePublicKey(miningPubKey)
          }
        }
      }
    case UpdateSecret(s) =>
      log.info("Setting secret and public key")
      onStart(secretKeyOpt = Some(s), publicKey = s.publicImage)
    case UpdatePublicKey(pk) =>
      log.info("Setting public key")
      onStart(secretKeyOpt = secretKeyOpt, publicKey = pk)
    case _: scala.runtime.BoxedUnit =>
    // ignore, this message is caused by way of interaction with NodeViewHolder.
    case _ => // stashing all messages until miner is initialized, like StartMining message from ErgoApp
      stash()
  }

  /** We need to ignore all historical blocks, mining is triggered by latest blocks only */
  private def shouldStartMine(b: ErgoFullBlock): Boolean =
    b.header.isNew(timeProvider, ergoSettings.chainSettings.blockInterval * 2)

  /** Let's wait for a signal to start mining, either from ErgoApp or when a latest blocks get applied to blockchain */
  def starting(minerState: MinerState): Receive = {
    case StartMining
        if minerState.secretKeyOpt.isDefined || ergoSettings.nodeSettings.useExternalMiner =>
      if (!ergoSettings.nodeSettings.useExternalMiner && ergoSettings.nodeSettings.internalMinersCount != 0) {
        log.info(
          s"Starting ${ergoSettings.nodeSettings.internalMinersCount} native miner(s)"
        )
        (1 to ergoSettings.nodeSettings.internalMinersCount) foreach { _ =>
          ErgoMiningThread(
            ergoSettings,
            minerState.candidateGeneratorRef,
            minerState.secretKeyOpt.get.w
          )(context)
        }
      }
      context.system.eventStream
        .unsubscribe(self, classOf[SemanticallySuccessfulModifier[_]])
      context.become(started(minerState))

    case StartMining =>
      // unexpected, we made sure that either external mining is used or secret key is set at this state for internal mining
      log.error(s"Unexpected state of missing secret key for internal mining")

    /**
      * Non obvious but case when mining is enabled, but miner isn't started yet. Initialization case.
      * We've received block that been generated by somebody else or genesis while we doesn't start.
      * And this block was generated after our miner had been started. That means that we are ready
      * to start mining.
      * This block could be either genesis or generated by another node.
      */
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock) if shouldStartMine(mod) =>
      log.info("Starting mining triggered by incoming block")
      self ! StartMining

    /**
      * Just ignore all other modifiers.
      */
    case SemanticallySuccessfulModifier(_) =>
  }

  /** Bridge between external miner and CandidateGenerator (Internal mining threads are talking to CandidateGenerator directly.)
    * The reason is that replying is optional and it is not possible to obtain a sender reference from MiningApiRoute 'ask'.
    */
  def started(minerState: MinerState): Receive = {
    case genCandidate@GenerateCandidate(_, _) =>
      minerState.candidateGeneratorRef forward genCandidate

    case solution: AutolykosSolution =>
      minerState.candidateGeneratorRef forward solution

    case ReadMinerPk => // used in /mining/rewardAddress API method
      sender() ! minerState.publicKey // PK is always set when we get to 'started` state

    case m =>
      log.warn(s"Unexpected message $m of class: ${m.getClass}")

  }
}

object ErgoMiner extends ScorexLogging {
  case class QueryWallet(secret: Boolean)
  case class UpdateSecret(s: DLogProverInput)
  case class UpdatePublicKey(pk: ProveDlog)
  case object ReadMinerPk
  case object StartMining

  case class MinerState(
    secretKeyOpt: Option[DLogProverInput], // first secret from wallet for internal miner
    publicKey: ProveDlog, // "miningPubkeyHex" setting in config has preference over wallet's secret key,
    candidateGeneratorRef: ActorRef,
    prepareCandidateRetryDelay: FiniteDuration = 100.millis, // duration to wait before new prepare candidate attempt (adjusted based on feedback from previous execution)
    solvedBlock: Option[Header]                = None // we cache it as it is a signal for competing miners that they are too late
  )

  def apply(
    ergoSettings: ErgoSettings,
    viewHolderRef: ActorRef,
    readersHolderRef: ActorRef,
    timeProvider: NetworkTimeProvider,
    skOpt: Option[DLogProverInput] = None
  )(implicit context: ActorRefFactory): ActorRef =
    context.actorOf(
      Props(
        new ErgoMiner(ergoSettings, viewHolderRef, readersHolderRef, timeProvider, skOpt)
      )
    )
}
