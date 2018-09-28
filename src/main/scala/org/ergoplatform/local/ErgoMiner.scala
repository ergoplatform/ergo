package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, PoisonPill, Props}
import io.circe.Encoder
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.{BoxId, R4, TokenId}
import org.ergoplatform._
import org.ergoplatform.mining.CandidateBlock
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ExtensionCandidate, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, ErgoMemPoolReader}
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.{ErgoWallet, P2PKAddress}
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings, Parameters}
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import sigmastate.SBoolean
import sigmastate.Values.{LongConstant, TrueLeaf, Value}
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
                extPropOpt: Option[Value[SBoolean.type]]) extends Actor with ScorexLogging {

  import ErgoMiner._

  //shared mutable state
  private var isMining = false
  private var candidateOpt: Option[CandidateBlock] = None
  private val miningThreads: mutable.Buffer[ActorRef] = new ArrayBuffer[ActorRef]()
  // cost of a regular transaction with one proveDlog input
  private val ExpectedTxCost: Int = 10000
  // size of a regular transaction with input and 2 outputs.
  private val ExpectedTxSize: Int = 150

  private var publicKeyOpt: Option[P2PKAddress] = None

  private def minerPropOpt: Option[Value[SBoolean.type]] = extPropOpt.orElse {
    publicKeyOpt.map(_.pubkey)
  }

  override def preStart(): Unit = {
    viewHolderRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Unit] { v =>
      v.vault.randomPublicKey().onComplete { keyTry =>
        publicKeyOpt = keyTry.toOption
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

  private def miningStatus: Receive = {
    case MiningStatusRequest =>
      sender ! MiningStatusResponse(isMining, candidateOpt)
  }

  private def startMining: Receive = {
    case StartMining if candidateOpt.nonEmpty && !isMining && ergoSettings.nodeSettings.mining =>
      candidateOpt.foreach { candidate =>
        log.info("Starting Mining")
        isMining = true
        miningThreads += ErgoMiningThread(ergoSettings, viewHolderRef, candidate, timeProvider)(context)
        miningThreads.foreach(_ ! candidate)
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
    unknownMessage

  private def onReaders: Receive = {
    case Readers(h, s, m, _) if s.isInstanceOf[UtxoStateReader] =>
      minerPropOpt.foreach { minerProp =>
        createCandidate(minerProp, h, m, s.asInstanceOf[UtxoStateReader]) match {
          case Success(candidate) => procCandidateBlock(candidate)
          case Failure(e) => log.warn("Failed to produce candidate block.", e)
        }
      }
  }

  private def procCandidateBlock(c: CandidateBlock): Unit = {
    log.debug(s"Got candidate block at height ${c.parentOpt.map(_.height).getOrElse(-1) + 1}" +
      s" with ${c.transactions.size} transactions")
    candidateOpt = Some(c)
    miningThreads.foreach(_ ! c)
  }

  /**
    * Return subsequence of valid non-conflicting transactions from `mempoolTxs`
    * with total cost, that does not exceeds `remainingCost`
    * total size, that does not exceeds `remainingSize`
    * and that does not try to spend any of `idsToExclude`
    */
  @tailrec
  private def collectTxs(state: UtxoStateReader,
                         idsToExclude: Seq[BoxId],
                         mempoolTxs: Iterable[ErgoTransaction],
                         remainingCost: Long,
                         remainingSize: Long,
                         acc: Seq[ErgoTransaction]): Seq[ErgoTransaction] = {
    mempoolTxs.headOption match {
      case Some(tx) if remainingCost > ExpectedTxCost && remainingSize > ExpectedTxSize =>
        Try {
          // check, that transaction does not try to spend `idsToExclude`
          require(!idsToExclude.exists(id => tx.inputs.exists(box => java.util.Arrays.equals(box.boxId, id))))
        }.flatMap { _ =>
          // check validity and calculate transaction cost
          tx.statefulValidity(tx.inputs.flatMap(i => state.boxById(i.boxId)), state.stateContext, ergoSettings.metadata)
        } match {
          case Success(costConsumed) if remainingCost > costConsumed && remainingSize > tx.size =>
            // valid transaction with small enough computations
            collectTxs(state, idsToExclude, mempoolTxs.tail, remainingCost - costConsumed, remainingSize - tx.size,
              tx +: acc)
          case _ =>
            // incorrect or too expensive transaction
            collectTxs(state, idsToExclude, mempoolTxs.tail, remainingCost, remainingSize, acc)
        }
      case _ =>
        // enough transactions collected, exclude conflicting transactions and return
        fixTxsConflicts(acc)
    }
  }

  private def createCandidate(minerProp: Value[SBoolean.type],
                              history: ErgoHistoryReader,
                              pool: ErgoMemPoolReader,
                              state: UtxoStateReader): Try[CandidateBlock] = Try {
    val bestHeaderOpt: Option[Header] = history.bestFullBlockOpt.map(_.header)

    //only transactions valid from against the current utxo state we take from the mem pool
    val txsNoConflict = collectTxs(state,
      state.emissionBoxOpt.map(_.id).toSeq,
      pool.unconfirmed.values,
      Parameters.MaxBlockCost - Constants.CoinbaseTxCost,
      Parameters.MaxBlockSize,
      Seq())

    val feeBoxes: Seq[ErgoBox] = ErgoState.boxChanges(txsNoConflict)._2.filter(_.proposition == TrueLeaf)
    val coinbase = ErgoMiner.createCoinbase(state, feeBoxes, minerProp, ergoSettings.emission)
    val txs = txsNoConflict :+ coinbase

    state.proofsForTransactions(txs).map { case (adProof, adDigest) =>
      val timestamp = timeProvider.time()
      val nBits: Long = bestHeaderOpt
        .map(parent => history.requiredDifficultyAfter(parent))
        .map(d => RequiredDifficulty.encodeCompactBits(d))
        .getOrElse(Constants.InitialNBits)

      // todo fill with interlinks and other useful values after nodes update
      val extensionCandidate = ExtensionCandidate(Seq(), Seq())

      CandidateBlock(bestHeaderOpt, nBits, adDigest, adProof, txs, timestamp, extensionCandidate)
    }
  }.flatten

  def requestCandidate(): Unit = readersHolderRef ! GetReaders
}


object ErgoMiner extends ScorexLogging {

  def createCoinbase(state: UtxoStateReader,
                     feeBoxes: Seq[ErgoBox],
                     minerProp: Value[SBoolean.type],
                     emissionRules: EmissionRules): ErgoTransaction = {
    val emissionBoxOpt = state.emissionBoxOpt
    emissionBoxOpt foreach { emissionBox =>
      assert(state.boxById(emissionBox.id).isDefined, s"Emission box ${Algos.encode(emissionBox.id)} missed")
    }
    createCoinbase(emissionBoxOpt, state.stateContext.height, feeBoxes, minerProp, emissionRules)
  }

  def createCoinbase(emissionBoxOpt: Option[ErgoBox],
                     height: Int,
                     feeBoxes: Seq[ErgoBox],
                     minerProp: Value[SBoolean.type],
                     emission: EmissionRules): ErgoTransaction = {
    feeBoxes.foreach(b => assert(b.proposition == TrueLeaf, s"Trying to create coinbase from protected fee box $b"))

    val (inputBoxes, emissionAmount, newEmissionBoxOpt) = emissionBoxOpt match {
      case Some(emissionBox) =>
        val prop = emissionBox.proposition
        val emissionAmount = emission.emissionAtHeight(height)
        val newEmissionBox: ErgoBoxCandidate =
          new ErgoBoxCandidate(emissionBox.value - emissionAmount, prop, Seq(), Map(R4 -> LongConstant(height)))

        ((emissionBox +: feeBoxes).toIndexedSeq, emissionAmount, Some(newEmissionBox))
      case None => (feeBoxes, 0L, None)
    }

    val inputs = inputBoxes
      .map(b => new Input(b.id, ProverResult(Array.emptyByteArray, ContextExtension.empty)))
      .toIndexedSeq

    val feeAmount = feeBoxes.map(_.value).sum

    val feeAssets = feeBoxes.flatMap(_.additionalTokens).take(ErgoBox.MaxTokens - 1)

    //todo: a miner is creating a new asset, remove it after playing for a while
    val newAsset: (TokenId, Long) = (Digest32 @@ inputBoxes.head.id) -> 1000
    val minerAssets = feeAssets :+ newAsset

    val minerBox = new ErgoBoxCandidate(emissionAmount + feeAmount, minerProp, minerAssets, Map())

    ErgoTransaction(
      inputs,
      IndexedSeq(newEmissionBoxOpt, Some(minerBox)).flatten
    )
  }

  def fixTxsConflicts(txs: Seq[ErgoTransaction]): Seq[ErgoTransaction] = txs
    .foldLeft((Seq.empty[ErgoTransaction], Set.empty[ByteArrayWrapper])) { case ((s, keys), tx) =>
      val bxsBaw = tx.inputs.map(_.boxId).map(ByteArrayWrapper.apply)
      if (bxsBaw.forall(k => !keys.contains(k)) && bxsBaw.size == bxsBaw.toSet.size) {
        (s :+ tx) -> (keys ++ bxsBaw)
      } else {
        (s, keys)
      }
    }._1


  case object StartMining

  case object MiningStatusRequest

  case class MiningStatusResponse(isMining: Boolean, candidateBlock: Option[CandidateBlock])

  implicit val jsonEncoder: Encoder[MiningStatusResponse] = (r: MiningStatusResponse) =>
    Map(
      "isMining" -> r.isMining.asJson,
      "candidateBlock" -> r.candidateBlock.asJson
    ).asJson

}

object ErgoMinerRef {

  def props(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            timeProvider: NetworkTimeProvider,
            minerPropOpt: Option[Value[SBoolean.type]] = None): Props =
    Props(new ErgoMiner(ergoSettings, viewHolderRef, readersHolderRef, timeProvider, minerPropOpt))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            timeProvider: NetworkTimeProvider,
            minerPropOpt: Option[Value[SBoolean.type]] = None)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, readersHolderRef, timeProvider, minerPropOpt))


  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            timeProvider: NetworkTimeProvider,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, readersHolderRef, timeProvider), name)
}
