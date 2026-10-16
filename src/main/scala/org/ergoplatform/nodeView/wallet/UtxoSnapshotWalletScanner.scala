package org.ergoplatform.nodeView.wallet

import akka.actor.{Actor, ActorRef, Props, Timers}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import org.ergoplatform.{ErgoBox, GlobalConstants}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Height
import org.ergoplatform.nodeView.history.storage.modifierprocessors.{UtxoSnapshotScanSource, UtxoSnapshotScanSourceReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.{UtxoSnapshotChunkIntegrityException, UtxoSnapshotScanStatus}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.crypto.authds.avltree.batch.Constants.DigestType
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverSubtree, ProxyInternalNode}
import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverLeaf, ProverNodes}
import scorex.util.{ModifierId, ScorexLogging}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

private[wallet] class UtxoSnapshotWalletScanner(walletActor: ActorRef,
                                                settings: ErgoSettings,
                                                sourceReader: UtxoSnapshotScanSourceReader)
  extends Actor with Timers with ScorexLogging {

  import UtxoSnapshotWalletScanner._

  private implicit val timeout: Timeout = Timeout(5.minutes)
  private implicit val callbackDispatcher = context.dispatcher
  private val snapshotDispatcher = context.system.dispatchers.lookup(GlobalConstants.IndexerDispatcher)

  private var generation: Long = 0L
  private var activeRun: Option[UtxoSnapshotScanRun] = None
  private var activeSource: Option[UtxoSnapshotScanSource] = None
  private var terminalFailures: Set[(Height, ModifierId)] = Set.empty

  override def receive: Receive = {
    case StartUtxoSnapshotScan(run, forceRestart) =>
      val snapshot = run.snapshotHeight -> run.snapshotBlockId
      if (!forceRestart && activeRun.exists(_.hasSnapshot(run.snapshotHeight, run.snapshotBlockId))) {
        log.debug(s"UTXO snapshot wallet scan is already running for height ${run.snapshotHeight}")
      } else if (!forceRestart && activeRun.nonEmpty) {
        log.debug("Ignoring UTXO snapshot wallet scan while another run is active")
      } else if (!forceRestart && terminalFailures.contains(snapshot)) {
        log.debug(s"UTXO snapshot wallet scan is stopped after terminal failure at height ${run.snapshotHeight}")
      } else if (!settings.nodeSettings.utxoSettings.utxoBootstrap) {
        log.debug("Ignoring UTXO snapshot wallet scan request because utxoBootstrap is disabled")
      } else {
        generation += 1
        timers.cancel(RetryTimerKey)
        terminalFailures -= snapshot
        activeRun = Some(run)
        activeSource = None
        begin(generation, run, attempt = 0)
      }

    case AbortUtxoSnapshotScan(run) =>
      if (activeRun.contains(run)) {
        generation += 1
        timers.cancel(RetryTimerKey)
        activeRun = None
        activeSource = None
        terminalFailures += run.snapshotHeight -> run.snapshotBlockId
      }

    case Initialized(id, run, attempt, Success((source, status))) if isCurrent(id, run) =>
      activeSource = Some(source)
      if (status.completed) {
        activeRun = None
        activeSource = None
        log.info(s"UTXO snapshot wallet scan is already completed at height ${status.snapshotHeight}")
      } else {
        lastCommittedBatchStatus(status) match {
          case Success(Some(checkpointStatus)) =>
            readBatch(id, run, attempt, source, checkpointStatus)
          case Success(None) =>
            readBatch(id, run, attempt, source, status)
          case Failure(t) =>
            retryOrFail(id, run, attempt, t)
        }
      }

    case Initialized(id, run, attempt, Failure(t)) if isCurrent(id, run) =>
      retryOrFail(id, run, attempt, t)

    case BatchRead(id, run, attempt, Success(batch)) if isCurrent(id, run) =>
      val applied = (walletActor ? ApplyUtxoSnapshotScanBatch(
          run,
          batch.subtreeIndex,
          batch.nextSubtreeIndex,
          batch.completed,
          batch.boxes
        )).mapTo[Try[UtxoSnapshotScanStatus]].recover { case t => Failure(t) }
      applied.map(Applied(id, run, attempt, _)).pipeTo(self)

    case BatchRead(id, run, attempt, Failure(t)) if isCurrent(id, run) =>
      retryOrFail(id, run, attempt, t)

    case Applied(id, run, _, Success(status)) if isCurrent(id, run) =>
      if (status.completed) {
        activeRun = None
        activeSource = None
        log.info(s"UTXO snapshot wallet scan completed at height ${status.snapshotHeight}")
      } else {
        activeSource match {
          case Some(source) => readBatch(id, run, attempt = 0, source, status)
          case None => retryOrFail(id, run, attempt = 0,
            new IllegalStateException("Validated UTXO snapshot scan source is unavailable"))
        }
      }

    case Applied(_, run, _, Failure(t: UtxoSnapshotChunkIntegrityException))
      if activeRun.contains(run) =>
      timers.cancel(RetryTimerKey)
      activeRun = None
      activeSource = None
      terminalFailures += run.snapshotHeight -> run.snapshotBlockId
      log.error("UTXO snapshot wallet scan stopped after durable frontier integrity failure", t)

    case Applied(id, run, attempt, Failure(t)) if isCurrent(id, run) =>
      retryOrFail(id, run, attempt, t)

    case RetryScan(id, run, attempt) if isCurrent(id, run) =>
      begin(id, run, attempt)

    case _: Initialized | _: BatchRead | _: Applied | _: RetryScan =>
      log.debug("Ignoring stale UTXO snapshot wallet scan message")
  }

  private def isCurrent(id: Long, run: UtxoSnapshotScanRun): Boolean =
    id == generation && activeRun.contains(run)

  private def begin(id: Long,
                     run: UtxoSnapshotScanRun,
                     attempt: Int): Unit = {
    val sourceAndCount = Future {
      Try(sourceReader.readUtxoSnapshotScanSource(run.snapshotBlockId)).flatten.flatMap { source =>
        if (source.snapshotHeight != run.snapshotHeight) {
          Failure(new IllegalStateException(
            s"Persisted UTXO snapshot height ${source.snapshotHeight} does not match expected ${run.snapshotHeight}"))
        } else {
          Success(source)
        }
      }
    }(snapshotDispatcher)
    val initialized = sourceAndCount.flatMap {
      case Success(source) =>
        (walletActor ? GetOrInitUtxoSnapshotScanStatus(
          run,
          source.manifestDepth.toInt,
          source.partCount
        )).mapTo[Try[UtxoSnapshotScanStatus]]
          .map(_.map(source -> _))
          .recover { case t => Failure(t) }
      case Failure(t) => Future.successful(Failure(t))
    }
    initialized.map(Initialized(id, run, attempt, _)).recover {
      case t => Initialized(id, run, attempt, Failure(t))
    }.pipeTo(self)
  }

  private def readBatch(id: Long,
                         run: UtxoSnapshotScanRun,
                         attempt: Int,
                         source: UtxoSnapshotScanSource,
                         status: UtxoSnapshotScanStatus): Unit = {
    Future(readSnapshotBatch(sourceReader, source, status))(snapshotDispatcher)
      .map(BatchRead(id, run, attempt, _))
      .recover { case t => BatchRead(id, run, attempt, Failure(t)) }
      .pipeTo(self)
  }

  private def retryOrFail(id: Long,
                          run: UtxoSnapshotScanRun,
                          attempt: Int,
                          cause: Throwable): Unit = {
    activeRun match {
      case Some(current) if current == run && attempt < MaxRetryAttempts =>
        val nextAttempt = attempt + 1
        log.warn(s"UTXO snapshot wallet scan attempt failed; retrying ($nextAttempt/$MaxRetryAttempts)", cause)
        timers.startSingleTimer(RetryTimerKey, RetryScan(id, run, nextAttempt), RetryDelay)
      case Some(current) if current == run =>
        activeRun = None
        activeSource = None
        terminalFailures += run.snapshotHeight -> run.snapshotBlockId
        val message = s"UTXO snapshot wallet scan failed after ${attempt + 1} attempts: ${cause.getMessage}"
        log.error(message, cause)
        walletActor ! UtxoSnapshotScanTerminated(run, message)
      case _ =>
        log.debug("Ignoring UTXO snapshot wallet scan failure after the run stopped")
    }
  }
}

private[wallet] final case class UtxoSnapshotFinalizationPlan(state: UtxoSnapshotFinalizationState,
                                                               scheduleCatchUp: Boolean,
                                                               tryCleanup: Boolean)

private[wallet] final case class UtxoSnapshotFinalizationState(catchUpScheduled: Set[ModifierId],
                                                                cleanupCompleted: Set[ModifierId],
                                                                sourceCleanupStarted: Set[ModifierId]) {
  def plan(status: UtxoSnapshotScanStatus,
           catchUpReady: Boolean): UtxoSnapshotFinalizationPlan = {
    require(status.completed, "Only completed UTXO snapshot scans can be finalized")
    val id = status.snapshotBlockId
    val schedule = catchUpReady && !catchUpScheduled.contains(id)
    val updatedCatchUp = if (catchUpReady) catchUpScheduled + id else catchUpScheduled
    UtxoSnapshotFinalizationPlan(
      copy(catchUpScheduled = updatedCatchUp),
      schedule,
      updatedCatchUp.contains(id) && !cleanupCompleted.contains(id))
  }

  def cleanupSucceeded(snapshotBlockId: ModifierId): UtxoSnapshotFinalizationState =
    copy(
      cleanupCompleted = cleanupCompleted + snapshotBlockId,
      sourceCleanupStarted = sourceCleanupStarted - snapshotBlockId)

  def sourceCleanupRetryExhausted(snapshotBlockId: ModifierId): UtxoSnapshotFinalizationState =
    copy(sourceCleanupStarted = sourceCleanupStarted - snapshotBlockId)

  def claimSourceCleanup(snapshotBlockId: ModifierId): (UtxoSnapshotFinalizationState, Boolean) = {
    val shouldStart = !sourceCleanupStarted.contains(snapshotBlockId) &&
      !cleanupCompleted.contains(snapshotBlockId)
    val next = if (shouldStart) {
      copy(sourceCleanupStarted = sourceCleanupStarted + snapshotBlockId)
    } else {
      this
    }
    next -> shouldStart
  }

  def isSourceCleanupStarted(snapshotBlockId: ModifierId): Boolean =
    sourceCleanupStarted.contains(snapshotBlockId)

  def catchUpFailed(snapshotBlockId: ModifierId): UtxoSnapshotFinalizationState =
    copy(catchUpScheduled = catchUpScheduled - snapshotBlockId)

  def catchUpCompleted(snapshotBlockId: ModifierId): UtxoSnapshotFinalizationState =
    copy(catchUpScheduled = catchUpScheduled - snapshotBlockId)

  def invalidate(snapshotBlockId: ModifierId): UtxoSnapshotFinalizationState =
    copy(
      catchUpScheduled = catchUpScheduled - snapshotBlockId,
      cleanupCompleted = cleanupCompleted - snapshotBlockId,
      sourceCleanupStarted = sourceCleanupStarted - snapshotBlockId
    )
}

private[wallet] object UtxoSnapshotFinalizationState {
  val empty: UtxoSnapshotFinalizationState =
    UtxoSnapshotFinalizationState(Set.empty, Set.empty, Set.empty)
}

private[wallet] object UtxoSnapshotScanStartPolicy {
  def shouldStartApplied(snapshotHeight: Height,
                         snapshotBlockId: ModifierId,
                         registryPristine: Boolean,
                         rescanInProgress: Boolean,
                         statusOpt: Option[UtxoSnapshotScanStatus]): Boolean = if (rescanInProgress) {
    false
  } else statusOpt match {
    case Some(status) =>
      !status.completed &&
        status.snapshotHeight == snapshotHeight &&
        status.snapshotBlockId == snapshotBlockId
    case None => registryPristine
  }
}

private[wallet] object UtxoSnapshotWalletScanner {

  val SnapshotScanBatchSize: Int = UtxoSnapshotScanDefinition.SnapshotScanBatchSize
  private val MaxRetryAttempts: Int = 3
  private val RetryDelay: FiniteDuration = 1.second
  private case object RetryTimerKey

  def props(walletActor: ActorRef,
            settings: ErgoSettings,
            sourceReader: UtxoSnapshotScanSourceReader): Props =
    Props(classOf[UtxoSnapshotWalletScanner], walletActor, settings, sourceReader)

  private[wallet] final case class SnapshotBatch(snapshotHeight: Height,
                                                 snapshotBlockId: ModifierId,
                                                 subtreeIndex: Int,
                                                 nextSubtreeIndex: Int,
                                                 completed: Boolean,
                                                 boxes: IndexedSeq[ErgoBox])

  private final case class Initialized(generation: Long,
                                        run: UtxoSnapshotScanRun,
                                        attempt: Int,
                                        result: Try[(UtxoSnapshotScanSource, UtxoSnapshotScanStatus)])
  private final case class BatchRead(generation: Long,
                                     run: UtxoSnapshotScanRun,
                                     attempt: Int,
                                     result: Try[SnapshotBatch])
  private final case class Applied(generation: Long,
                                   run: UtxoSnapshotScanRun,
                                   attempt: Int,
                                   result: Try[UtxoSnapshotScanStatus])
  private final case class RetryScan(generation: Long,
                                     run: UtxoSnapshotScanRun,
                                     attempt: Int)

  private[wallet] def lastCommittedBatchStatus(
    status: UtxoSnapshotScanStatus): Try[Option[UtxoSnapshotScanStatus]] = Try {
    require(!status.completed, "Completed UTXO snapshot progress has no resumable frontier")
    require(status.totalSubtrees > 0,
      s"Invalid UTXO snapshot part count ${status.totalSubtrees}")
    require(status.nextSubtreeIndex >= 0 &&
      status.nextSubtreeIndex < status.totalSubtrees,
      s"Invalid UTXO snapshot scan cursor ${status.nextSubtreeIndex}/${status.totalSubtrees}")
    if (status.nextSubtreeIndex == 0) {
      None
    } else {
      require(status.nextSubtreeIndex % SnapshotScanBatchSize == 0,
        s"Unaligned UTXO snapshot scan cursor ${status.nextSubtreeIndex}/${status.totalSubtrees}")
      val previousCursor = Math.subtractExact(status.nextSubtreeIndex, SnapshotScanBatchSize)
      require(previousCursor >= 0 &&
        nextBatchCursor(status.totalSubtrees, previousCursor) == status.nextSubtreeIndex,
        s"Unaligned UTXO snapshot scan cursor ${status.nextSubtreeIndex}/${status.totalSubtrees}")
      Some(status.copy(nextSubtreeIndex = previousCursor, completed = false))
    }
  }

  private[wallet] def nextBatchCursor(totalSubtrees: Int, subtreeIndex: Int): Int =
    Math.min(
      totalSubtrees.toLong,
      subtreeIndex.toLong + SnapshotScanBatchSize.toLong).toInt

  private[wallet] def readSnapshotBatch(sourceReader: UtxoSnapshotScanSourceReader,
                                        source: UtxoSnapshotScanSource,
                                        status: UtxoSnapshotScanStatus): Try[SnapshotBatch] = Try {
    require(status.nextSubtreeIndex >= 0 && status.nextSubtreeIndex < status.totalSubtrees,
      s"Invalid UTXO snapshot scan cursor ${status.nextSubtreeIndex}/${status.totalSubtrees}")
    val end = nextBatchCursor(status.totalSubtrees, status.nextSubtreeIndex)
    val boxesBuilder = Vector.newBuilder[ErgoBox]
    var index = status.nextSubtreeIndex
    while (index < end) {
      val subtree = sourceReader.readUtxoSnapshotScanPart(source, index).get
      boxesBuilder ++= collectBoxes(subtree).get
      index += 1
    }
    require(index > status.nextSubtreeIndex, "UTXO snapshot scan batch made no progress")
    SnapshotBatch(
      status.snapshotHeight,
      status.snapshotBlockId,
      status.nextSubtreeIndex,
      index,
      completed = index == status.totalSubtrees,
      boxesBuilder.result()
    )
  }

  private[wallet] def collectBoxes(subtree: BatchAVLProverSubtree[DigestType]): Try[IndexedSeq[ErgoBox]] = {
    val boxes = mutable.ArrayBuffer.empty[ErgoBox]

    def loop(node: ProverNodes[DigestType]): Try[Unit] = node match {
      case leaf: ProverLeaf[DigestType]
          if leaf.key.length == UtxoKeyLength && leaf.key.forall(_ == 0) && leaf.value.isEmpty =>
        Success(())
      case leaf: ProverLeaf[DigestType] =>
        ErgoBoxSerializer.parseBytesTry(leaf.value).map(box => boxes += box)
      case proxy: ProxyInternalNode[DigestType] =>
        Failure(new IllegalStateException(s"Unexpected proxy node in UTXO snapshot subtree ${Algos.encode(proxy.label)}"))
      case in: InternalProverNode[DigestType] =>
        loop(in.left).flatMap(_ => loop(in.right))
    }

    loop(subtree.subtreeTop).map(_ => boxes.toVector)
  }

  private val UtxoKeyLength = 32
}
