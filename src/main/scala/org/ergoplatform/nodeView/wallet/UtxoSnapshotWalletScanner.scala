package org.ergoplatform.nodeView.wallet

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import org.ergoplatform.ErgoBox
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Height
import org.ergoplatform.nodeView.state.UtxoStateReader
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.UtxoSnapshotScanStatus
import org.ergoplatform.serialization.ManifestSerializer
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.ergoplatform.GlobalConstants
import scorex.crypto.authds.avltree.batch.Constants.DigestType
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverSubtree, ProxyInternalNode}
import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverLeaf, ProverNodes}
import scorex.util.{ModifierId, ScorexLogging}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

private[wallet] class UtxoSnapshotWalletScanner(walletActor: ActorRef,
                                                settings: ErgoSettings)
  extends Actor with ScorexLogging {

  import UtxoSnapshotWalletScanner._

  private implicit val timeout: Timeout = Timeout(5.minutes)
  private implicit val callbackDispatcher = context.dispatcher
  private val snapshotDispatcher = context.system.dispatchers.lookup(GlobalConstants.IndexerDispatcher)

  private var runId: Long = 0L
  private var activeSnapshot: Option[(Height, ModifierId)] = None

  override def receive: Receive = {
    case StartUtxoSnapshotScan(snapshotHeight, snapshotBlockId, stateReader, forceRestart) =>
      val snapshot = snapshotHeight -> snapshotBlockId
      if (!forceRestart && activeSnapshot.contains(snapshot)) {
        log.debug(s"UTXO snapshot wallet scan is already running for height $snapshotHeight")
      } else if (!settings.nodeSettings.utxoSettings.utxoBootstrap) {
        log.debug("Ignoring UTXO snapshot wallet scan request because utxoBootstrap is disabled")
      } else {
        runId += 1
        activeSnapshot = Some(snapshot)
        start(runId, snapshotHeight, snapshotBlockId, stateReader)
      }

    case Initialized(id, stateReader, Success(status)) if isCurrent(id) =>
      if (status.completed) {
        activeSnapshot = None
        log.info(s"UTXO snapshot wallet scan is already completed at height ${status.snapshotHeight}")
      } else {
        readBatch(id, stateReader, status)
      }

    case Initialized(id, _, Failure(t)) if isCurrent(id) =>
      activeSnapshot = None
      log.error("Failed to initialize UTXO snapshot wallet scan", t)

    case BatchRead(id, stateReader, Success(batch)) if isCurrent(id) =>
      val applyResult = (walletActor ? ApplyUtxoSnapshotScanBatch(
        batch.snapshotHeight,
        batch.snapshotBlockId,
        batch.subtreeIndex,
        batch.nextSubtreeIndex,
        batch.completed,
        batch.boxes
      )).mapTo[Try[UtxoSnapshotScanStatus]]

      applyResult.map(Applied(id, stateReader, _)).pipeTo(self)

    case BatchRead(id, _, Failure(t)) if isCurrent(id) =>
      activeSnapshot = None
      log.error("Failed to read UTXO snapshot wallet scan batch", t)

    case Applied(id, stateReader, Success(status)) if isCurrent(id) =>
      if (status.completed) {
        activeSnapshot = None
        log.info(s"UTXO snapshot wallet scan completed at height ${status.snapshotHeight}")
      } else {
        readBatch(id, stateReader, status)
      }

    case Applied(id, _, Failure(t)) if isCurrent(id) =>
      activeSnapshot = None
      log.error("Failed to apply UTXO snapshot wallet scan batch", t)

    case _: Initialized | _: BatchRead | _: Applied =>
      log.debug("Ignoring stale UTXO snapshot wallet scan message")
  }

  private def isCurrent(id: Long): Boolean =
    id == runId

  private def start(id: Long,
                    snapshotHeight: Height,
                    snapshotBlockId: ModifierId,
                    stateReader: UtxoStateReader): Unit = {
    val manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt
    val init = Future {
      stateReader.countSnapshotSubtrees(manifestDepth)
    }(snapshotDispatcher).flatMap {
      case Success(totalSubtrees) =>
        (walletActor ? GetOrInitUtxoSnapshotScanStatus(
          snapshotHeight,
          snapshotBlockId,
          manifestDepth,
          totalSubtrees
        )).mapTo[Try[UtxoSnapshotScanStatus]]
      case Failure(t) =>
        Future.successful(Failure(t))
    }

    init.map(Initialized(id, stateReader, _)).pipeTo(self)
  }

  private def readBatch(id: Long,
                        stateReader: UtxoStateReader,
                        status: UtxoSnapshotScanStatus): Unit = {
    Future {
      readSnapshotBatch(stateReader, status)
    }(snapshotDispatcher).map(BatchRead(id, stateReader, _)).pipeTo(self)
  }
}

private[wallet] object UtxoSnapshotWalletScanner {

  val SnapshotScanBatchSize: Int = 32

  def props(walletActor: ActorRef, settings: ErgoSettings): Props =
    Props(classOf[UtxoSnapshotWalletScanner], walletActor, settings)

  private[wallet] final case class SnapshotBatch(snapshotHeight: Height,
                                                 snapshotBlockId: ModifierId,
                                                 subtreeIndex: Int,
                                                 nextSubtreeIndex: Int,
                                                 completed: Boolean,
                                                 boxes: IndexedSeq[ErgoBox])

  private final case class Initialized(runId: Long,
                                       stateReader: UtxoStateReader,
                                       result: Try[UtxoSnapshotScanStatus])

  private final case class BatchRead(runId: Long,
                                     stateReader: UtxoStateReader,
                                     result: Try[SnapshotBatch])

  private final case class Applied(runId: Long,
                                   stateReader: UtxoStateReader,
                                   result: Try[UtxoSnapshotScanStatus])

  private[wallet] def readSnapshotBatch(stateReader: UtxoStateReader,
                                        status: UtxoSnapshotScanStatus): Try[SnapshotBatch] = {
    val boxesBuilder = Vector.newBuilder[ErgoBox]
    val nextIndex = stateReader.iterateSnapshotSubtrees(
      status.nextSubtreeIndex,
      status.manifestDepth,
      SnapshotScanBatchSize
    ) { case (_, subtree) =>
      collectBoxes(subtree).map(boxesBuilder ++= _)
    }

    nextIndex.map { next =>
      SnapshotBatch(
        status.snapshotHeight,
        status.snapshotBlockId,
        status.nextSubtreeIndex,
        next,
        completed = next >= status.totalSubtrees,
        boxesBuilder.result()
      )
    }
  }

  private[wallet] def collectBoxes(subtree: BatchAVLProverSubtree[DigestType]): Try[IndexedSeq[ErgoBox]] = {
    val boxes = mutable.ArrayBuffer.empty[ErgoBox]

    def loop(node: ProverNodes[DigestType]): Try[Unit] = node match {
      case leaf: ProverLeaf[DigestType] =>
        ErgoBoxSerializer.parseBytesTry(leaf.value).map(box => boxes += box)
      case proxy: ProxyInternalNode[DigestType] =>
        Failure(new IllegalStateException(s"Unexpected proxy node in UTXO snapshot subtree ${Algos.encode(proxy.label)}"))
      case in: InternalProverNode[DigestType] =>
        loop(in.left).flatMap(_ => loop(in.right))
    }

    loop(subtree.subtreeTop).map(_ => boxes.toVector)
  }
}
