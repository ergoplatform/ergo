package org.ergoplatform.nodeView.history

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.history.UtxoSetScanner._
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages.ScanBoxesFromUtxoSnapshot
import org.ergoplatform.serialization.ManifestSerializer
import org.ergoplatform.serialization.ManifestSerializer.MainnetManifestDepth
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage
import scorex.crypto.hash.Blake2b256
import scorex.db.ByteArrayWrapper
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import java.nio.ByteBuffer
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

/**
  * Actor that scans the current UTXO set for wallet-related boxes and external scan rules
  * when bootstrapping a node from a UTXO set snapshot.
  *
  * It traverses the authenticated AVL+ tree of the UTXO state via a LevelDB snapshot,
  * deserializes leaf values into [[ErgoBox]] instances, and forwards them to the wallet
  * actor in batches of up to 32 subtrees while persisting progress in [[HistoryStorage]].
  *
  * @param nodeView - reference to the [[org.ergoplatform.nodeView.ErgoNodeViewHolder]] actor
  * @param depth    - AVL+ tree depth at which to split into subtrees (defaults to [[MainnetManifestDepth]])
  */
class UtxoSetScanner(nodeView: ActorRef, depth: Int = MainnetManifestDepth) extends Actor with ScorexLogging {

  def this(nodeView: ActorRef) = this(nodeView, MainnetManifestDepth)

  private var history: ErgoHistory = _
  private def historyStorage: HistoryStorage = history.historyStorage

  private val totalSubtrees: Int = math.pow(2, depth).toInt

  private implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
  private implicit val duration: Duration = Duration.create(10, TimeUnit.SECONDS)

  /**
    * Internal buffer that holds deserialized AVL subtrees until they are sent to the wallet.
    */
  private val chunkBuffer: ArrayBuffer[(ModifierId, Array[ErgoBox])] = ArrayBuffer.empty[(ModifierId, Array[ErgoBox])]

  /**
    * Ensures `history` is initialized by querying `nodeView` if not yet received via event stream.
    */
  private def ensureHistory(): Boolean = {
    if (history == null && nodeView != null) {
      history = Await.result(
        (nodeView ? GetDataFromCurrentView[UtxoState, ErgoHistory](_.history)).mapTo[ErgoHistory],
        duration
      )
    }
    history != null
  }

  /**
    * Reads the current progress of the UTXO set scanner from history storage.
    *
    * @return `(currentSegment, totalSegments)`
    */
  private def readProgress(): (Int, Int) =
    historyStorage.getIndex(utxoSetScanProgressKey).map(ByteBuffer.wrap).map { buffer =>
      val current = buffer.getInt
      val total = buffer.getInt
      (current, total)
    }.getOrElse((0, 0))

  /**
    * Persists scanner progress to history storage.
    *
    * @param current - number of subtrees scanned so far
    * @param total   - total number of subtrees to scan
    */
  private def writeProgress(current: Int, total: Int): Unit = {
    val buffer: ByteBuffer = ByteBuffer.allocate(8)
    buffer.putInt(current)
    buffer.putInt(total)
    historyStorage.insert(Array((utxoSetScanProgressKey, buffer.array)), Array.empty[BlockSection])
  }

  /**
    * Sends buffered AVL subtrees to the wallet actor for scanning and records progress.
    *
    * @param wallet     - wallet handle
    * @param current    - current subtree count after this batch
    * @param scanHeight - height of the UTXO state being scanned
    * @param offset     - how far `current` was moved past the subtrees actually visited (shallow tree), 0 otherwise
    * @return whether the wallet scanned the whole batch
    */
  private def sendBufferToWallet(wallet: ErgoWallet, current: Int, scanHeight: Int, offset: Int = 0): Boolean = {
    val scanned = wallet.scanUtxoSnapshot(ScanBoxesFromUtxoSnapshot(chunkBuffer.toVector, current, totalSubtrees, scanHeight))
    val complete = scanned == current
    // on a failure, record the real position in the tree, so the next scan resumes at the failed subtree
    writeProgress(if (complete) current else scanned - offset, totalSubtrees)
    chunkBuffer.clear()
    complete
  }

  private def run(): Unit = {
    if (!ensureHistory()) return

    var (current, total) = readProgress()
    if (total == 0 || // scan should not start yet, still syncing
        current == totalSubtrees) // scan already done
      return

    val (state, wallet) = Await.result(
      (nodeView ? GetDataFromCurrentView[UtxoState, (UtxoState, ErgoWallet)](x => (x.state, x.vault)))
        .mapTo[(UtxoState, ErgoWallet)],
      duration
    )

    val initialized: Boolean = Await.result(wallet.getWalletStatus.map(_.initialized), duration)
    if (!initialized) // wallet is not initialized
      return

    val scanHeight = state.stateContext.currentHeight
    log.info(s"Starting UTXO set snapshot scan for $total chunks at height $scanHeight")

    var failed = false
    chunkBuffer.clear()
    state.persistentProver.storage.asInstanceOf[VersionedLDBAVLStorage].iterateAVLTree(current, depth) { subtree =>
      if (!failed) {
        // Flush a full 32-subtree batch only when another subtree follows, keeping the last subtree
        // for the final flush so it receives `total` and records `scanHeight` even on shallow trees.
        if (chunkBuffer.size == 32) {
          failed = !sendBufferToWallet(wallet, current, scanHeight)
        }
        if (!failed) {
          current += 1
          chunkBuffer += ((
            bytesToId(subtree.id),
            subtree.leafValues.par.flatMap(ErgoBoxSerializer.parseBytesTry(_).toOption).toArray
          ))
        }
      }
    }

    if (failed) {
      log.error("Wallet failed to scan a UTXO set subtree; the scan resumes from it on the next scan request")
      return
    }

    // If the AVL+ tree had leaves above `depth` (fewer than 2^depth subtrees), all leaves have now been visited
    val offset = total - current
    if (current < total) {
      current = total
    }

    // flush remaining data, if any
    if (chunkBuffer.nonEmpty) {
      if (!sendBufferToWallet(wallet, current, scanHeight, offset)) {
        log.error("Wallet failed to scan a UTXO set subtree; the scan resumes from it on the next scan request")
        return
      }
    } else if (readProgress()._1 < total) {
      writeProgress(total, total)
    }

    if (current == total) {
      log.info(s"Successfully scanned $total Utxo set subtrees")
      // send newest block to wallet if new blocks were applied since the UTXO snapshot scan began
      history.bestFullBlockOpt.filter(_.height > scanHeight).foreach(wallet.scanPersistent)
    } else {
      log.error(s"Inconsistent Utxo set scan state: $current scanned subtrees out of $total")
    }
  }

  override def receive: Receive = {
    case InitializeUtxoSetScannerWithHistory(history: ErgoHistory) =>
      this.history = history
      run()
    case StartUtxoSetScan(rescan: Boolean) =>
      if (ensureHistory()) {
        if (readProgress()._1 == 0 || rescan) // start over UTXO set scan
          writeProgress(0, totalSubtrees)
        run()
      }
  }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[InitializeUtxoSetScannerWithHistory])
    context.system.eventStream.subscribe(self, classOf[StartUtxoSetScan])
  }

}

object UtxoSetScanner {

  /**
    * Signal sent on node startup (when `utxoBootstrap` is enabled) to initialize [[UtxoSetScanner]]
    * with the [[ErgoHistory]] database handle and resume an interrupted UTXO set scan if one was in progress.
    *
    * @param history - history database handle
    */
  case class InitializeUtxoSetScannerWithHistory(history: ErgoHistory)

  /**
    * Signal sent when the node finishes syncing after UTXO snapshot bootstrap (with `rescan = false`)
    * or when a user requests a wallet rescan (with `rescan = true`) to start or restart scanning the UTXO set.
    *
    * @param rescan - if `true`, resets progress to 0 and rescans the entire UTXO set from scratch;
    *                 if `false`, starts scanning if not yet started or continues from persisted progress
    */
  case class StartUtxoSetScan(rescan: Boolean)

  /**
    * Number of subtrees to divide the mainnet AVL+ tree into (`2^MainnetManifestDepth`).
    */
  val MainnetTotal: Int = math.pow(2, ManifestSerializer.MainnetManifestDepth).toInt

  private[history] val utxoSetScanProgressKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("scanned chunk"))

  /**
    * Starts the [[UtxoSetScanner]] actor in the given actor system.
    *
    * @param nodeView - reference to the [[org.ergoplatform.nodeView.ErgoNodeViewHolder]] actor
    * @return reference to the started [[UtxoSetScanner]] actor
    */
  def apply(nodeView: ActorRef)(implicit system: ActorSystem): ActorRef =
    system.actorOf(Props.create(classOf[UtxoSetScanner], nodeView))

  /**
    * Starts the [[UtxoSetScanner]] actor with a custom subtree split depth.
    *
    * @param nodeView - reference to the [[org.ergoplatform.nodeView.ErgoNodeViewHolder]] actor
    * @param depth    - AVL+ tree depth at which to split into subtrees
    * @return reference to the started [[UtxoSetScanner]] actor
    */
  def apply(nodeView: ActorRef, depth: Int)(implicit system: ActorSystem): ActorRef =
    system.actorOf(Props(new UtxoSetScanner(nodeView, depth)))
}
