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
 * This class is used to provide the current UTXO set for wallet scans when bootstrapping
 * by UTXO set snapshot. This is done by creating a snapshot of the UTXO set, deserializing
 * the raw bytes to ErgoBoxes and sending them to the wallet actor in chunks.
 * @param nodeView - NodeView actor to get wallet and UTXOs from
 */
class UtxoSetScanner(nodeView: ActorRef) extends Actor with ScorexLogging {

  private var history: ErgoHistory = _
  private def historyStorage: HistoryStorage = history.historyStorage

  private implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
  private implicit val duration: Duration = Duration.create(10, TimeUnit.SECONDS)

  /**
   * Internal buffer that holds deserialized AVL subtrees until they are sent to wallet
   */
  private val chunkBuffer: ArrayBuffer[(ModifierId,Array[ErgoBox])] = ArrayBuffer.empty[(ModifierId,Array[ErgoBox])]

  /**
   * Reads the current progress of the scanner.
   * @return (current segment, total segments)
   */
  private def readProgress(): (Int, Int) =
    historyStorage.getIndex(utxoSetScanProgressKey).map(ByteBuffer.wrap).map { buffer =>
      val current = buffer.getInt
      val total = buffer.getInt
      (current, total)
    }.getOrElse((0, 0))

  /**
   * Writes progress to db.
   * @param current - current retrieved segment
   * @param total - total segment count
   */
  private def writeProgress(current: Int, total: Int): Unit = {
    val buffer: ByteBuffer = ByteBuffer.allocate(8)
    buffer.putInt(current)
    buffer.putInt(total)
    historyStorage.insert(Array((utxoSetScanProgressKey, buffer.array)), Array.empty[BlockSection])
  }

  /**
   * Send deserialized AVL subtrees to wallet for scanning.
   * @param wallet - wallet to send to
   * @param current - current retrieved segment
   */
  private def sendBufferToWallet(wallet: ErgoWallet, current: Int): Unit = {
    wallet.scanUtxoSnapshot(ScanBoxesFromUtxoSnapshot(chunkBuffer, current, MainnetTotal))
    writeProgress(current, MainnetTotal)
    chunkBuffer.clear()
  }

  private def run(): Unit = {

    var (current, total) = readProgress()
    if(total == 0 || // scan should not start yet, still syncing
       current == MainnetTotal) // scan already done
      return

    val (state, wallet) = Await.result(
      (nodeView ? GetDataFromCurrentView[UtxoState, (UtxoState, ErgoWallet)](x => (x.state, x.vault)))
        .mapTo[(UtxoState, ErgoWallet)],
      duration
    )

    val initialized: Boolean = Await.result(wallet.getWalletStatus.map(_.initialized), duration)
    if(!initialized) // wallet is not initialized
      return

    log.info(s"Starting UTXO set snapshot scan for $total chunks")

    state.persistentProver.storage.asInstanceOf[VersionedLDBAVLStorage].iterateAVLTree(current, MainnetManifestDepth) { subtree =>
      current += 1

      chunkBuffer += ((
        bytesToId(subtree.id),
        subtree.leafValues.par.flatMap(ErgoBoxSerializer.parseBytesTry(_).toOption).toArray
      ))

      if(chunkBuffer.size == 32) {
        sendBufferToWallet(wallet, current)
      }
    }

    // flush remaining data, if any
    if(chunkBuffer.nonEmpty) {
      sendBufferToWallet(wallet, current)
    }

    if(current == total) {
      log.info(s"Successfully scanned $total Utxo set subtrees")
      // send newest block to wallet, if blocks were applied since scan began it will go back to scan them
      wallet.scanPersistent(history.bestFullBlockOpt.get)
    }else {
      log.error(s"Inconsistent Utxo set scan state: $current scanned subtrees out of $total")
    }

  }

  override def receive: Receive = {
    case InitializeUtxoSetScannerWithHistory(history: ErgoHistory) =>
      this.history = history
      run()
    case StartUtxoSetScan(rescan: Boolean) =>
      if(readProgress()._1 == 0 || //
        rescan) // start over UTXO set scan
        writeProgress(0, MainnetTotal)
      run()
  }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[InitializeUtxoSetScannerWithHistory])
    context.system.eventStream.subscribe(self, classOf[StartUtxoSetScan])
  }

}

object UtxoSetScanner {

  /**
   * Initialize UTXO set scanner with database and try continuing scan if possible
   * @param history - database handle
   */
  case class InitializeUtxoSetScannerWithHistory(history: ErgoHistory)

  /**
   * Start scanning UTXO set, or continue if the scan was interrupted, or start over if rescan = true
   * @param rescan - whether to start over or just continue scan
   */
  case class StartUtxoSetScan(rescan: Boolean)

  /**
   * Number of subtrees to divide AVL tree to
   */
  private val MainnetTotal: Int = math.pow(2, ManifestSerializer.MainnetManifestDepth).toInt

  private val utxoSetScanProgressKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("scanned chunk"))

  def apply(nodeView: ActorRef)(implicit system: ActorSystem): ActorRef =
    system.actorOf(Props.create(classOf[UtxoSetScanner], nodeView))
}
