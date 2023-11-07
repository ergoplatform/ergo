package org.ergoplatform.nodeView.history

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.history.UtxoSnapshotScanner._
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.ScanBoxesFromUtxoSnapshot
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.core.serialization.ManifestSerializer
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

class UtxoSnapshotScanner(nodeView: ActorRef) extends Actor with ScorexLogging {

  private var history: ErgoHistory = _
  private def historyStorage: HistoryStorage = history.historyStorage

  private implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
  private implicit val duration: Duration = Duration.create(10, TimeUnit.SECONDS)

  private val chunkBuffer: ArrayBuffer[(ModifierId,Array[ErgoBox])] = ArrayBuffer.empty[(ModifierId,Array[ErgoBox])]

  private def readProgress(): (Int, Int) =
    historyStorage.getIndex(utxoSetScanProgressKey).map(ByteBuffer.wrap).map { buffer =>
      val current = buffer.getInt
      val total = buffer.getInt
      (current, total)
    }.getOrElse((0, 0))

  private def writeProgress(current: Int, total: Int): Unit = {
    val buffer: ByteBuffer = ByteBuffer.allocate(8)
    buffer.putInt(current)
    buffer.putInt(total)
    historyStorage.insert(Array((utxoSetScanProgressKey, buffer.array)), Array.empty[BlockSection])
  }

  private def sendBufferToWallet(wallet: ErgoWallet, current: Int, total: Int): Unit = {
    wallet.scanUtxoSnapshot(ScanBoxesFromUtxoSnapshot(chunkBuffer, current, total))
    writeProgress(current, total)
    chunkBuffer.clear()
  }

  private def run(): Unit = {

    var (current, total) = readProgress()
    if(total == math.pow(2, ManifestSerializer.MainnetManifestDepth)) return

    val (state, wallet) = Await.result(
      (nodeView ? GetDataFromCurrentView[UtxoState, (UtxoState, ErgoWallet)](x => (x.state, x.vault)))
        .mapTo[(UtxoState, ErgoWallet)],
      duration
    )

    val initialized: Boolean = Await.result(wallet.getWalletStatus.map(_.initialized), duration)
    if(!initialized) return

    writeProgress(current, total)

    log.info(s"Starting UTXO set snapshot scan for $total chunks")

    state.persistentProver.storage.asInstanceOf[VersionedLDBAVLStorage].iterateAVLTree { subtree =>
      current += 1

      chunkBuffer += ((
        bytesToId(subtree.id),
        subtree.leafValues.par.flatMap(ErgoBoxSerializer.parseBytesTry(_).toOption).toArray
      ))

      if(chunkBuffer.size == 32) {
        sendBufferToWallet(wallet, current, total)
      }
    }

    // flush remaining data, if any
    if(chunkBuffer.nonEmpty) {
      sendBufferToWallet(wallet, current, total)
    }

    if(current == total) {
      log.info(s"Successfully scanned $total UTXO set snapshot chunks")
      writeProgress(0, 0)
      // start wallet scan with first available block
      val firstBlock = history.bestFullBlockAt(history.readMinimalFullBlockHeight()).get
      wallet.scanPersistent(firstBlock)
    }

  }

  override def receive: Receive = {
    case InitializeUtxoSetScannerWithHistory(history: ErgoHistory) =>
      this.history = history
    case StartUtxoSetSnapshotScan() =>
      run()
  }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[InitializeUtxoSetScannerWithHistory])
    context.system.eventStream.subscribe(self, classOf[StartUtxoSetSnapshotScan])
  }

}

object UtxoSnapshotScanner {

  case class InitializeUtxoSetScannerWithHistory(history: ErgoHistory)

  case class StartUtxoSetSnapshotScan()

  private val utxoSetScanProgressKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("scanned chunk"))

  def apply(nodeView: ActorRef)(implicit system: ActorSystem): ActorRef =
    system.actorOf(Props.create(classOf[UtxoSnapshotScanner], nodeView))
}
