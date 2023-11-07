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

class UtxoSetScanner(nodeView: ActorRef) extends Actor with ScorexLogging {

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
    if(!initialized) return

    log.info(s"Starting UTXO set snapshot scan for $total chunks")

    state.persistentProver.storage.asInstanceOf[VersionedLDBAVLStorage].iterateAVLTree(current) { subtree =>
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
      wallet.scanPersistent(history.bestFullBlockOpt.get)
    }else {
      log.error(s"Inconsistent Utxo set scan state: $current scanned subtrees out of $total")
    }

  }

  override def receive: Receive = {
    case StartUtxoSetScanWithHistory(history: ErgoHistory) =>
      this.history = history
      run()
    case StartUtxoSetScan(rescan: Boolean) =>
      if(readProgress()._1 == 0 || rescan) writeProgress(0, MainnetTotal)
      run()
  }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[StartUtxoSetScanWithHistory])
    context.system.eventStream.subscribe(self, classOf[StartUtxoSetScan])
  }

}

object UtxoSetScanner {

  case class StartUtxoSetScanWithHistory(history: ErgoHistory)

  case class StartUtxoSetScan(rescan: Boolean)

  private val MainnetTotal: Int = math.pow(2, ManifestSerializer.MainnetManifestDepth).toInt

  private val utxoSetScanProgressKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("scanned chunk"))

  def apply(nodeView: ActorRef)(implicit system: ActorSystem): ActorRef =
    system.actorOf(Props.create(classOf[UtxoSetScanner], nodeView))
}
