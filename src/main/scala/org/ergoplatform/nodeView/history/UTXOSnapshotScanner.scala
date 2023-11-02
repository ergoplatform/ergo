package org.ergoplatform.nodeView.history

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.google.common.primitives.Ints
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.UTXOSnapshotScanner._
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.core.serialization.SubtreeSerializer
import scorex.crypto.authds.avltree.batch.Constants
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSubtree
import scorex.crypto.hash.Blake2b256
import scorex.db.ByteArrayWrapper
import scorex.util.{ScorexLogging, bytesToId}

import java.nio.ByteBuffer
import scala.util.{Failure, Success}


class UTXOSnapshotScanner() extends Actor with ScorexLogging {

  private var history: ErgoHistory = _
  private def historyStorage: HistoryStorage = history.historyStorage

  private def readProgress(): (Int, Int) =
    historyStorage.getIndex(utxoSetScanProgressKey).map(ByteBuffer.wrap).map { buffer =>
      val current = buffer.getInt
      val total = buffer.getInt
      (current, total)
    }.getOrElse((0, 0))

  private def saveProgress(current: Int, total: Int): Unit = {
    val buffer: ByteBuffer = ByteBuffer.allocate(8)
    buffer.putInt(current)
    buffer.putInt(total)
    historyStorage.insert(Array((utxoSetScanProgressKey,buffer.array)), Array.empty[BlockSection])
  }

  private def scanBox(box: ErgoBox, current: Int, total: Int): Unit = {
    //filterWalletOutput(box, Some(box.creationHeight), null, None)
    log.info(s"Scanned box ${bytesToId(box.id)} at height ${box.creationHeight} in chunk $current / $total")
  }

  private def run(): Unit = {
    var (current, total) = readProgress()
    if(total == 0) return
    log.info(s"Starting UTXO set snapshot scan for $total chunks")
    downloadedChunksIterator(historyStorage, current, total).foreach { subtree =>
      subtree.leafValues.foreach { leaf =>
        ErgoBoxSerializer.parseBytesTry(leaf) match {
          case Success(box) => scanBox(box, current, total)
          case Failure(e) => log.error(s"Failed to parse box from snapshot chunk $current / $total: $e")
        }
      }
      current += 1
      saveProgress(current, total)
    }
    if(current == total) {
      history.removeUtxoSnapshotChunks()
      saveProgress(0, 0)
      log.info(s"Successfully scanned $total UTXO set snapshot chunks")
    }
  }

  override def receive: Receive = {
    case InitializeUTXOSetScanner(history: ErgoHistory) =>
      this.history = history
      run()
    case StartUtxoSetSnapshotScan() =>
      run()
  }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[InitializeUTXOSetScanner])
    context.system.eventStream.subscribe(self, classOf[StartUtxoSetSnapshotScan])
  }

}

object UTXOSnapshotScanner {

  case class InitializeUTXOSetScanner(history: ErgoHistory)

  case class StartUtxoSetSnapshotScan()

  private val downloadedChunksPrefix = Blake2b256.hash("downloaded chunk").drop(4)

  private def chunkIdFromIndex(index: Int): Array[Byte] = {
    val idxBytes = Ints.toByteArray(index)
    downloadedChunksPrefix ++ idxBytes
  }

  private def downloadedChunkIdsIterator(from: Int, to: Int): Iterator[Array[Byte]] = {
    Iterator.range(from, to).map(chunkIdFromIndex)
  }

  def downloadedChunksIterator(historyStorage: HistoryStorage, from: Int, to: Int): Iterator[BatchAVLProverSubtree[Constants.DigestType]] = {
     downloadedChunkIdsIterator(from, to).flatMap { chunkId =>
       historyStorage
         .get(chunkId)
         .flatMap(bs => SubtreeSerializer.parseBytesTry(bs).toOption)
     }
  }

  private val utxoSetScanProgressKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("scanned chunk"))

  def apply()(implicit system: ActorSystem): ActorRef =
    system.actorOf(Props.create(classOf[UTXOSnapshotScanner]))
}
