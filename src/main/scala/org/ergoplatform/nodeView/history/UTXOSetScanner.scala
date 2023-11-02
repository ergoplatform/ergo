package org.ergoplatform.nodeView.history

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.google.common.primitives.Ints
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.UTXOSetScanner._
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


class UTXOSetScanner() extends Actor with ScorexLogging {

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
    log.info(s"Scanning box ${bytesToId(box.id)} in chunk $current / $total")
    // TODO perform scan
  }

  private def run(): Unit = {
    var (current, total) = readProgress()
    downloadedChunksIterator(historyStorage, current, total).foreach { subtree =>
      subtree.leafValues.foreach { leaf =>
        ErgoBoxSerializer.parseBytesTry(leaf) match {
          case Success(box) => scanBox(box, current, total)
          case Failure(e) => log.error(s"Failed to parse box from prover leaf: $e")
        }
      }
      current += 1
      saveProgress(current, total)
    }
    if(current == total)
      history.removeUtxoSnapshotChunks()
  }

  override def receive: Receive = {
    case InitializeUTXOSetScanner(history: ErgoHistory) =>
      this.history = history
      if(readProgress()._2 != 0)
        run()
    case StartScan() =>
      run()
  }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[InitializeUTXOSetScanner])
  }

}

object UTXOSetScanner {

  case class InitializeUTXOSetScanner(history: ErgoHistory)

  case class StartScan()

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
    system.actorOf(Props.create(classOf[UTXOSetScanner]))
}
