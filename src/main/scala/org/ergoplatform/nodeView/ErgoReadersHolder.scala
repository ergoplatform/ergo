package org.ergoplatform.nodeView

import akka.actor.{Actor, ActorRef}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, ErgoMemPoolReader}
import org.ergoplatform.nodeView.state.ErgoStateReader
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder._
import scorex.core.utils.ScorexLogging

class ErgoReadersHolder(viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    val vhEvents = Seq(
      NodeViewHolder.EventType.HistoryChanged,
      NodeViewHolder.EventType.StateChanged,
      NodeViewHolder.EventType.MempoolChanged,
      NodeViewHolder.EventType.VaultChanged,
    )
    viewHolderRef ! Subscribe(vhEvents)
    viewHolderRef ! GetNodeViewChanges(history = true, state = true, vault = true, mempool = true)
  }

  var historyReaderOpt: Option[ErgoHistoryReader] = None
  var stateReaderOpt: Option[ErgoStateReader] = None
  var mempoolReaderOpt: Option[ErgoMemPoolReader] = None

  @SuppressWarnings(Array("IsInstanceOf"))
  override def receive = {
    case ChangedHistory(reader: ErgoHistoryReader@unchecked) if reader.isInstanceOf[ErgoHistoryReader] =>
      historyReaderOpt = Some(reader)

    case ChangedState(reader: ErgoStateReader@unchecked) if reader.isInstanceOf[ErgoStateReader] =>
      stateReaderOpt = Some(reader)

    case ChangedMempool(reader: ErgoMemPoolReader@unchecked) if reader.isInstanceOf[ErgoMemPoolReader] =>
      mempoolReaderOpt = Some(reader)

    case GetReaders =>
      sender ! Readers(historyReaderOpt, stateReaderOpt, mempoolReaderOpt)

    case GetDataFromHistory(f) =>
      historyReaderOpt match {
        case Some(historyReader) =>
          sender() ! f(historyReader)
        case None =>
          log.warn("Trying to get data from undefined history reader")
      }

    case _ =>
    //Do nothing for now. Implement when needed

  }
}

object ErgoReadersHolder {

  case class GetDataFromHistory[A](f: ErgoHistoryReader => A)

  case object GetReaders

  case class Readers(h: Option[ErgoHistoryReader], s: Option[ErgoStateReader], m: Option[ErgoMemPoolReader])

}