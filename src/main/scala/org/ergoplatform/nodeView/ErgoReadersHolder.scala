package org.ergoplatform.nodeView

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.ErgoStateReader
import scorex.core.NodeViewHolder.ReceivableMessages._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.core.utils.ScorexLogging

class ErgoReadersHolder(viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    viewHolderRef ! GetNodeViewChanges(history = true, state = true, vault = true, mempool = true)
  }

  var historyReaderOpt: Option[ErgoHistoryReader] = None
  var stateReaderOpt: Option[ErgoStateReader] = None
  var mempoolReaderOpt: Option[ErgoMemPoolReader] = None

  @SuppressWarnings(Array("IsInstanceOf"))
  override def receive: Receive = {
    case ChangedHistory(reader: ErgoHistoryReader@unchecked) if reader.isInstanceOf[ErgoHistoryReader] =>
      historyReaderOpt = Some(reader)

    case ChangedState(reader: ErgoStateReader@unchecked) if reader.isInstanceOf[ErgoStateReader] =>
      stateReaderOpt = Some(reader)

    case ChangedMempool(reader: ErgoMemPoolReader@unchecked) if reader.isInstanceOf[ErgoMemPoolReader] =>
      mempoolReaderOpt = Some(reader)

    case GetReaders =>
      (historyReaderOpt, stateReaderOpt, mempoolReaderOpt) match {
        case (Some(h), Some(s), Some(m)) => sender ! Readers(h, s, m)
        case _ =>
      }

    case GetDataFromHistory(f) =>
      historyReaderOpt.map(sender ! f(_)).getOrElse(log.warn("Trying to get data from undefined history reader"))

    case _ =>
    //Do nothing for now. Implement when needed

  }
}

object ErgoReadersHolder {

  case class GetDataFromHistory[A](f: ErgoHistoryReader => A)

  case object GetReaders

  case class Readers(h: ErgoHistoryReader, s: ErgoStateReader, m: ErgoMemPoolReader)

}

object ErgoReadersHolderRef {
  def props(viewHolderRef: ActorRef): Props = Props(new ErgoReadersHolder(viewHolderRef))

  def apply(viewHolderRef: ActorRef)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolderRef))

  def apply(viewHolderRef: ActorRef,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolderRef), name)

}
