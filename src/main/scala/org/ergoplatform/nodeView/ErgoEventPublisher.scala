package org.ergoplatform.nodeView

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.history.header.Header
import org.zeromq.SocketType
import org.zeromq.ZMQ
import org.zeromq.ZContext
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.{FullBlockApplied, SuccessfulTransaction}
import scorex.util.ScorexLogging

class ErgoEventPublisher(socket: ZMQ.Socket) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SuccessfulTransaction])
    context.system.eventStream.subscribe(self, classOf[FullBlockApplied])
  }

  @SuppressWarnings(Array("IsInstanceOf"))
  override def receive: Receive = {
    case SuccessfulTransaction(transaction: UnconfirmedTransaction) =>
      socket.send(s"utx${transaction.id}")

    case FullBlockApplied(header: Header) =>
      socket.send(s"blk${header.id}${header.height.toString}")

    case a: Any => log.warn(s"ErgoEventPublisher got improper input: $a")
  }
}

object ErgoEventPublisherRef {

  def apply()
           (implicit context: ActorRefFactory): ActorRef = {
    val zContext: ZContext = new ZContext()
    val socket = zContext.createSocket(SocketType.PUB)
    socket.bind("tcp://127.0.0.1:5555")
    val props = Props(new ErgoEventPublisher(socket))
    context.actorOf(props)
  }

}
