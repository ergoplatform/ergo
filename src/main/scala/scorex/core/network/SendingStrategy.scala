package scorex.core.network

import scala.util.Random

trait SendingStrategy {
  def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer]
}

object SendToRandom extends SendingStrategy {

  private val limit: Long = 3 * 60 * 60 * 1000 // 3h

  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = {
    val activeRecently: Seq[ConnectedPeer] = // choose peers which were active in the last limit hours
      peers.filter(System.currentTimeMillis - _.lastMessage  < limit)
    if (activeRecently.nonEmpty) {
      Seq(activeRecently(Random.nextInt(activeRecently.length)))
    } else {
      Nil
    }
  }
}

case object Broadcast extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = peers
}

case class SendToPeer(chosenPeer: ConnectedPeer) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = Seq(chosenPeer)
}

case class SendToPeers(chosenPeers: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = chosenPeers
}
