package scorex.core.network

import scala.util.Random

trait SendingStrategy {
  def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer]
}

object SendToRandom extends SendingStrategy {

  private val dayInMs: Long = 24 * 60 * 60 * 1000

  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = {
    val activeRecently: Seq[ConnectedPeer] = // choose peers which were active in the last 24 hours
      peers.filter(p => p.peerInfo.isDefined && System.currentTimeMillis - p.peerInfo.get.lastHandshake  < dayInMs)
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
