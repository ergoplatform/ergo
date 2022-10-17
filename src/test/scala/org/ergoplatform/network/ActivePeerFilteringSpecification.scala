package org.ergoplatform.network

import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.network.peer.PeerInfo
import scorex.core.network.peer.PeerManager.ReceivableMessages.SeenPeers

import java.net.{InetAddress, InetSocketAddress}

class ActivePeerFilteringSpecification extends ErgoPropertyTest {

  private val filter: SeenPeers = SeenPeers(5)

  private def newPeer(address: String, port: Int, lastHandshakeOffset: Long): (InetSocketAddress, PeerInfo) = {
    val addr = new InetSocketAddress(address, port)
    (addr, PeerInfo(defaultPeerSpec.copy(declaredAddress = Some(addr)), System.currentTimeMillis() - lastHandshakeOffset))
  }

  private val knownPeers: Map[InetSocketAddress, PeerInfo] = Map(
    newPeer("1.2.3.4", 1234, 12340), // blacklisted
    newPeer("2.3.4.1", 2341, 23410),
    newPeer("3.4.1.2", 3412, 34120),
    newPeer("4.1.2.3", 4123, 41230 + filter.limit), // over limit
  )

  private val blacklistedPeers: Seq[InetAddress] = Seq(InetAddress.getByName("1.2.3.4"))

  private val correct: Seq[PeerInfo] = knownPeers.toSeq.slice(1,3).map(_._2)

  property("time based activity filter") {

    val result: Seq[PeerInfo] =
      filter.choose(knownPeers, blacklistedPeers, null).sortBy(-_.lastHandshake) // sort to undo Random.shuffle

    result shouldBe correct
  }

}
