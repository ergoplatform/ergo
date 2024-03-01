package org.ergoplatform.network

import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.SeenPeers

import java.net.{InetAddress, InetSocketAddress}

class ActivePeerFilteringSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants.defaultPeerSpec

  private val filter: SeenPeers = SeenPeers(5)

  private def newPeer(address: String, port: Int, lastHandshakeOffset: Long): (InetSocketAddress, PeerInfo) = {
    val addr = new InetSocketAddress(address, port)
    val pi = PeerInfo(
      defaultPeerSpec.copy(declaredAddress = Some(addr)),
      System.currentTimeMillis() - lastHandshakeOffset,
      None,
      System.currentTimeMillis() - lastHandshakeOffset
    )
    (addr, pi)
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

    result.size shouldBe 2
    result shouldBe correct
  }

}
