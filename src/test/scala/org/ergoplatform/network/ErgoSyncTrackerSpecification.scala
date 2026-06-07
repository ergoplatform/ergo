package org.ergoplatform.network

import org.ergoplatform.consensus.{Equal, Fork, Older, Unknown, Younger}
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.core.network.{ConnectedPeer, ConnectionId, Incoming}
import org.ergoplatform.network.peer.PeerInfo

class ErgoSyncTrackerSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.tools.MinerBench._

  private def createPeer(name: String, localPort: Int): ConnectedPeer = {
    val peerInfo = PeerInfo(defaultPeerSpec, System.currentTimeMillis(), Some(Incoming), 5L)
    val localAddr = new java.net.InetSocketAddress("127.0.0.1", localPort)
    val remoteAddr = new java.net.InetSocketAddress("127.0.0.1", localPort + 1000)
    val cid = ConnectionId(localAddr, remoteAddr, Incoming)
    ConnectedPeer(cid, handlerRef = null, Some(peerInfo))
  }

  property("getters test") {
    val time = 10L
    val peerInfo = PeerInfo(defaultPeerSpec, time, Some(Incoming), 5L)
    val cid = ConnectionId(inetAddr1, inetAddr2, Incoming)
    val connectedPeer = ConnectedPeer(cid, handlerRef = null, Some(peerInfo))
    val syncTracker = ErgoSyncTracker(settings.scorexSettings.network)

    val height = 1000
    // add peer to sync
    syncTracker.updateStatus(connectedPeer, Younger, Some(height))
    syncTracker.maxHeight() shouldBe Some(height)
    syncTracker.statuses(connectedPeer) shouldBe ErgoPeerStatus(connectedPeer, Younger, height, None, None)
    // updating status should change status and height of existing peer
    syncTracker.updateStatus(connectedPeer, Older, Some(height+1))
    syncTracker.maxHeight() shouldBe Some(height + 1)
    syncTracker.getStatus(connectedPeer) shouldBe Some(Older)

    syncTracker.peersByStatus.apply(Older).head shouldBe connectedPeer
    // peer should not be synced yet
    syncTracker.notSyncedOrOutdated(connectedPeer) shouldBe true
    syncTracker.outdatedPeers shouldBe Vector.empty
    // peer should be ready for sync
    syncTracker.peersToSyncWith().head shouldBe connectedPeer
    syncTracker.updateLastSyncSentTime(connectedPeer)
    // peer should be synced now
    syncTracker.notSyncedOrOutdated(connectedPeer) shouldBe false

    syncTracker.clearStatus(connectedPeer)
    // peer should not be tracked anymore
    syncTracker.getStatus(connectedPeer) shouldBe None
    syncTracker.peersByStatus.isEmpty shouldBe true
    syncTracker.statuses.get(connectedPeer) shouldBe None
    syncTracker.peersToSyncWith().length shouldBe 0
    syncTracker.maxHeight() shouldBe None

    // clearStatus() is ok when there's no peer
    syncTracker.clearStatus(connectedPeer)
    syncTracker.getStatus(connectedPeer) shouldBe None
    syncTracker.maxHeight() shouldBe None
  }

  property("peersByStatus should group peers by their chain status") {
    val syncTracker = ErgoSyncTracker(settings.scorexSettings.network)

    val olderPeer = createPeer("older", 9001)
    val equalPeer = createPeer("equal", 9002)
    val youngerPeer = createPeer("younger", 9003)
    val unknownPeer = createPeer("unknown", 9004)
    val forkPeer = createPeer("fork", 9005)

    syncTracker.updateStatus(olderPeer, Older, Some(1000))
    syncTracker.updateStatus(equalPeer, Equal, Some(1000))
    syncTracker.updateStatus(youngerPeer, Younger, Some(500))
    syncTracker.updateStatus(unknownPeer, Unknown, None)
    syncTracker.updateStatus(forkPeer, Fork, Some(1000))

    val peersByStatus = syncTracker.peersByStatus

    peersByStatus(Older) should contain(olderPeer)
    peersByStatus(Equal) should contain(equalPeer)
    peersByStatus(Younger) should contain(youngerPeer)
    peersByStatus(Unknown) should contain(unknownPeer)
    peersByStatus(Fork) should contain(forkPeer)

    peersByStatus(Older).size shouldBe 1
    peersByStatus(Equal).size shouldBe 1
    peersByStatus(Younger).size shouldBe 1
    peersByStatus(Unknown).size shouldBe 1
    peersByStatus(Fork).size shouldBe 1
  }

  property("peersByStatus should update when peer status changes") {
    val syncTracker = ErgoSyncTracker(settings.scorexSettings.network)
    val peer = createPeer("peer", 9001)

    syncTracker.updateStatus(peer, Older, Some(1000))
    syncTracker.peersByStatus(Older) should contain(peer)
    syncTracker.peersByStatus.get(Equal) shouldBe None

    syncTracker.updateStatus(peer, Equal, Some(1000))
    syncTracker.peersByStatus.get(Older) shouldBe None
    syncTracker.peersByStatus(Equal) should contain(peer)

    syncTracker.clearStatus(peer)
    syncTracker.peersByStatus.get(Equal) shouldBe None
  }

  property("peersByStatus should return empty for unknown statuses") {
    val syncTracker = ErgoSyncTracker(settings.scorexSettings.network)
    syncTracker.peersByStatus.get(Older) shouldBe None
    syncTracker.peersByStatus.get(Equal) shouldBe None
    syncTracker.peersByStatus.isEmpty shouldBe true
  }
}
