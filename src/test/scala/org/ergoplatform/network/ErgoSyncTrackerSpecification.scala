package org.ergoplatform.network

import akka.actor.ActorSystem
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.consensus.History.{Older, Younger}
import scorex.core.network.{ConnectedPeer, ConnectionId, Incoming}
import scorex.core.network.peer.PeerInfo

class ErgoSyncTrackerSpecification extends ErgoPropertyTest {
  property("getters test") {
    val time = 10L
    val peerInfo = PeerInfo(defaultPeerSpec, time, Some(Incoming))
    val cid = ConnectionId(inetAddr1, inetAddr2, Incoming)
    val connectedPeer = ConnectedPeer(cid, handlerRef = null, lastMessage = 5L, Some(peerInfo))
    val syncTracker = ErgoSyncTracker(ActorSystem(), settings.scorexSettings.network, timeProvider)

    val height = 1000
    // add peer to sync
    syncTracker.updateStatus(connectedPeer, Younger, Some(height))
    syncTracker.maxHeight() shouldBe Some(height)
    syncTracker.statuses(connectedPeer) shouldBe ErgoPeerStatus(connectedPeer, Younger, height, None, None)
    // updating status should change status and height of existing peer
    syncTracker.updateStatus(connectedPeer, Older, Some(height+1))
    syncTracker.maxHeight() shouldBe Some(height + 1)
    syncTracker.getStatus(connectedPeer) shouldBe Some(Older)
    syncTracker.fullInfo().head.height shouldBe height+1

    syncTracker.peersByStatus.apply(Older).head shouldBe connectedPeer
    // peer should not be synced yet
    syncTracker.notSyncedOrOutdated(connectedPeer) shouldBe true
    syncTracker.outdatedPeers shouldBe Vector.empty
    // peer should be ready for sync
    syncTracker.peersToSyncWith().head shouldBe connectedPeer
    syncTracker.updateLastSyncSentTime(connectedPeer)
    // peer should be synced now
    syncTracker.notSyncedOrOutdated(connectedPeer) shouldBe false
    syncTracker.clearStatus(connectedPeer.connectionId.remoteAddress)
    // peer should not be tracked anymore
    syncTracker.getStatus(connectedPeer) shouldBe None
    syncTracker.peersByStatus.isEmpty shouldBe true
    syncTracker.statuses.get(connectedPeer) shouldBe None
    syncTracker.peersToSyncWith().length shouldBe 0
  }
}
