package org.ergoplatform.network

import akka.actor.ActorSystem
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.consensus.History.Older
import scorex.core.network.{ConnectedPeer, ConnectionId, Incoming}
import scorex.core.network.peer.PeerInfo

class ErgoSyncTrackerSpecification extends ErgoPropertyTest {
  property("getters test") {
    val time = 10L
    val peerInfo = PeerInfo(defaultPeerSpec, time, Some(Incoming))
    val cid = ConnectionId(inetAddr1, inetAddr2, Incoming)
    val connectedPeer = ConnectedPeer(cid, handlerRef = null, lastMessage = 5L, Some(peerInfo))
    val syncTracker = ErgoSyncTracker(ActorSystem(), settings.scorexSettings.network, timeProvider)

    val status = Older
    val height = 1000
    syncTracker.updateStatus(connectedPeer, status, Some(height))

    syncTracker.getStatus(connectedPeer) shouldBe Some(status)
    syncTracker.peersByStatus.apply(status).head shouldBe connectedPeer
    syncTracker.isOutdated(connectedPeer) shouldBe true
    syncTracker.peersToSyncWith().head shouldBe connectedPeer

    syncTracker.clearStatus(connectedPeer.connectionId.remoteAddress)
    syncTracker.getStatus(connectedPeer) shouldBe None
    syncTracker.peersByStatus.isEmpty shouldBe true
    syncTracker.lastSyncSentTime.get(connectedPeer) shouldBe None
    syncTracker.peersToSyncWith().length shouldBe 0
  }
}
