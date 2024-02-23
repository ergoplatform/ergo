package org.ergoplatform.network

import akka.actor.ActorRef
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.network.peer.PeerInfo
import scorex.core.network.{ConnectedPeer, ConnectionId}

class PeerFilteringRuleSpecification extends ErgoPropertyTest {

  private def peerWithVersion(version: Version): ConnectedPeer = {
    val ref = ActorRef.noSender
    val peerSpec = PeerSpec("", version, "", None, Seq.empty)
    val peerInfo = PeerInfo(peerSpec, lastHandshake = 0L, None, 0L)
    ConnectedPeer(ConnectionId(null, null, null), ref, Some(peerInfo))
  }

  property("syncv2 filter") {
    val v1Peer = peerWithVersion(Version(4, 0, 15))
    val v2Peer0 = peerWithVersion(Version(4, 0, 16))
    val v2Peer1 = peerWithVersion(Version(4, 0, 21))
    val v2Peer2 = peerWithVersion(Version(5, 0, 0))

    SyncV2Filter.filter(Seq(v1Peer, v2Peer0, v2Peer1, v2Peer2)) shouldBe Seq(v2Peer0, v2Peer1, v2Peer2)
  }

  property("utxo set snapshot filter") {
    val peer0 = peerWithVersion(Version(4, 0, 17))
    val peer1 = peerWithVersion(Version(4, 0, 18))
    val peer2 = peerWithVersion(Version(4, 0, 16))
    val peer3 = peerWithVersion(Version(4, 0, 19))
    val peer4 = peerWithVersion(Version(5, 0, 0))
    val peer5 = peerWithVersion(Version(5, 0, 5))
    val peer6 = peerWithVersion(Version(5, 0, 15))
    val peer7 = peerWithVersion(Version(5, 0, 25))

    UtxoSetNetworkingFilter.filter(Seq(peer0, peer1, peer2, peer3, peer4, peer5, peer6, peer7)) shouldBe
      Seq(peer6, peer7)
  }

  property("nipopow support filter") {
    val peer0 = peerWithVersion(Version(4, 0, 17))
    val peer1 = peerWithVersion(Version(4, 0, 18))
    val peer2 = peerWithVersion(Version(4, 0, 16))
    val peer3 = peerWithVersion(Version(4, 0, 19))
    val peer4 = peerWithVersion(Version(5, 0, 0))
    val peer5 = peerWithVersion(Version(5, 0, 12))
    val peer6 = peerWithVersion(Version(5, 0, 13))
    val peer7 = peerWithVersion(Version(5, 0, 25))

    NipopowSupportFilter.filter(Seq(peer0, peer1, peer2, peer3, peer4, peer5, peer6, peer7)) shouldBe
      Seq(peer6, peer7)
  }

}
