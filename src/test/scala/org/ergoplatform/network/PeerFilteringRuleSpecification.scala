package org.ergoplatform.network

import akka.actor.ActorRef
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.app.Version
import scorex.core.network.PeerSpec
import scorex.core.network.peer.PeerInfo
import scorex.core.network.{ConnectedPeer, ConnectionId}

class PeerFilteringRuleSpecification extends ErgoPropertyTest {

  private def peerWithVersion(version: Version): ConnectedPeer = {
    val ref = ActorRef.noSender
    val peerSpec = PeerSpec("", version, "", None, Seq.empty)
    val peerInfo = PeerInfo(peerSpec, lastHandshake = 0L, None)
    ConnectedPeer(ConnectionId(null, null, null), ref, lastMessage = 0L, Some(peerInfo))
  }

  property("syncv2 filter") {
    val v1Peer = peerWithVersion(Version(4, 0, 15))
    val v2Peer0 = peerWithVersion(Version(4, 0, 16))
    val v2Peer1 = peerWithVersion(Version(4, 0, 21))
    val v2Peer2 = peerWithVersion(Version(5, 0, 0))

    SyncV2Filter.filter(Seq(v1Peer, v2Peer0, v2Peer1, v2Peer2)) shouldBe Seq(v2Peer0, v2Peer1, v2Peer2)
  }

  property("digest mode filter") {
    val beforePeer = peerWithVersion(Version(4, 0, 21))
    val afterPeer0 = peerWithVersion(Version(4, 0, 22))
    val afterPeer1 = peerWithVersion(Version(5, 0, 0))

    DigestModeFilter.filter(Seq(beforePeer, afterPeer0, afterPeer1)) shouldBe Seq(afterPeer0, afterPeer1)
  }

  property("broken modifiers filter") {
    val inPeer0 = peerWithVersion(Version(4, 0, 17))
    val inPeer1 = peerWithVersion(Version(4, 0, 18))
    val outPeer0 = peerWithVersion(Version(4, 0, 16))
    val outPeer1 = peerWithVersion(Version(4, 0, 19))
    val outPeer2 = peerWithVersion(Version(5, 0, 0))

    BrokenModifiersFilter.filter(Seq(inPeer0, inPeer1, outPeer0, outPeer1, outPeer2)) shouldBe
      Seq(outPeer0, outPeer1, outPeer2)
  }

  property("utxo set snapshot filter") {
    val inPeer0 = peerWithVersion(Version(4, 0, 17))
    val inPeer1 = peerWithVersion(Version(4, 0, 18))
    val outPeer0 = peerWithVersion(Version(4, 0, 16))
    val outPeer1 = peerWithVersion(Version(4, 0, 19))
    val outPeer2 = peerWithVersion(Version(5, 0, 0))
    val outPeer3 = peerWithVersion(Version(5, 0, 5))

    UtxoSetNetworkingFilter.filter(Seq(inPeer0, inPeer1, outPeer0, outPeer1, outPeer2, outPeer3)) shouldBe
      Seq(outPeer2, outPeer3)
  }

}
