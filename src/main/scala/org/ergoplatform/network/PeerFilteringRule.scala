package org.ergoplatform.network

import scorex.core.app.Version
import scorex.core.network.ConnectedPeer

sealed trait PeerFilteringRule {

  def condition(version: Version): Boolean

  def condition(peer: ConnectedPeer): Boolean = {
    val version = peer.peerInfo.map(_.peerSpec.protocolVersion).getOrElse(Version.initial)
    condition(version)
  }

  def filter(peers: Iterable[ConnectedPeer]): Iterable[ConnectedPeer] = {
    peers.filter(cp => condition(cp))
  }

}


// 4.0.22+ allow for downloading ADProofs that are too big in block at 667614
// for prior versions, a peer will not deliver block # 667614 and some other blocks
object DigestModeFilter extends PeerFilteringRule {

  override def condition(version: Version): Boolean = {
      version.compare(Version.v4022) >= 0
  }

}

// filter out peers of 4.0.17 or 4.0.18 version as they are delivering broken modifiers
object BrokenModifiersFilter extends PeerFilteringRule {

  override def condition(version: Version): Boolean = {
    version != Version.v4017 && version != Version.v4018
  }

}

// If neighbour version is >= 4.0.16, the neighbour supports sync V2
object SyncV2Filter extends PeerFilteringRule {

  override def condition(version: Version): Boolean = {
    val syncV2Version = Version(4, 0, 16)
    version.compare(syncV2Version) >= 0
  }

}
