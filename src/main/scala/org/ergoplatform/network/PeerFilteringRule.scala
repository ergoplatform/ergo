package org.ergoplatform.network

import org.ergoplatform.nodeView.state.StateType
import scorex.core.app.Version
import scorex.core.network.ConnectedPeer

/**
  * Basic abstract component describing an action of choosing peers from available ones
  * based on peer version (and other properties).
  */
sealed trait PeerFilteringRule {

  /**
    * @param version - peer version
    * @return whether peer of this version should be selected
    */
  def condition(version: Version): Boolean

  /**
    * @param peer - peer
    * @return - whether the peer should be selected
    */
  def condition(peer: ConnectedPeer): Boolean = {
    val version = peer.peerInfo.map(_.peerSpec.protocolVersion).getOrElse(Version.initial)
    condition(version)
  }

  /**
    * Select peers satisfying the condition from provided ones
    * @param peers - unfiltered peers
    * @return filtered peers
    */
  def filter(peers: Iterable[ConnectedPeer]): Iterable[ConnectedPeer] = {
    peers.filter(cp => condition(cp))
  }

}


/**
  * 4.0.22+ allow for downloading ADProofs that are too big in block at 667614
  * for prior versions, a peer will not deliver block # 667614 and some other blocks
  */
object DigestModeFilter extends PeerFilteringRule {

  override def condition(version: Version): Boolean = {
      version.compare(Version.v4022) >= 0
  }

}

/**
  * Filter out peers of 4.0.17 or 4.0.18 version as they are delivering broken block sections
  */
object BrokenModifiersFilter extends PeerFilteringRule {

  override def condition(version: Version): Boolean = {
    version != Version.v4017 && version != Version.v4018
  }

}

/**
  * Filter to download block sections, combining `DigestModeFilter` and `BrokenModifiersFilter`
  * @param stateType - own (node's) state type
  */
final case class BlockSectionsDownloadFilter(stateType: StateType) extends PeerFilteringRule {
  override def condition(version: Version): Boolean = {
    if (stateType == StateType.Digest) {
      DigestModeFilter.condition(version)
    } else {
      BrokenModifiersFilter.condition(version)
    }
  }
}

/**
  * If peer's version is >= 4.0.16, the peer is supporting sync V2
  */
object SyncV2Filter extends PeerFilteringRule {

  override def condition(version: Version): Boolean = {
    val syncV2Version = Version(4, 0, 16)
    version.compare(syncV2Version) >= 0
  }

}

/**
  * Filter used to differentiate peers supporting UTXO state snapshots, so possibly
  * storing and serving them, from peers do not supporting UTXO set snapshots related networking protocol
  */
object UtxoSetNetworkingFilter extends PeerFilteringRule {
  val UtxoSnapsnotActivationVersion = Version(5, 0, 12)

  def condition(version: Version): Boolean = {
    // If neighbour version is >= `UtxoSnapsnotActivationVersion`, the neighbour supports utxo snapshots exchange
    version.compare(UtxoSnapsnotActivationVersion) >= 0
  }
}
