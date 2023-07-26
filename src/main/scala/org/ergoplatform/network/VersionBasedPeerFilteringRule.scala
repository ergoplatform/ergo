package org.ergoplatform.network

import org.ergoplatform.nodeView.state.StateType
import scorex.core.app.Version
import scorex.core.network.ConnectedPeer

/**
  * Basic interface for a filter describing an action of choosing peers from available ones
  * based on peer capabilities.
  */
sealed trait PeerFilteringRule {
  /**
    * @param peer - peer
    * @return - whether the peer should be selected
    */
  def condition(peer: ConnectedPeer): Boolean

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
  * Basic interface for filters based on peer version only
  */
trait VersionBasedPeerFilteringRule extends PeerFilteringRule {

  /**
    * @param version - peer version
    * @return whether peer of this version should be selected
    */
  def condition(version: Version): Boolean

  /**
    * @param peer - peer
    * @return - whether the peer should be selected
    */
  override def condition(peer: ConnectedPeer): Boolean = {
    val version = peer.peerInfo.map(_.peerSpec.protocolVersion).getOrElse(Version.initial)
    condition(version)
  }

}


/**
  * 4.0.22+ allow for downloading ADProofs that are too big in block at 667614
  * for prior versions, a peer will not deliver block # 667614 and some other blocks
  */
object DigestModeFilter extends VersionBasedPeerFilteringRule {

  override def condition(version: Version): Boolean = {
      version.compare(Version.v4022) >= 0
  }

}

/**
  * Filter out peers of 4.0.17 or 4.0.18 version as they are delivering broken block sections
  */
object BrokenModifiersFilter extends VersionBasedPeerFilteringRule {

  override def condition(version: Version): Boolean = {
    version != Version.v4017 && version != Version.v4018
  }

}

/**
  * Filter to download block sections, combining `DigestModeFilter` and `BrokenModifiersFilter`
  * @param stateType - own (node's) state type
  */
final case class BlockSectionsDownloadFilter(stateType: StateType) extends VersionBasedPeerFilteringRule {
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
object SyncV2Filter extends VersionBasedPeerFilteringRule {
  private val syncV2Version = Version(4, 0, 16)

  override def condition(version: Version): Boolean = {
    version.compare(syncV2Version) >= 0
  }

}


/**
  * Filter used to differentiate peers supporting UTXO state snapshots, so possibly
  * storing and serving them, from peers do not supporting UTXO set snapshots related networking protocol
  */
object UtxoSetNetworkingFilter extends VersionBasedPeerFilteringRule {

  def condition(version: Version): Boolean = {
    // If neighbour version is >= `UtxoSnapsnotActivationVersion`, the neighbour supports utxo snapshots exchange
    version.compare(Version.UtxoSnapsnotActivationVersion) >= 0
  }

}

/**
  * Filter which selects peers NOT bootstrapped via NiPoPoWs (so peers having all the headers),
  * and also having version supporting bootstrapping with NiPoPoWs
  */
object NipopowSupportFilter extends PeerFilteringRule {

  /**
    * @param peer - peer
    * @return - whether the peer should be selected
    */
  override def condition(peer: ConnectedPeer): Boolean = {
    val version = peer.peerInfo.map(_.peerSpec.protocolVersion).getOrElse(Version.initial)

    peer.mode.flatMap(_.nipopowSuffix).isEmpty &&
      version.compare(Version.NipopowActivationVersion) >= 0
  }

}
