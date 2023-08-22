package org.ergoplatform.network

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

    peer.mode.flatMap(_.nipopowBootstrapped).isEmpty &&
      version.compare(Version.NipopowActivationVersion) >= 0
  }

}

/**
  * Filter to download block sections (except of headers).
  * Currently, it is accepting peers which do have all the full blocks (so not bootstrapped via UTXO set snapshot
  * or stateless client with full blocks suffix)
  */
object BlockSectionsDownloadFilter extends PeerFilteringRule {
  override def condition(peer: ConnectedPeer): Boolean = {
    peer.mode.exists(_.allBlocksAvailable)
  }
}

/**
  * Filter to download headers.
  * Currently, it is accepting peers which do have all the headers (so not bootstrapped via NiPoPoWs.
  */
object HeadersDownloadFilter extends PeerFilteringRule {
  override def condition(peer: ConnectedPeer): Boolean = {
    peer.mode.exists(_.allHeadersAvailable)
  }
}
