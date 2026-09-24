package org.ergoplatform.network.peer

import java.net.{Inet4Address, Inet6Address, InetAddress, InetSocketAddress}

import org.ergoplatform.settings.NetworkType

/**
  * Classifies peer addresses as "bogus" — unroutable or otherwise illegitimate
  * for a public Ergo peer to advertise via the `Peers` gossip message.
  *
  * Network-conditional: a private RFC 1918 address (e.g. 192.168.0.1) is bogus
  * on mainnet (no legitimate mainnet peer is reachable there) but may be valid
  * on a testnet running inside a developer's LAN.
  */
object PeerAddressFilter {

  /**
    * @return true if `addr` is bogus on the given `networkType`. Bogus entries
    *         should be dropped from incoming `Peers` gossip; gossiping bogus
    *         addresses earns a penalty (the gossiper is buggy or malicious).
    */
  def isBogus(addr: InetSocketAddress, networkType: NetworkType): Boolean = {
    Option(addr.getAddress).exists { ip =>
      isAlwaysBogus(ip) || (networkType.isMainNet && isMainnetOnlyBogus(ip))
    }
  }

  /**
    * Never a legitimate Ergo peer regardless of network. Note: IPv4-mapped IPv6
    * addresses (::ffff:0:0/96) are auto-normalized by `InetAddress.getByAddress`
    * to `Inet4Address`, so they fall through to the IPv4 path below — no
    * separate v4-mapped check is needed.
    */
  private def isAlwaysBogus(ip: InetAddress): Boolean = {
    ip.isLoopbackAddress ||      // 127.0.0.0/8, ::1
    ip.isLinkLocalAddress ||     // 169.254.0.0/16, fe80::/10
    ip.isMulticastAddress ||     // 224.0.0.0/4, ff00::/8
    ip.isAnyLocalAddress ||      // 0.0.0.0, ::
    isV4BroadcastOrReserved(ip)  // 240.0.0.0/4 (incl. 255.255.255.255), 198.18/15
  }

  /**
    * Bogus on mainnet, legitimate on testnet running inside a private network.
    * RFC 1918 / CGN / IPv6 ULA / documentation ranges.
    */
  private def isMainnetOnlyBogus(ip: InetAddress): Boolean = {
    ip.isSiteLocalAddress ||  // 10/8, 172.16/12, 192.168/16 (RFC 1918)
    isCgn(ip) ||              // 100.64.0.0/10 (RFC 6598)
    isUniqueLocal(ip) ||      // fc00::/7 (RFC 4193)
    isDocumentation(ip)       // 192.0.2/24, 198.51.100/24, 203.0.113/24, 2001:db8::/32
  }

  // 240.0.0.0/4 reserved (RFC 1112, includes 255.255.255.255 broadcast)
  // 198.18.0.0/15 benchmark (RFC 2544)
  private def isV4BroadcastOrReserved(ip: InetAddress): Boolean = ip match {
    case v4: Inet4Address =>
      val bytes = v4.getAddress
      val b0 = bytes(0) & 0xff
      val b1 = bytes(1) & 0xff
      (b0 & 0xf0) == 0xf0 ||                     // 240/4
        (b0 == 198 && (b1 == 18 || b1 == 19))    // 198.18/15
    case _ => false
  }

  // 100.64.0.0/10 — Carrier-Grade NAT (RFC 6598)
  private def isCgn(ip: InetAddress): Boolean = ip match {
    case v4: Inet4Address =>
      val bytes = v4.getAddress
      (bytes(0) & 0xff) == 100 && ((bytes(1) & 0xff) & 0xc0) == 0x40
    case _ => false
  }

  // fc00::/7 — IPv6 Unique Local Addresses (RFC 4193)
  private def isUniqueLocal(ip: InetAddress): Boolean = ip match {
    case v6: Inet6Address =>
      (v6.getAddress()(0) & 0xfe) == 0xfc
    case _ => false
  }

  // 192.0.2/24, 198.51.100/24, 203.0.113/24 (RFC 5737)
  // 2001:db8::/32 (RFC 3849)
  private def isDocumentation(ip: InetAddress): Boolean = ip match {
    case v4: Inet4Address =>
      val bytes = v4.getAddress
      val b0 = bytes(0) & 0xff
      val b1 = bytes(1) & 0xff
      val b2 = bytes(2) & 0xff
      (b0 == 192 && b1 == 0 && b2 == 2) ||
        (b0 == 198 && b1 == 51 && b2 == 100) ||
        (b0 == 203 && b1 == 0 && b2 == 113)
    case v6: Inet6Address =>
      val bytes = v6.getAddress
      (bytes(0) & 0xff) == 0x20 && (bytes(1) & 0xff) == 0x01 &&
        (bytes(2) & 0xff) == 0x0d && (bytes(3) & 0xff) == 0xb8
    case _ => false
  }
}
