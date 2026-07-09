package org.ergoplatform.network.peer

import java.net.{InetAddress, InetSocketAddress}

import org.ergoplatform.settings.NetworkType
import org.ergoplatform.utils.ErgoCorePropertyTest

class PeerAddressFilterSpec extends ErgoCorePropertyTest {

  // Use getByName so the hostname is parsed without a DNS lookup (literal IPs only).
  private def sa(host: String): InetSocketAddress =
    new InetSocketAddress(InetAddress.getByName(host), 9030)

  property("always-bogus: loopback") {
    PeerAddressFilter.isBogus(sa("127.0.0.1"), NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("127.0.0.1"), NetworkType.TestNet) shouldBe true
    PeerAddressFilter.isBogus(sa("::1"),       NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("::1"),       NetworkType.TestNet) shouldBe true
  }

  property("always-bogus: link-local (169.254/16, fe80::/10)") {
    PeerAddressFilter.isBogus(sa("169.254.0.2"),   NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("169.254.0.2"),   NetworkType.TestNet) shouldBe true
    PeerAddressFilter.isBogus(sa("fe80::1"),       NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("fe80::1"),       NetworkType.TestNet) shouldBe true
  }

  property("always-bogus: multicast (224/4, ff00::/8)") {
    PeerAddressFilter.isBogus(sa("224.0.0.1"),  NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("239.0.0.1"),  NetworkType.TestNet) shouldBe true
    PeerAddressFilter.isBogus(sa("ff02::1"),    NetworkType.MainNet) shouldBe true
  }

  property("always-bogus: unspecified (0.0.0.0, ::)") {
    PeerAddressFilter.isBogus(sa("0.0.0.0"), NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("0.0.0.0"), NetworkType.TestNet) shouldBe true
    PeerAddressFilter.isBogus(sa("::"),      NetworkType.MainNet) shouldBe true
  }

  property("always-bogus: broadcast / reserved Class E (240/4)") {
    PeerAddressFilter.isBogus(sa("255.255.255.255"), NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("240.0.0.1"),       NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("254.255.255.255"), NetworkType.TestNet) shouldBe true
  }

  property("always-bogus: benchmark range (198.18/15)") {
    PeerAddressFilter.isBogus(sa("198.18.0.1"),  NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("198.19.0.1"),  NetworkType.TestNet) shouldBe true
    PeerAddressFilter.isBogus(sa("198.20.0.1"),  NetworkType.MainNet) shouldBe false
    PeerAddressFilter.isBogus(sa("198.17.0.1"),  NetworkType.MainNet) shouldBe false
  }

  property("mainnet-only-bogus: RFC 1918 private (10/8, 172.16/12, 192.168/16)") {
    Seq("10.0.0.1", "172.16.0.1", "172.31.255.255", "192.168.1.1").foreach { ip =>
      PeerAddressFilter.isBogus(sa(ip), NetworkType.MainNet) shouldBe true
      PeerAddressFilter.isBogus(sa(ip), NetworkType.TestNet) shouldBe false
    }
    // 172.32.0.0 is NOT in RFC 1918, must NOT be bogus
    PeerAddressFilter.isBogus(sa("172.32.0.1"), NetworkType.MainNet) shouldBe false
  }

  property("mainnet-only-bogus: CGN (100.64.0.0/10)") {
    PeerAddressFilter.isBogus(sa("100.64.0.1"),    NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("100.127.255.1"), NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("100.64.0.1"),    NetworkType.TestNet) shouldBe false
    // 100.63.x.x and 100.128.x.x are public
    PeerAddressFilter.isBogus(sa("100.63.0.1"),  NetworkType.MainNet) shouldBe false
    PeerAddressFilter.isBogus(sa("100.128.0.1"), NetworkType.MainNet) shouldBe false
  }

  property("mainnet-only-bogus: IPv6 ULA (fc00::/7)") {
    PeerAddressFilter.isBogus(sa("fc00::1"), NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("fd00::1"), NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("fc00::1"), NetworkType.TestNet) shouldBe false
  }

  property("mainnet-only-bogus: documentation ranges") {
    Seq("192.0.2.1", "198.51.100.1", "203.0.113.1").foreach { ip =>
      PeerAddressFilter.isBogus(sa(ip), NetworkType.MainNet) shouldBe true
      PeerAddressFilter.isBogus(sa(ip), NetworkType.TestNet) shouldBe false
    }
    PeerAddressFilter.isBogus(sa("2001:db8::1"), NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(sa("2001:db8::1"), NetworkType.TestNet) shouldBe false
  }

  property("public addresses are not bogus on any network") {
    Seq(
      "8.8.8.8",
      "1.1.1.1",
      "213.239.193.208",  // Kushti's seed
      "2001:41d0:700:6662::",  // OVH IPv6
      "2606:4700:4700::1111"   // Cloudflare DNS IPv6
    ).foreach { ip =>
      PeerAddressFilter.isBogus(sa(ip), NetworkType.MainNet) shouldBe false
      PeerAddressFilter.isBogus(sa(ip), NetworkType.TestNet) shouldBe false
    }
  }

  property("v4-mapped IPv6 byte form is auto-normalized to Inet4Address by the JVM") {
    // ::ffff:1.2.3.4 byte form — InetAddress.getByAddress auto-normalizes this
    // to Inet4Address(1.2.3.4). The IPv4 path then governs whether it's bogus.
    // 1.2.3.4 is public, so this is NOT bogus on either network — i.e., the
    // wire-form v4-mapped representation is transparent and reuses our IPv4
    // bogus checks.
    val v4MappedBytes = Array.fill[Byte](10)(0) ++ Array[Byte](0xff.toByte, 0xff.toByte, 1, 2, 3, 4)
    val asV4 = new InetSocketAddress(InetAddress.getByAddress(v4MappedBytes), 9030)
    PeerAddressFilter.isBogus(asV4, NetworkType.MainNet) shouldBe false
    PeerAddressFilter.isBogus(asV4, NetworkType.TestNet) shouldBe false

    // And the same byte form representing a private IPv4 (192.168.1.1) DOES
    // get caught — by the mainnet-only private-network check after normalization.
    val v4MappedPriv = Array.fill[Byte](10)(0) ++ Array[Byte](0xff.toByte, 0xff.toByte, 192.toByte, 168.toByte, 1, 1)
    val asV4Priv = new InetSocketAddress(InetAddress.getByAddress(v4MappedPriv), 9030)
    PeerAddressFilter.isBogus(asV4Priv, NetworkType.MainNet) shouldBe true
    PeerAddressFilter.isBogus(asV4Priv, NetworkType.TestNet) shouldBe false
  }
}
