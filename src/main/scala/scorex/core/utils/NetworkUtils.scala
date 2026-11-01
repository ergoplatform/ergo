package scorex.core.utils

import java.net.{Inet4Address, Inet6Address, InetAddress, InetSocketAddress, NetworkInterface}
import scala.collection.JavaConverters._

object NetworkUtils {

  def getListenAddresses(bindAddress: InetSocketAddress): Set[InetSocketAddress] = {
    if (bindAddress.getAddress.isAnyLocalAddress || bindAddress.getAddress.isLoopbackAddress) {
      NetworkInterface.getNetworkInterfaces.asScala
        .flatMap(_.getInetAddresses.asScala)
        .collect { case a: Inet4Address => a}
        .map(a => new InetSocketAddress(a, bindAddress.getPort))
        .toSet
    } else {
      Set(bindAddress)
    }
  }

  def isSelf(peerAddress: InetSocketAddress,
             bindAddress: InetSocketAddress,
             externalNodeAddress: Option[InetSocketAddress]): Boolean = {
    NetworkUtils.getListenAddresses(bindAddress).contains(peerAddress) ||
      externalNodeAddress.contains(peerAddress)
  }

  /** Java's site-local predicate excludes IPv6 unique-local addresses. */
  def isUniqueLocalIp(address: InetAddress): Boolean = address match {
    case ipv6: Inet6Address => (ipv6.getAddress.head & 0xfe) == 0xfc
    case _ => false
  }

  def isLocalIp(address: InetAddress): Boolean =
    address.isSiteLocalAddress || address.isLinkLocalAddress || address.isLoopbackAddress ||
      isUniqueLocalIp(address)

  /** When allowLocal is true, all addresses are allowed. */
  def isLocal(address: InetSocketAddress, allowLocal: Boolean): Boolean = {
    if (!allowLocal) {
      isLocalIp(address.getAddress)
    } else {
      false
    }
  }

}
