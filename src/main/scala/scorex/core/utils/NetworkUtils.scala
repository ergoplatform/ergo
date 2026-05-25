package scorex.core.utils

import java.net.{Inet4Address, InetSocketAddress, NetworkInterface}
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

  /**
    * Check if a given address is a local address (site-local, link-local, or loopback).
    * When localOnly is true, all addresses are allowed.
    */
  def checkLocalOnly(address: InetSocketAddress, localOnly: Boolean): Boolean = {
    if (!localOnly) {
      val addr = address.getAddress
      addr.isSiteLocalAddress || addr.isLinkLocalAddress || addr.isLoopbackAddress
    } else {
      false
    }
  }

}
