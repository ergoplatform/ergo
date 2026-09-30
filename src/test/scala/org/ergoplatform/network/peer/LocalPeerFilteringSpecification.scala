package org.ergoplatform.network.peer

import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.core.utils.NetworkUtils

import java.net.InetSocketAddress

class LocalPeerFilteringSpecification extends ErgoCorePropertyTest {

  private val remoteAddress = new InetSocketAddress("8.8.8.8", 9001)
  private val siteLocalAddress = new InetSocketAddress("192.168.1.1", 9002)
  private val linkLocalAddress = new InetSocketAddress("169.254.1.1", 9003)
  private val loopbackAddress = new InetSocketAddress("127.0.0.1", 9004)

  property("local addresses are correctly classified") {
    NetworkUtils.isLocal(remoteAddress, allowLocal = false) shouldBe false

    NetworkUtils.isLocal(siteLocalAddress, allowLocal = false) shouldBe true
    NetworkUtils.isLocal(linkLocalAddress, allowLocal = false) shouldBe true
    NetworkUtils.isLocal(loopbackAddress, allowLocal = false) shouldBe true

    NetworkUtils.isLocal(siteLocalAddress, allowLocal = true) shouldBe false
  }

}
