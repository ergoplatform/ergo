package org.ergoplatform.network.peer

import org.ergoplatform.utils.ErgoCorePropertyTest

import java.net.InetSocketAddress

class LocalPeerFilteringSpecification extends ErgoCorePropertyTest {

  private val remoteAddress = new InetSocketAddress("8.8.8.8", 9001)
  private val siteLocalAddress = new InetSocketAddress("192.168.1.1", 9002)
  private val linkLocalAddress = new InetSocketAddress("169.254.1.1", 9003)
  private val loopbackAddress = new InetSocketAddress("127.0.0.1", 9004)

  property("local addresses are correctly classified") {
    // Verify that the addresses we test with are indeed classified correctly
    remoteAddress.getAddress.isSiteLocalAddress shouldBe false
    remoteAddress.getAddress.isLinkLocalAddress shouldBe false
    remoteAddress.getAddress.isLoopbackAddress shouldBe false

    siteLocalAddress.getAddress.isSiteLocalAddress shouldBe true
    linkLocalAddress.getAddress.isLinkLocalAddress shouldBe true
    loopbackAddress.getAddress.isLoopbackAddress shouldBe true
  }

}
