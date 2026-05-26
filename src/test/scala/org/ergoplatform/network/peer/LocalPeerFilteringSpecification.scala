package org.ergoplatform.network.peer

import akka.testkit.TestProbe
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.{AddOrUpdatePeer, AddPeerIfEmpty, GetAllPeers}
import org.ergoplatform.network.{PeerSpec, Version}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoNodeTestConstants
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.core.app.ScorexContext
import scorex.core.utils.NetworkUtils
import scorex.testkit.utils.AkkaFixture

import java.nio.file.Files
import java.net.InetSocketAddress
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class LocalPeerFilteringSpecification extends ErgoCorePropertyTest {

  private val remoteAddress = new InetSocketAddress("8.8.8.8", 9001)
  private val siteLocalAddress = new InetSocketAddress("192.168.1.1", 9002)
  private val linkLocalAddress = new InetSocketAddress("169.254.1.1", 9003)
  private val loopbackAddress = new InetSocketAddress("127.0.0.1", 9004)

  private def settingsWith(knownPeers: Seq[InetSocketAddress], localOnly: Boolean): ErgoSettings = {
    val base = ErgoNodeTestConstants.settings
    val networkSettings = base.scorexSettings.network.copy(
      knownPeers = knownPeers,
      localOnly = localOnly
    )
    base.copy(
      directory = Files.createTempDirectory("ergo-peer-filtering").toFile.getAbsolutePath,
      scorexSettings = base.scorexSettings.copy(network = networkSettings)
    )
  }

  private class PeerManagerFixture(ergoSettings: ErgoSettings) extends AkkaFixture {
    val probe: TestProbe = TestProbe()
    val scorexContext: ScorexContext = ScorexContext(Seq.empty, None, None)
    val peerManager = system.actorOf(PeerManagerRef.props(ergoSettings, scorexContext))

    def peers: Map[InetSocketAddress, PeerInfo] = {
      probe.send(peerManager, GetAllPeers)
      probe.expectMsgPF() {
        case peers: Map[_, _] => peers.asInstanceOf[Map[InetSocketAddress, PeerInfo]]
      }
    }
  }

  private def withPeerManager(ergoSettings: ErgoSettings)(testCode: PeerManagerFixture => Any): Unit = {
    val fixture = new PeerManagerFixture(ergoSettings)
    try {
      testCode(fixture)
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  private def peerSpec(address: InetSocketAddress): PeerSpec =
    PeerSpec("test", Version(6, 0, 0), "test", Some(address), Seq.empty)

  property("local addresses are correctly classified") {
    NetworkUtils.checkLocalOnly(remoteAddress, localOnly = false) shouldBe false

    NetworkUtils.checkLocalOnly(siteLocalAddress, localOnly = false) shouldBe true
    NetworkUtils.checkLocalOnly(linkLocalAddress, localOnly = false) shouldBe true
    NetworkUtils.checkLocalOnly(loopbackAddress, localOnly = false) shouldBe true

    NetworkUtils.checkLocalOnly(siteLocalAddress, localOnly = true) shouldBe false
  }

  property("peer manager does not seed local known peers when localOnly is disabled") {
    val ergoSettings = settingsWith(
      knownPeers = Seq(remoteAddress, siteLocalAddress, linkLocalAddress, loopbackAddress),
      localOnly = false
    )

    withPeerManager(ergoSettings) { fixture =>
      fixture.peers.keySet shouldBe Set(remoteAddress)
    }
  }

  property("peer manager purges local peers already stored in the database") {
    val ergoSettings = settingsWith(knownPeers = Seq.empty, localOnly = false)
    val peerDatabase = new PeerDatabase(ergoSettings)
    peerDatabase.addOrUpdateKnownPeer(PeerInfo.fromAddress(remoteAddress))
    peerDatabase.addOrUpdateKnownPeer(PeerInfo.fromAddress(siteLocalAddress))
    peerDatabase.addOrUpdateKnownPeer(PeerInfo.fromAddress(loopbackAddress))

    withPeerManager(ergoSettings) { fixture =>
      fixture.peers.keySet shouldBe Set(remoteAddress)
    }
  }

  property("peer manager rejects local peers received from peer messages") {
    val ergoSettings = settingsWith(knownPeers = Seq(remoteAddress), localOnly = false)

    withPeerManager(ergoSettings) { fixture =>
      fixture.probe.send(fixture.peerManager, AddOrUpdatePeer(PeerInfo.fromAddress(siteLocalAddress)))
      fixture.probe.send(fixture.peerManager, AddPeerIfEmpty(peerSpec(loopbackAddress)))

      fixture.peers.keySet shouldBe Set(remoteAddress)
    }
  }

}
