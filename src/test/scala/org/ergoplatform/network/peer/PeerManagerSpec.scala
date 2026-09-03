package org.ergoplatform.network.peer

import akka.actor.ActorRef
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.db.DBSpec
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{
  DisconnectedPeer,
  HandshakedPeer
}
import org.ergoplatform.network.PeerSpec
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants._
import scorex.core.app.ScorexContext
import scorex.core.network.{ConnectionDirection, ConnectionId, ConnectedPeer, Incoming, Outgoing}
import scorex.testkit.utils.AkkaFixture

import java.io.File
import java.net.{InetAddress, InetSocketAddress}
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class PeerManagerSpec extends ErgoCorePropertyTest with DBSpec {

  import PeerManager.ReceivableMessages._

  private class PeerManagerFixture(knownPeers: Seq[InetSocketAddress] = Seq.empty) extends AkkaFixture {
    val dir: File = createTempDir

    val settings: ErgoSettings = {
      val base = initSettings.copy(directory = dir.getAbsolutePath)
      base.copy(
        scorexSettings = base.scorexSettings.copy(
          network = base.scorexSettings.network.copy(
            knownPeers = knownPeers
          )
        )
      )
    }

    val scorexContext: ScorexContext = ScorexContext(Seq.empty, None, None)
    val peerManager: TestActorRef[PeerManager] =
      TestActorRef(new PeerManager(settings, scorexContext))
  }

  private def withFixture(testCode: PeerManagerFixture => Any): Unit = {
    val f = new PeerManagerFixture
    try {
      testCode(f)
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
    }
  }

  private def peerSpec(address: InetSocketAddress): PeerSpec =
    defaultPeerSpec.copy(declaredAddress = Some(address))

  private def peerInfo(address: InetSocketAddress,
                       lastHandshake: Long = 0L,
                       connectionType: Option[ConnectionDirection] = None,
                       lastActivity: Long = 0L): PeerInfo =
    PeerInfo(
      defaultPeerSpec.copy(declaredAddress = Some(address)),
      lastHandshake,
      connectionType,
      lastActivity
    )

  private def address(i: Int): InetSocketAddress = new InetSocketAddress(s"8.8.${i / 256}.${i % 256}", 9000 + i)

  private def seenPeers(howMany: Int,
                        peers: Map[InetSocketAddress, PeerInfo],
                        blacklisted: Seq[InetAddress] = Seq.empty): Seq[PeerInfo] =
    SeenPeers(howMany).choose(peers, blacklisted, ScorexContext(Seq.empty, None, None))

  private def connectedPeer(address: InetSocketAddress): ConnectedPeer = {
    val localAddress = new InetSocketAddress("127.0.0.1", 9002)
    ConnectedPeer(
      ConnectionId(address, localAddress, Outgoing),
      ActorRef.noSender,
      None
    )
  }

  property("PeerManager should keep a connected peer during old-peer cleanup and not purge untried peers") {
    withFixture { f =>
      import f._
      val address = new InetSocketAddress("8.8.8.8", 9001)
      val spec = peerSpec(address)
      val probe = TestProbe()

      probe.send(peerManager, AddPeerIfEmpty(spec))
      probe.send(peerManager, GetAllPeers)
      val peers1 = probe.expectMsgType[Map[InetSocketAddress, PeerInfo]]
      peers1.keys should contain(address)

      probe.send(peerManager, HandshakedPeer(connectedPeer(address)))
      probe.send(peerManager, CleanupOldPeers)
      probe.send(peerManager, GetAllPeers)
      val peers2 = probe.expectMsgType[Map[InetSocketAddress, PeerInfo]]
      peers2.keys should contain(address)

      probe.send(peerManager, DisconnectedPeer(connectedPeer(address)))
      probe.send(peerManager, CleanupOldPeers)
      probe.send(peerManager, GetAllPeers)
      val peers3 = probe.expectMsgType[Map[InetSocketAddress, PeerInfo]]
      // the peer was never handshaked with (lastHandshake == 0), so it is exempt
      // from handshake-age cleanup
      peers3.keys should contain(address)
    }
  }

  property("PeerManager should protect an inbound peer whose advertised address differs from the socket address") {
    withFixture { f =>
      import f._
      // for an inbound connection the transport endpoint carries the peer's ephemeral
      // source port, while the peer database is keyed by the advertised listening address
      val socketAddress = new InetSocketAddress("8.8.8.8", 54321)
      val advertised = new InetSocketAddress("8.8.8.8", 9001)
      val oldTs = System.currentTimeMillis() - PeerDatabase.KnownPeerMaxAgeMs - 1000
      val probe = TestProbe()

      probe.send(peerManager, AddOrUpdatePeer(peerInfo(advertised, lastHandshake = oldTs)))

      val localAddress = new InetSocketAddress("127.0.0.1", 9002)
      val inbound = ConnectedPeer(
        ConnectionId(socketAddress, localAddress, Incoming),
        ActorRef.noSender,
        Some(peerInfo(advertised, lastHandshake = oldTs))
      )
      probe.send(peerManager, HandshakedPeer(inbound))
      probe.send(peerManager, CleanupOldPeers)
      probe.send(peerManager, GetAllPeers)
      val peers1 = probe.expectMsgType[Map[InetSocketAddress, PeerInfo]]
      peers1.keys should contain(advertised)

      probe.send(peerManager, DisconnectedPeer(inbound))
      probe.send(peerManager, CleanupOldPeers)
      probe.send(peerManager, GetAllPeers)
      val peers2 = probe.expectMsgType[Map[InetSocketAddress, PeerInfo]]
      peers2.keys should not contain advertised
    }
  }

  property("PeerManager should keep unavailable configured seed peers during cleanup") {
    val seed = new InetSocketAddress("8.8.8.8", 9001)
    val f = new PeerManagerFixture(Seq(seed))
    try {
      import f._
      val probe = TestProbe()

      // the seed is added on startup with lastHandshake == 0
      probe.send(peerManager, GetAllPeers)
      val peers1 = probe.expectMsgType[Map[InetSocketAddress, PeerInfo]]
      peers1.keys should contain(seed)

      probe.send(peerManager, CleanupOldPeers)
      probe.send(peerManager, GetAllPeers)
      val peers2 = probe.expectMsgType[Map[InetSocketAddress, PeerInfo]]
      peers2.keys should contain(seed)
    } finally {
      Await.result(f.system.terminate(), Duration.Inf)
    }
  }

  property("SeenPeers should return empty for non-positive or empty input") {
    seenPeers(0, Map.empty) shouldBe empty
    seenPeers(-1, Map.empty) shouldBe empty
    seenPeers(5, Map.empty) shouldBe empty
  }

  property("SeenPeers should return at most howMany peers") {
    val peers = (1 to 10).map(i => address(i) -> peerInfo(address(i), lastHandshake = 1L)).toMap
    val chosen = seenPeers(3, peers)
    chosen.size should be <= 3
    chosen.size should be > 0
  }

  property("SeenPeers should not return peers with neither handshake nor connection type") {
    val good = (1 to 5).map(i => address(i) -> peerInfo(address(i), lastHandshake = 1L)).toMap
    val bad = (6 to 10).map(i => address(i) -> peerInfo(address(i))).toMap
    val chosen = seenPeers(10, good ++ bad)
    chosen.map(_.peerSpec.declaredAddress.get).toSet.intersect(bad.keys.toSet) shouldBe empty
    chosen.size shouldBe 5
  }

  property("SeenPeers should exclude blacklisted peers") {
    val peers = (1 to 10).map { i =>
      val addr = address(i)
      addr -> peerInfo(addr, lastHandshake = 1L)
    }.toMap
    val blacklistedIp = InetAddress.getByName("8.8.8.1")
    val chosen = (1 to 100).flatMap(_ => seenPeers(10, peers, Seq(blacklistedIp))).toSet
    chosen.map(_.peerSpec.declaredAddress.get.getAddress).toSet should not contain blacklistedIp
  }

  property("SeenPeers should prefer recently active peers") {
    val now = System.currentTimeMillis()
    val recent = (1 to 5).map(i => address(i) -> peerInfo(address(i), lastHandshake = 1L, lastActivity = now)).toMap
    val old = (6 to 10).map(i => address(i) -> peerInfo(address(i), lastHandshake = 1L, lastActivity = 0L)).toMap
    val chosen = seenPeers(10, recent ++ old)
    chosen.size shouldBe 5
    chosen.map(_.lastStoredActivityTime).toSet should contain only now
  }

  property("SeenPeers should be able to reach any peer in a small DB over multiple calls") {
    val peers = (1 to 50).map(i => address(i) -> peerInfo(address(i), lastHandshake = 1L)).toMap
    val returned = (1 to 200).flatMap(_ => seenPeers(5, peers)).map(_.peerSpec.declaredAddress.get).toSet
    returned.size should be >= 45
  }

  property("SeenPeers should handle a large DB without materializing the full map") {
    val peers = (1 to 5000).map(i => address(i) -> peerInfo(address(i), lastHandshake = 1L)).toMap
    val chosen = seenPeers(8, peers)
    chosen.size shouldBe 8
    chosen.toSet.size shouldBe 8
  }

  property("SeenPeers should return all peers when howMany exceeds eligible count") {
    val peers = (1 to 5).map(i => address(i) -> peerInfo(address(i), lastHandshake = 1L)).toMap
    seenPeers(10, peers).size shouldBe 5
  }

  property("SeenPeers may skip eligible peers outside the bounded scan window") {
    val eligibleAddr = address(1)
    val eligible = Map(eligibleAddr -> peerInfo(eligibleAddr, lastHandshake = 1L))
    // all other peers are ineligible (never handshaked, no connection record)
    val ineligible = (2 to 2000).map(i => address(i) -> peerInfo(address(i))).toMap
    val peers = eligible ++ ineligible
    // With a single eligible peer among 2000, most random scan windows contain only
    // ineligible records and yield an empty result; windows that do cover the eligible
    // peer return only it. This is the accepted bounded-work tradeoff: the scan never
    // examines more than a bounded window even though eligible peers may be missed.
    val chosen = (1 to 20).flatMap(_ => seenPeers(5, peers))
    chosen.size should be <= 20
    chosen.forall(_.peerSpec.declaredAddress.contains(eligibleAddr)) shouldBe true
  }

}
