package org.ergoplatform.network.peer

import akka.actor.ActorSystem
import akka.testkit.TestActorRef
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages._
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.FileUtils
import scorex.core.app.ScorexContext
import scorex.testkit.utils.AkkaFixture

import java.net.InetSocketAddress
import scala.concurrent.Await
import scala.concurrent.duration._

class PeerManagerFailedDialSpec extends ErgoCorePropertyTest with FileUtils {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val hour = 1.hour.toMillis

  private def peer(address: InetSocketAddress, lastHandshake: Long): PeerInfo =
    PeerInfo(defaultPeerSpec.copy(declaredAddress = Some(address)), lastHandshake)

  // a peer manager over an empty peers database, with `configured` as the config file's known peers
  private def withPeerManager(configured: Seq[InetSocketAddress] = Seq.empty)
                             (test: (AkkaFixture, TestActorRef[PeerManager]) => Any): Unit = {
    val fixture = new AkkaFixture
    try {
      val testSettings = settings.copy(
        directory = createTempDir.getAbsolutePath,
        scorexSettings = settings.scorexSettings.copy(
          network = settings.scorexSettings.network.copy(knownPeers = configured)
        )
      )
      implicit val system: ActorSystem = fixture.system
      val peerManager = TestActorRef(new PeerManager(testSettings, ScorexContext(Seq.empty, None, None)))
      test(fixture, peerManager)
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  private def knownAfterFailedDial(f: AkkaFixture, peerManager: TestActorRef[PeerManager],
                                   address: InetSocketAddress): Boolean = {
    peerManager.tell(DialFailed(address), f.testActor)
    peerManager.tell(GetAllPeers, f.testActor)
    f.expectMsgType[Map[InetSocketAddress, PeerInfo]].contains(address)
  }

  property("a failed dial removes a peer this node never completed a handshake with") {
    withPeerManager() { (f, peerManager) =>
      val learned = new InetSocketAddress("203.0.113.60", 9060)
      peerManager ! AddPeerIfEmpty(defaultPeerSpec.copy(declaredAddress = Some(learned)))
      knownAfterFailedDial(f, peerManager, learned) shouldBe false
    }
  }

  property("a failed dial keeps a peer whose last handshake is less than 3 days old") {
    withPeerManager() { (f, peerManager) =>
      val restarting = new InetSocketAddress("203.0.113.61", 9061)
      peerManager ! AddOrUpdatePeer(peer(restarting, System.currentTimeMillis() - 3.days.toMillis + hour))
      knownAfterFailedDial(f, peerManager, restarting) shouldBe true
    }
  }

  property("a failed dial removes a peer whose last handshake is more than 3 days old") {
    withPeerManager() { (f, peerManager) =>
      val gone = new InetSocketAddress("203.0.113.62", 9062)
      peerManager ! AddOrUpdatePeer(peer(gone, System.currentTimeMillis() - 3.days.toMillis - hour))
      knownAfterFailedDial(f, peerManager, gone) shouldBe false
    }
  }

  property("a failed dial keeps a peer from the config file that never completed a handshake") {
    // config file peers are seeded only into an empty database, so a removed one would not come back on restart
    val configured = new InetSocketAddress("203.0.113.63", 9063)
    withPeerManager(configured = Seq(configured)) { (f, peerManager) =>
      knownAfterFailedDial(f, peerManager, configured) shouldBe true
    }
  }

  property("RemovePeer still removes a peer whose last handshake is recent") {
    withPeerManager() { (f, peerManager) =>
      val dropped = new InetSocketAddress("203.0.113.64", 9064)
      peerManager ! AddOrUpdatePeer(peer(dropped, System.currentTimeMillis()))
      peerManager ! RemovePeer(dropped)
      peerManager.tell(GetAllPeers, f.testActor)
      f.expectMsgType[Map[InetSocketAddress, PeerInfo]].contains(dropped) shouldBe false
    }
  }

}
