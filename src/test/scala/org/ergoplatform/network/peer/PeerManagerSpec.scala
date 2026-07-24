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
import scorex.core.network.{ConnectionId, ConnectedPeer, Outgoing}
import scorex.testkit.utils.AkkaFixture

import java.io.File
import java.net.InetSocketAddress
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class PeerManagerSpec extends ErgoCorePropertyTest with DBSpec {

  import PeerManager.ReceivableMessages._

  private class PeerManagerFixture extends AkkaFixture {
    val dir: File = createTempDir

    val settings: ErgoSettings = {
      val base = initSettings.copy(directory = dir.getAbsolutePath)
      base.copy(
        scorexSettings = base.scorexSettings.copy(
          network = base.scorexSettings.network.copy(
            knownPeers = Seq.empty
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

  private def connectedPeer(address: InetSocketAddress): ConnectedPeer = {
    val localAddress = new InetSocketAddress("127.0.0.1", 9002)
    ConnectedPeer(
      ConnectionId(address, localAddress, Outgoing),
      ActorRef.noSender,
      None
    )
  }

  property("PeerManager should keep a connected peer during old-peer cleanup") {
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
      peers3.keys should not contain address
    }
  }

}
