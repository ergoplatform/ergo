package scorex.core.network

import akka.io.Tcp.{Bind, Close, Connected, PeerClosed}
import akka.testkit.TestProbe
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.{ConfirmConnection, ConnectionConfirmed, ConnectionDenied}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoNodeTestConstants
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.app.ScorexContext
import scorex.testkit.utils.AkkaFixture

import java.net.InetSocketAddress
import java.nio.file.Files
import scala.concurrent.Await
import scala.concurrent.duration._

class NetworkControllerSpecification extends AnyFlatSpec with Matchers {

  private class NetworkControllerFixture(maxConnections: Int) extends AkkaFixture {
    implicit val ec = system.dispatcher

    val peerManagerProbe: TestProbe = TestProbe()
    val tcpManagerProbe: TestProbe = TestProbe()
    val scorexContext: ScorexContext = ScorexContext(Seq.empty, None, None)

    val ergoSettings: ErgoSettings = {
      val base = ErgoNodeTestConstants.settings
      val networkSettings = base.scorexSettings.network.copy(
        maxConnections = maxConnections,
        bindAddress = new InetSocketAddress("127.0.0.1", 0),
        declaredAddress = None
      )
      base.copy(
        directory = Files.createTempDirectory("ergo-network-controller").toFile.getAbsolutePath,
        scorexSettings = base.scorexSettings.copy(network = networkSettings)
      )
    }

    val networkController = system.actorOf(
      NetworkControllerRef.props(
        ergoSettings,
        peerManagerProbe.ref,
        scorexContext,
        tcpManagerProbe.ref,
        _ => Map.empty[MessageCode, akka.actor.ActorRef]
      )
    )

    tcpManagerProbe.expectMsgType[Bind]

    def incomingLimit: Int =
      Math.max(maxConnections / 2, maxConnections - NetworkController.OutgoingConnections)

    def remote(i: Int): InetSocketAddress =
      new InetSocketAddress(s"203.0.113.$i", 9000 + i)

    val local: InetSocketAddress = new InetSocketAddress("127.0.0.1", 9000)
  }

  private def withNetworkController(maxConnections: Int)(testCode: NetworkControllerFixture => Any): Unit = {
    val fixture = new NetworkControllerFixture(maxConnections)
    try {
      testCode(fixture)
    } finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  it should "count pending inbound confirmations against the incoming connection limit" in {
    withNetworkController(maxConnections = 10) { fixture =>
      import fixture._

      (1 to incomingLimit).foreach { i =>
        val connection = TestProbe()
        connection.send(networkController, Connected(remote(i), local))
        peerManagerProbe.expectMsgType[ConfirmConnection]
      }

      val overflowConnection = TestProbe()
      overflowConnection.send(networkController, Connected(remote(99), local))
      overflowConnection.expectMsg(Close)
      peerManagerProbe.expectNoMessage(300.millis)
    }
  }

  it should "release a pending inbound slot when peer manager denies the connection" in {
    withNetworkController(maxConnections = 10) { fixture =>
      import fixture._

      (1 to incomingLimit).foreach { i =>
        val connection = TestProbe()
        connection.send(networkController, Connected(remote(i), local))
        val confirm = peerManagerProbe.expectMsgType[ConfirmConnection]
        if (i == 1) {
          peerManagerProbe.send(networkController, ConnectionDenied(confirm.connectionId, confirm.handlerRef))
          connection.expectMsg(Close)
        }
      }

      val replacementConnection = TestProbe()
      replacementConnection.send(networkController, Connected(remote(99), local))
      peerManagerProbe.expectMsgType[ConfirmConnection]
    }
  }

  it should "close duplicate inbound connections while confirmation is pending" in {
    withNetworkController(maxConnections = 10) { fixture =>
      import fixture._

      val firstConnection = TestProbe()
      firstConnection.send(networkController, Connected(remote(1), local))
      peerManagerProbe.expectMsgType[ConfirmConnection]

      val duplicateConnection = TestProbe()
      duplicateConnection.send(networkController, Connected(remote(1), local))
      duplicateConnection.expectMsg(Close)
      peerManagerProbe.expectNoMessage(300.millis)
    }
  }

  it should "ignore stale inbound confirmations after the raw connection has closed" in {
    withNetworkController(maxConnections = 10) { fixture =>
      import fixture._

      val firstConnection = TestProbe()
      firstConnection.send(networkController, Connected(remote(1), local))
      val firstConfirm = peerManagerProbe.expectMsgType[ConfirmConnection]
      firstConnection.send(networkController, PeerClosed)

      val secondConnection = TestProbe()
      secondConnection.send(networkController, Connected(remote(1), local))
      peerManagerProbe.expectMsgType[ConfirmConnection]

      peerManagerProbe.send(networkController, ConnectionConfirmed(firstConfirm.connectionId, firstConfirm.handlerRef))
      firstConnection.expectMsg(Close)

      val duplicateConnection = TestProbe()
      duplicateConnection.send(networkController, Connected(remote(1), local))
      duplicateConnection.expectMsg(Close)
      peerManagerProbe.expectNoMessage(300.millis)
    }
  }

  it should "ignore stale inbound denials after the raw connection has closed" in {
    withNetworkController(maxConnections = 10) { fixture =>
      import fixture._

      val firstConnection = TestProbe()
      firstConnection.send(networkController, Connected(remote(1), local))
      val firstConfirm = peerManagerProbe.expectMsgType[ConfirmConnection]
      firstConnection.send(networkController, PeerClosed)

      val secondConnection = TestProbe()
      secondConnection.send(networkController, Connected(remote(1), local))
      peerManagerProbe.expectMsgType[ConfirmConnection]

      peerManagerProbe.send(networkController, ConnectionDenied(firstConfirm.connectionId, firstConfirm.handlerRef))
      firstConnection.expectMsg(Close)

      val duplicateConnection = TestProbe()
      duplicateConnection.send(networkController, Connected(remote(1), local))
      duplicateConnection.expectMsg(Close)
      peerManagerProbe.expectNoMessage(300.millis)
    }
  }
}
