package org.ergoplatform.it

import akka.actor.{ActorRef, ActorSystem}
import akka.io.Tcp
import akka.testkit.{ExplicitlyTriggeredScheduler, TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.RandomPeerExcluding
import org.ergoplatform.settings.ScorexSettings
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.app.ScorexContext
import scorex.core.network.NetworkController

import java.net.InetSocketAddress
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

class DeepRollBackIsolationSpec extends AnyFlatSpec with Matchers {
  "Isolated rollback mining" should "disable automatic peer selection and reject incoming connections" in {
    val ordinaryConfig = ConfigFactory.load()
    val isolatedNetwork = ScorexSettings.fromConfig(
      DeepRollBackSpec.isolatedMiningConfig.withFallback(ordinaryConfig).resolve()).network
    ScorexSettings.fromConfig(ordinaryConfig).network.maxConnections should be > 0

    implicit val system: ActorSystem = ActorSystem("RollbackIsolation", ConfigFactory.parseString(
      "akka.scheduler.implementation = akka.testkit.ExplicitlyTriggeredScheduler"))
    implicit val ec: ExecutionContext = system.dispatcher
    try {
      def controller(maxConnections: Int): (TestActorRef[NetworkController], TestProbe) = {
        val peers = TestProbe()
        val tcp = TestProbe()
        val controllerSettings = settings.copy(scorexSettings = settings.scorexSettings.copy(
          network = settings.scorexSettings.network.copy(maxConnections = maxConnections)))
        val ref = TestActorRef(new NetworkController(controllerSettings, peers.ref,
          ScorexContext(Seq.empty, None, None), tcp.ref, _ => Map.empty[MessageCode, ActorRef]))
        tcp.expectMsgType[Tcp.Bind]
        ref ! Tcp.Bound(controllerSettings.scorexSettings.network.bindAddress)
        (ref, peers)
      }

      val (isolated, isolatedPeers) = controller(isolatedNetwork.maxConnections)
      val (_, ordinaryPeers) = controller(maxConnections = 1)
      system.scheduler.asInstanceOf[ExplicitlyTriggeredScheduler].timePasses(5.seconds)
      ordinaryPeers.expectMsgType[RandomPeerExcluding](3.seconds)
      isolatedPeers.expectNoMessage(200.millis)

      val incoming = TestProbe()
      incoming.send(isolated, Tcp.Connected(new InetSocketAddress("127.0.0.2", 9000),
        settings.scorexSettings.network.bindAddress))
      incoming.expectMsg(Tcp.Close)
      isolatedPeers.expectNoMessage(200.millis)
    } finally {
      Await.result(system.terminate(), 10.seconds)
    }
  }
}
