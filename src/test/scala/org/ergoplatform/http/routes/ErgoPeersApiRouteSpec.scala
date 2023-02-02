package org.ergoplatform.http.routes

import akka.actor.Actor
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.{TestDuration, TestProbe}
import akka.util.Timeout
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.http.api.ErgoPeersApiRoute
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.network.NetworkController.ReceivableMessages.GetConnectedPeers
import scorex.core.network.peer.PeerManager.ReceivableMessages.GetAllPeers
import scorex.core.settings.RESTApiSettings

import java.net.InetSocketAddress
import scala.concurrent.Future
import scala.concurrent.duration._

class ErgoPeersApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with ScalaCheckPropertyChecks
  with Stubs {

  implicit val actorTimeout: Timeout = Timeout(15.seconds.dilated)
  implicit val routeTimeout: RouteTestTimeout = RouteTestTimeout(15.seconds.dilated)

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds, None)
  val peerManagerProbe = TestProbe()

  it should "return all peers" in {
    forAll(connectedPeerGen(Actor.noSender)) { peer =>
      val networkControllerProbe = TestProbe()
      val route: Route = ErgoPeersApiRoute(peerManagerProbe.ref, networkControllerProbe.ref, null, null, restApiSettings).route
      Future {
        peerManagerProbe.expectMsg(GetAllPeers)
        peerManagerProbe.reply(Map(peer.connectionId.remoteAddress -> peer.peerInfo.get))
      }

      Get("/peers/all") ~> route ~> check {
        status shouldBe StatusCodes.OK
        val json = responseAs[Json]
        log.info(s"Received connected peers: $json")
        val c = json.asArray.get.head.hcursor
        c.downField("address").as[String] shouldEqual Right(peer.connectionId.remoteAddress.toString)
        peer.peerInfo.get.peerSpec.publicUrlOpt.foreach { restApiUrl =>
          c.downField("restApiUrl").as[String] shouldEqual Right(restApiUrl.toString)
        }
        c.downField("lastMessage").as[Long] shouldEqual Right(0L)
        c.downField("lastHandshake").as[Long] shouldEqual Right(0L)
        c.downField("name").as[String] shouldEqual Right(peer.peerInfo.get.peerSpec.nodeName)
        c.downField("connectionType").as[String] shouldEqual Right("Incoming")
      }
    }
  }

  it should "return connected peers" in {
    forAll(connectedPeerGen(Actor.noSender)) { peer =>
      val networkControllerProbe = TestProbe()
      val route: Route = ErgoPeersApiRoute(peerManagerProbe.ref, networkControllerProbe.ref, null, null, restApiSettings).route
      Future {
        networkControllerProbe.expectMsg(GetConnectedPeers)
        networkControllerProbe.reply(Seq(peer))
      }

      Get("/peers/connected") ~> route ~> check {
        status shouldBe StatusCodes.OK
        val json = responseAs[Json]
        log.info(s"Received connected peers: $json")
        val c = json.asArray.get.head.hcursor
        peer.peerInfo.get.peerSpec.address.foreach { address =>
          c.downField("address").as[String] shouldEqual Right(address.toString)
        }
        peer.peerInfo.get.peerSpec.publicUrlOpt.foreach { restApiUrl =>
          c.downField("restApiUrl").as[String] shouldEqual Right(restApiUrl.toString)
        }
        c.downField("lastMessage").as[Long] shouldEqual Right(0L)
        c.downField("lastHandshake").as[Long] shouldEqual Right(0L)
        c.downField("name").as[String] shouldEqual Right(peer.peerInfo.get.peerSpec.nodeName)
        c.downField("connectionType").as[String] shouldEqual Right("Incoming")
      }
    }
  }
}

