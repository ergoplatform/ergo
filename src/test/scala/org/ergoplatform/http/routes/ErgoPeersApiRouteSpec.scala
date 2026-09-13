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
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.network.NetworkController.ReceivableMessages.{ConnectTo, GetConnectedPeers}
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.GetAllPeers
import org.ergoplatform.settings.RESTApiSettings

import java.net.{InetAddress, InetSocketAddress}
import scala.concurrent.Future
import scala.concurrent.duration._

class ErgoPeersApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with ScalaCheckPropertyChecks
  with Stubs {

  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._

  implicit val actorTimeout: Timeout = Timeout(15.seconds.dilated)
  implicit val routeTimeout: RouteTestTimeout = RouteTestTimeout(15.seconds.dilated)

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds, None)
  val peerManagerProbe = TestProbe()

  private def assertConnectRejected(address: String): Unit = {
    val networkControllerProbe = TestProbe()
    val route = Route.seal(
      ErgoPeersApiRoute(peerManagerProbe.ref, networkControllerProbe.ref, null, null, restApiSettings).route)

    Post("/peers/connect", Json.fromString(address)) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }

    networkControllerProbe.expectNoMessage(200.millis)
  }

  it should "reject peer connections to port zero" in {
    assertConnectRejected("127.0.0.1:0")
  }

  it should "reject peer connections above the maximum port" in {
    assertConnectRejected("127.0.0.1:65536")
  }

  it should "reject peer connections with trailing input" in {
    assertConnectRejected("127.0.0.1:5673junk")
  }

  it should "reject peer connections to unresolved hosts" in {
    assertConnectRejected("doesnotresolve.invalid:5673")
  }

  it should "accept peer connections at both valid port boundaries" in {
    Seq(1, 65535).foreach { port =>
      val networkControllerProbe = TestProbe()
      val route = Route.seal(
        ErgoPeersApiRoute(peerManagerProbe.ref, networkControllerProbe.ref, null, null, restApiSettings).route)

      Post("/peers/connect", Json.fromString(s"127.0.0.1:$port")) ~> route ~> check {
        status shouldBe StatusCodes.OK
      }

      val address = new InetSocketAddress(InetAddress.getByName("127.0.0.1"), port)
      networkControllerProbe.expectMsg(ConnectTo(PeerInfo.fromAddress(address)))
    }
  }

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

