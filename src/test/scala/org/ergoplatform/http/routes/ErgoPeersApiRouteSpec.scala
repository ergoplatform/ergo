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
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.GetAllPeers
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.settings.RESTApiSettings

import java.net.InetSocketAddress
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

  it should "return at most 50 peers by default" in {
    val networkControllerProbe = TestProbe()
    val route: Route = ErgoPeersApiRoute(peerManagerProbe.ref, networkControllerProbe.ref, null, null, restApiSettings).route
    val peers = (1 to 55).map { i =>
      val addr = new InetSocketAddress(s"8.8.0.$i", 9000 + i)
      addr -> PeerInfo.fromAddress(addr)
    }.toMap
    Future {
      peerManagerProbe.expectMsg(GetAllPeers)
      peerManagerProbe.reply(peers)
    }

    Get("/peers/all") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].asArray.get.size shouldBe 50
    }
  }

  it should "respect limit and offset query parameters" in {
    val networkControllerProbe = TestProbe()
    val route: Route = ErgoPeersApiRoute(peerManagerProbe.ref, networkControllerProbe.ref, null, null, restApiSettings).route
    val peers = (1 to 20).map { i =>
      val addr = new InetSocketAddress(s"8.8.0.$i", 9000 + i)
      addr -> PeerInfo.fromAddress(addr)
    }.toMap
    val sortedAddresses = peers.keys.toSeq.sortBy(_.toString)
    Future {
      peerManagerProbe.expectMsg(GetAllPeers)
      peerManagerProbe.reply(peers)
    }

    Get("/peers/all?limit=5&offset=10") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val arr = responseAs[Json].asArray.get
      arr.size shouldBe 5
      arr.head.hcursor.downField("address").as[String] shouldEqual Right(sortedAddresses(10).toString)
    }
  }

  it should "return empty array when offset is beyond peer count" in {
    val networkControllerProbe = TestProbe()
    val route: Route = ErgoPeersApiRoute(peerManagerProbe.ref, networkControllerProbe.ref, null, null, restApiSettings).route
    val peers = (1 to 5).map { i =>
      val addr = new InetSocketAddress(s"8.8.0.$i", 9000 + i)
      addr -> PeerInfo.fromAddress(addr)
    }.toMap
    Future {
      peerManagerProbe.expectMsg(GetAllPeers)
      peerManagerProbe.reply(peers)
    }

    Get("/peers/all?offset=100") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].asArray.get shouldBe empty
    }
  }

  it should "reject invalid pagination parameters" in {
    val networkControllerProbe = TestProbe()
    val route: Route = ErgoPeersApiRoute(peerManagerProbe.ref, networkControllerProbe.ref, null, null, restApiSettings).route
    Future {
      peerManagerProbe.expectMsg(GetAllPeers)
      peerManagerProbe.reply(Map.empty[InetSocketAddress, PeerInfo])
    }

    Get("/peers/all?limit=-1") ~> Route.seal(route) ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }
}

