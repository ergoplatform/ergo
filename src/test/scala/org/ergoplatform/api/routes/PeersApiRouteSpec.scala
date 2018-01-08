package org.ergoplatform.api.routes

import java.net.InetSocketAddress

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import io.circe.syntax._
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.api.http.PeersApiRoute
import scorex.core.settings.RESTApiSettings

import scala.concurrent.duration._

//TODO move this tests to scorex
class PeersApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  implicit val timeout = RouteTestTimeout(15.seconds dilated)

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, false, 10 seconds)
  val prefix = "/peers"
  val routes = PeersApiRoute(pmRef, networkControllerRef, restApiSettings).route

  val peersResp = peers.map { case (address, peerInfo) =>
    Seq(
      Some("address" -> address.toString.asJson),
      Some("lastSeen" -> peerInfo.lastSeen.asJson),
      peerInfo.nodeName.map(name => "name" -> name.asJson),
      peerInfo.nonce.map(nonce => "nonce" -> nonce.asJson)).flatten.toMap
  }.asJson.toString

  val connectedPeersResp = connectedPeers.map { handshake =>
    Map(
      "address" -> handshake.declaredAddress.toString.asJson,
      "name" -> handshake.nodeName.asJson,
      "lastSeen" -> handshake.time.asJson
    ).asJson
  }.asJson

  it should "get all peers" in {
    Get(prefix + "/all") ~> routes ~> check {
      status shouldBe StatusCodes.OK
      peersResp shouldBe responseAs[String]
    }
  }

  //can't check it cause original node using now() timestamp for last seen field
  ignore should "get connected peers" in {
    Get(prefix + "/connected") ~> routes ~> check {
      status shouldBe StatusCodes.OK
      connectedPeersResp shouldBe responseAs[String]
    }
  }

  it should "connect to peer" in {
    Post(prefix + "/connect", "localhost:8080") ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "get blacklisted peers" in {
    Get(prefix + "/blacklisted") ~> routes ~> check {
      status shouldBe StatusCodes.OK
      println(responseAs[String])
      Map("address" -> blacklistedPeers).asJson.toString shouldBe responseAs[String]
    }
  }
}
