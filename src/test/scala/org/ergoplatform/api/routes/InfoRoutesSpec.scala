package org.ergoplatform.api.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import io.circe.syntax._
import org.ergoplatform.Version
import org.ergoplatform.settings.Algos
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base58

import scala.concurrent.duration._

class InfoRoutesSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  val restApiSettings = RESTApiSettings("localhost", 8080, None, false, 10 seconds)

  val route = InfoRoute(readersRef, minerRef, pmRef, digest = true, restApiSettings, nodeId).route

  implicit val timeout = RouteTestTimeout(15.seconds dilated)

  val stateVersion = readers.s.map(_.version).map(Algos.encode)
  val bestHeader = readers.h.flatMap(_.bestHeaderOpt)
  val bestFullBlock = readers.h.flatMap(_.bestFullBlockOpt)
  val poolSize = readers.m.map(_.size).getOrElse(-1)
  val stateRoot = readers.s.map(s => Algos.encode(s.rootHash)).getOrElse("Undefined")

  val resp = Map(
    "name" -> Algos.encode(nodeId).asJson,
    "stateVersion" -> Version.VersionString.asJson,
    "headersHeight" -> bestHeader.map(_.height).getOrElse(-1).asJson,
    "fullHeight" -> bestFullBlock.map(_.header.height).getOrElse(-1).asJson,
    "bestHeaderId" -> bestHeader.map(_.encodedId).getOrElse("None").asJson,
    "bestFullHeaderId" -> bestFullBlock.map(_.header.encodedId).getOrElse("None").asJson,
    "previousFullHeaderId" -> bestFullBlock.map(_.header.parentId).map(Base58.encode).getOrElse("None").asJson,
    "stateRoot" -> stateRoot.asJson,
    "difficulty" -> bestFullBlock.map(_.header.requiredDifficulty.toString).getOrElse("None").asJson,
    "unconfirmedCount" -> poolSize.asJson,
    "stateType" -> "digest".asJson,
    "stateVersion" -> stateVersion.asJson,
    "isMining" -> false.asJson,
    "votes" -> Algos.encode(Array.empty).asJson,
    "peersCount" -> 0.asJson
  ).asJson

  it should "return info" in {
    Get("/info") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val rawData = responseAs[String]
      resp.toString() shouldEqual rawData
    }
  }


}
