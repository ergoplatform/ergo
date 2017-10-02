package org.ergoplatform.api.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.circe.syntax._
import io.circe.parser._
import org.ergoplatform.api.services.StateService
import org.ergoplatform.settings.ErgoSettings
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.VersionTag
import scorex.core.api.http.SuccessApiResponse
import scorex.crypto.encode.Base58

import scala.concurrent.Future

class StateApiRouteSpec extends FlatSpec with Matchers with ScalatestRouteTest {

  val service1 = StateService1Mock
  val service2 = StateService2Mock
  val service3 = StateService3Mock

  val route1 = StateApiRoute(service1, ErgoSettings.read(None).scorexSettings).route
  val route2 = StateApiRoute(service2, ErgoSettings.read(None).scorexSettings).route
  val route3 = StateApiRoute(service3, ErgoSettings.read(None).scorexSettings).route

  val base = "/state/"

  val versionRequests = Seq(Get(base + "version"), Post(base + "version"))
  val typeRequests = Seq(Get(base + "type"), Post(base + "type"))

  versionRequests.foreach { r =>
    r ~> route1 ~> check {
      status shouldEqual StatusCodes.OK
      val resp = responseAs[String]
      val json = parse(resp).right.get
      SuccessApiResponse(Map("version" -> Base58.encode(Array.fill(32)(1.toByte))).asJson).toJson shouldEqual json
    }
  }

  typeRequests.foreach { r =>
    r ~> route1 ~> check {
      status shouldEqual StatusCodes.OK
      val resp = responseAs[String]
      val json = parse(resp).right.get
      SuccessApiResponse(Map("type" -> "utxo").asJson).toJson shouldEqual json
    }
  }

  versionRequests.foreach { r =>
    r ~> route2 ~> check {
      status shouldEqual StatusCodes.OK
      val resp = responseAs[String]
      val json = parse(resp).right.get
      SuccessApiResponse(Map("version" -> Base58.encode(Array.fill(32)(2.toByte))).asJson).toJson shouldEqual json
    }
  }

  typeRequests.foreach { r =>
    r ~> route2 ~> check {
      status shouldEqual StatusCodes.OK
      val resp = responseAs[String]
      val json = parse(resp).right.get
      SuccessApiResponse(Map("type" -> "digest").asJson).toJson shouldEqual json
    }
  }

  versionRequests.foreach { r =>
    r ~> route3 ~> check {
      status shouldEqual StatusCodes.OK
      val resp = responseAs[String]
      val json = parse(resp).right.get
      SuccessApiResponse(Map("version" -> Base58.encode(Array.fill(32)(3.toByte))).asJson).toJson shouldEqual json
    }
  }

  typeRequests.foreach { r =>
    r ~> route3 ~> check {
      status shouldEqual StatusCodes.OK
      val resp = responseAs[String]
      val json = parse(resp).right.get
      SuccessApiResponse(Map("type" -> "unknown").asJson).toJson shouldEqual json
    }
  }

}

object StateService1Mock extends StateService {
  override def getVersion = Future.successful(VersionTag @@ Array.fill(32)(1.toByte))

  override def getType = Future.successful("utxo")
}

object StateService2Mock extends StateService {
  override def getVersion = Future.successful(VersionTag @@ Array.fill(32)(2.toByte))

  override def getType = Future.successful("digest")
}

object StateService3Mock extends StateService {
  override def getVersion = Future.successful(VersionTag @@ Array.fill(32)(3.toByte))

  override def getType = Future.successful("unknown")
}
