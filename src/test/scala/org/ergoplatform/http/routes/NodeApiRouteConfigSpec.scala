package org.ergoplatform.http.routes

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.http.api.NodeApiRoute
import org.ergoplatform.settings.{ErgoSettings, PersistError, SettingsHolder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NodeApiRouteConfigSpec
  extends AnyFlatSpec
    with Matchers
    with ScalatestRouteTest
    with FailFastCirceSupport {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val prefix = "/node"

  private def makeRoute(initial: ErgoSettings, holder: SettingsHolder): Route =
    NodeApiRoute(initial, holder).route

  private def jsonEntity(s: String): HttpEntity.Strict =
    HttpEntity(ContentTypes.`application/json`, s)

  it should "GET /node/config returns the mutable subset" in {
    val holder = SettingsHolder.readonly(settings)
    val route = makeRoute(settings, holder)
    Get(prefix + "/config") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val j = responseAs[Json]
      j.hcursor.downField("mempool").downField("capacity").as[Int]
        .toOption shouldBe Some(settings.nodeSettings.mempoolCapacity)
      j.hcursor.downField("voting").focus shouldBe defined
    }
  }

  it should "PUT /node/config with valid mempool patch updates the holder" in {
    val holder = new SettingsHolder(settings, _ => Right(()), (_, _) => ())
    val route = makeRoute(settings, holder)
    val body = """{ "mempool": { "capacity": 7777, "minimalFeeAmount": 12345 } }"""
    Put(prefix + "/config", jsonEntity(body)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val j = responseAs[Json]
      j.hcursor.downField("mempool").downField("capacity").as[Int]
        .toOption shouldBe Some(7777)
      holder.current.nodeSettings.mempoolCapacity shouldBe 7777
      holder.current.nodeSettings.minimalFeeAmount shouldBe 12345L
    }
  }

  it should "PUT with invalid capacity (negative) returns 400 and leaves holder unchanged" in {
    val holder = new SettingsHolder(settings, _ => Right(()), (_, _) => ())
    val before = holder.current.nodeSettings.mempoolCapacity
    val route = makeRoute(settings, holder)
    val body = """{ "mempool": { "capacity": -1 } }"""
    Put(prefix + "/config", jsonEntity(body)) ~> Route.seal(route) ~> check {
      status shouldBe StatusCodes.BadRequest
      holder.current.nodeSettings.mempoolCapacity shouldBe before
    }
  }

  it should "PUT is all-or-nothing: invalid field rejects the whole patch" in {
    val holder = new SettingsHolder(settings, _ => Right(()), (_, _) => ())
    val before = holder.current.nodeSettings.mempoolCapacity
    val route = makeRoute(settings, holder)
    val body = """{ "mempool": { "capacity": 9999, "minimalFeeAmount": -7 } }"""
    Put(prefix + "/config", jsonEntity(body)) ~> Route.seal(route) ~> check {
      status shouldBe StatusCodes.BadRequest
      holder.current.nodeSettings.mempoolCapacity shouldBe before
      holder.current.nodeSettings.minimalFeeAmount should not be (-7L)
    }
  }

  it should "PUT with readonly holder (no user config) returns 409" in {
    val holder = SettingsHolder.readonly(settings)
    val route = makeRoute(settings, holder)
    val body = """{ "mempool": { "capacity": 1234 } }"""
    Put(prefix + "/config", jsonEntity(body)) ~> route ~> check {
      status shouldBe StatusCodes.Conflict
    }
  }

  it should "PUT with ConfigFileUnsupported persister returns 409" in {
    val holder = new SettingsHolder(
      settings,
      _ => Left(PersistError.ConfigFileUnsupported("config contains include directive")),
      (_, _) => ()
    )
    val route = makeRoute(settings, holder)
    val body = """{ "mempool": { "capacity": 1234 } }"""
    Put(prefix + "/config", jsonEntity(body)) ~> route ~> check {
      status shouldBe StatusCodes.Conflict
      val response = responseAs[Json]
      response.hcursor.downField("detail").as[String].toOption.getOrElse("") should include("include")
    }
  }

  it should "PUT with IoFailure persister returns 500" in {
    val holder = new SettingsHolder(
      settings,
      _ => Left(PersistError.IoFailure("disk full")),
      (_, _) => ()
    )
    val route = makeRoute(settings, holder)
    val body = """{ "mempool": { "capacity": 1234 } }"""
    Put(prefix + "/config", jsonEntity(body)) ~> route ~> check {
      status shouldBe StatusCodes.InternalServerError
    }
  }

  it should "PUT with voting patch updates voting targets and rulesToDisable" in {
    val holder = new SettingsHolder(settings, _ => Right(()), (_, _) => ())
    val route = makeRoute(settings, holder)
    val body = """{ "voting": { "targets": { "1": 1500000, "120": 1 }, "rulesToDisable": [215, 409] } }"""
    Put(prefix + "/config", jsonEntity(body)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val targets = holder.current.votingTargets.targets
      targets((1: Byte)) shouldBe 1500000
      targets((120: Byte)) shouldBe 1
      holder.current.votingTargets.desiredUpdate.rulesToDisable should contain allOf (215.toShort, 409.toShort)
    }
  }

  it should "PUT with malformed JSON body returns 400" in {
    val holder = new SettingsHolder(settings, _ => Right(()), (_, _) => ())
    val route = makeRoute(settings, holder)
    val body = "{ this is not json"
    Put(prefix + "/config", jsonEntity(body)) ~> Route.seal(route) ~> check {
      status.intValue() should (be(StatusCodes.BadRequest.intValue) or be(StatusCodes.UnprocessableEntity.intValue))
    }
  }

  it should "PUT with empty body is a no-op success" in {
    val holder = new SettingsHolder(settings, _ => Right(()), (_, _) => ())
    val before = holder.current
    val route = makeRoute(settings, holder)
    val body = "{}"
    Put(prefix + "/config", jsonEntity(body)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      holder.current shouldBe before
    }
  }
}
