package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{AuthorizationFailedRejection, Route}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.MiningApiRoute
import org.ergoplatform.http.api.requests.MiningRequest
import org.ergoplatform.mining.AutolykosSolution
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.Stubs
import org.ergoplatform.utils.generators.ErgoCoreGenerators.genECPoint
import org.ergoplatform.{ErgoTreePredef, Pay2SAddress}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class MiningApiRouteSpec
  extends AnyFlatSpec
    with Matchers
    with ScalatestRouteTest
    with Stubs
    with FailFastCirceSupport {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  val prefix = "/mining"

  val localSetting: ErgoSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(useExternalMiner = true))
  val route: Route = MiningApiRoute(minerRef, localSetting).route

  val settingsWithAuth: ErgoSettings = localSetting.copy(
    scorexSettings = localSetting.scorexSettings.copy(
      restApi = localSetting.scorexSettings.restApi.copy(
        apiKeyHash = Some("e1c7ef7b3b742c5ae8f52c24d2c5f5c5dccd9c23a41fa6e76e5a6b62c8f72a10")
      )
    )
  )
  val routeWithAuth: Route = MiningApiRoute(minerRef, settingsWithAuth).route

  val solution = AutolykosSolution(genECPoint.sample.get, genECPoint.sample.get, Array.fill(32)(9: Byte), BigInt(0))

  // Valid compressed public key hex (33 bytes = 66 hex chars) - using a valid secp256k1 point
  val validPkHex = "020000000000000000000000000000000000000000000000000000000000000001"

  it should "return requested candidate" in {
    Get(prefix + "/candidate") ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[Json]) shouldBe 'success
    }
  }

  it should "process external solution" in {
    Post(prefix + "/solution", solution.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "display miner pk" in {
    Get(prefix + "/rewardAddress") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val script = ErgoTreePredef.rewardOutputScript(settings.chainSettings.monetary.minerRewardDelay, pk)
      val addressStr = Pay2SAddress(script)(settings.addressEncoder).toString()
      responseAs[Json].hcursor.downField("rewardAddress").as[String] shouldEqual Right(addressStr)
    }
  }

  it should "return candidate with valid custom miner public key" in {
    val request = MiningRequest(Seq.empty, validPkHex)

    Post(prefix + "/candidateWithTxsAndPk", request.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[Json]) shouldBe 'success
    }
  }

  it should "return candidate without api_key when auth is enabled" in {
    Get(prefix + "/candidate") ~> routeWithAuth ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "return reward address without api_key when auth is enabled" in {
    Get(prefix + "/rewardAddress") ~> routeWithAuth ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "return reward public key without api_key when auth is enabled" in {
    Get(prefix + "/rewardPublicKey") ~> routeWithAuth ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "accept solution without api_key when auth is enabled" in {
    Post(prefix + "/solution", solution.asJson) ~> routeWithAuth ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "reject candidateWithTxs without api_key when auth is enabled" in {
    Post(prefix + "/candidateWithTxs", Json.arr()) ~> routeWithAuth ~> check {
      rejection shouldBe AuthorizationFailedRejection
    }
  }

  it should "encode and decode MiningRequest correctly" in {
    val request = MiningRequest(Seq.empty, validPkHex)

    val json = request.asJson
    val decodedTxs = json.hcursor.downField("txs").as[Seq[Json]]
    val decodedPk = json.hcursor.downField("pk").as[String]

    decodedTxs shouldBe 'right
    decodedPk shouldBe Right(validPkHex)
  }

}
