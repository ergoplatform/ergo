package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.MiningApiRoute
import org.ergoplatform.mining.AutolykosSolution
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.Stubs
import org.ergoplatform.utils.generators.ErgoCoreGenerators.genECPoint
import org.ergoplatform.{ErgoTreePredef, Pay2SAddress}
import org.bouncycastle.util.BigIntegers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.encode.Base16
import sigmastate.crypto.DLogProtocol.DLogProverInput

import scala.util.Try

class MiningApiRouteSpec
  extends AnyFlatSpec
    with Matchers
    with ScalatestRouteTest
    with Stubs
    with FailFastCirceSupport {

  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  val prefix = "/mining"

  val localSetting: ErgoSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(useExternalMiner = true))
  val route: Route = MiningApiRoute(minerRef, localSetting).route

  val solution = AutolykosSolution(genECPoint.sample.get, genECPoint.sample.get, Array.fill(32)(9: Byte), BigInt(0))

  it should "return requested candidate" in {
    Get(prefix + "/candidate") ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[Json]) shouldBe 'success
    }
  }

  it should "return requested candidate with a custom miner public key" in {
    val customPk = DLogProverInput(
      BigIntegers.fromUnsignedByteArray("custom route miner".getBytes())
    ).publicImage
    val customPkHex = Base16.encode(customPk.pkBytes)
    val request = Json.obj(
      "txs" -> Seq.empty[ErgoTransaction].asJson,
      "pk" -> customPkHex.asJson
    )

    Post(prefix + "/candidateWithTxsAndPk", request) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("pk").as[String] shouldEqual Right(customPkHex)
    }
  }

  it should "reject candidate requests with an invalid custom miner public key" in {
    val request = Json.obj(
      "txs" -> Seq.empty[ErgoTransaction].asJson,
      "pk" -> "not-a-public-key".asJson
    )

    Post(prefix + "/candidateWithTxsAndPk", request) ~> Route.seal(route) ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "reject candidate requests with malformed custom miner public key bytes" in {
    val request = Json.obj(
      "txs" -> Seq.empty[ErgoTransaction].asJson,
      "pk" -> "00".asJson
    )

    Post(prefix + "/candidateWithTxsAndPk", request) ~> Route.seal(route) ~> check {
      status shouldBe StatusCodes.BadRequest
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

}
