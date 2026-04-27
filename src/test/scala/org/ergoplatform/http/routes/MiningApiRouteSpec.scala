package org.ergoplatform.http.routes

import akka.actor.{Actor, ActorRef, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.StatusReply
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.MiningApiRoute
import org.ergoplatform.http.api.requests.MiningRequest
import org.ergoplatform.mining.CandidateGenerator.Candidate
import org.ergoplatform.mining.{CandidateGenerator, ErgoMiner, WeakAutolykosSolution}
import org.ergoplatform.settings.{ErgoSettings, ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.utils.Stubs
import org.ergoplatform.{AutolykosSolution, ErgoTreePredef, InputSolutionFound, OrderingSolutionFound, Pay2SAddress}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.ergoplatform.mining.AutolykosSolutionJsonCodecs._
import org.ergoplatform.mining.{genPk, q}
import org.ergoplatform.utils.generators.CoreObjectGenerators.genBytes
import org.scalacheck.Gen
import sigma.crypto.EcPointType

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Try

class MiningApiRouteSpec
  extends AnyFlatSpec
    with Matchers
    with ScalatestRouteTest
    with Stubs
    with FailFastCirceSupport {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  lazy val genECPoint: Gen[EcPointType] = genBytes(32).map(b => genPk(BigInt(b).mod(q)))

  val prefix = "/mining"

  val localSetting: ErgoSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(useExternalMiner = true))
  val route: Route = MiningApiRoute(minerRef, localSetting).route

  val solution = new AutolykosSolution(genECPoint.sample.get, genECPoint.sample.get, Array.fill(32)(9: Byte), BigInt(0))
  val weakSolution = WeakAutolykosSolution(genECPoint.sample.get, Array.fill(32)(9: Byte))

  // Valid compressed public key hex (33 bytes = 66 hex chars) - using a valid secp256k1 point
  val validPkHex = "020000000000000000000000000000000000000000000000000000000000000001"

  case object GetReceivedMessages

  class TrackingMinerStub extends Actor {
    val received: mutable.Buffer[Any] = mutable.Buffer.empty

    def receive: Receive = {
      case CandidateGenerator.GenerateCandidate(_, reply, _, _) =>
        if (reply) {
          val defaultParams = Parameters(0, Parameters.DefaultParameters, ErgoValidationSettingsUpdate.empty)
          val candidate = Candidate(null, externalWorkMessage, Seq.empty, defaultParams)
          sender() ! StatusReply.success(candidate)
        }
      case msg @ (_: OrderingSolutionFound | _: InputSolutionFound) =>
        received += msg
        sender() ! StatusReply.success(())
      case GetReceivedMessages =>
        sender() ! StatusReply.success(received.toSeq)
      case ErgoMiner.ReadMinerPk =>
        sender() ! StatusReply.success(pk)
    }
  }

  def trackingRoute: (Route, ActorRef) = {
    val miner = system.actorOf(Props(new TrackingMinerStub))
    (MiningApiRoute(miner, localSetting).route, miner)
  }

  it should "return requested candidate" in {
    Get(prefix + "/candidate") ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[Json]) shouldBe 'success
    }
  }

  it should "process external solution and send OrderingSolutionFound to miner" in {
    val (tr, miner) = trackingRoute
    Post(prefix + "/solution", solution.asJson) ~> tr ~> check {
      status shouldBe StatusCodes.OK
    }

    import akka.pattern.ask
    implicit val timeout: akka.util.Timeout = akka.util.Timeout(3.seconds)
    val receivedF = miner.ask(GetReceivedMessages).mapTo[StatusReply[Seq[Any]]]
    val received = scala.concurrent.Await.result(receivedF, 3.seconds).getValue

    received should have length 1
    received.head shouldBe a[OrderingSolutionFound]
    val osf = received.head.asInstanceOf[OrderingSolutionFound]
    osf.as.pk shouldBe solution.pk
    osf.as.w shouldBe solution.w
    osf.as.n shouldBe solution.n
    osf.as.d shouldBe solution.d
  }

  it should "process external weak solution and send InputSolutionFound to miner with v2 defaults" in {
    val (tr, miner) = trackingRoute
    Post(prefix + "/weakSolution", weakSolution.asJson) ~> tr ~> check {
      status shouldBe StatusCodes.OK
    }

    import akka.pattern.ask
    implicit val timeout: akka.util.Timeout = akka.util.Timeout(3.seconds)
    val receivedF = miner.ask(GetReceivedMessages).mapTo[StatusReply[Seq[Any]]]
    val received = scala.concurrent.Await.result(receivedF, 3.seconds).getValue

    received should have length 1
    received.head shouldBe a[InputSolutionFound]
    val isf = received.head.asInstanceOf[InputSolutionFound]
    isf.as.pk shouldBe weakSolution.pk
    isf.as.w shouldBe AutolykosSolution.wForV2
    isf.as.d shouldBe AutolykosSolution.dForV2
    isf.as.n shouldBe weakSolution.n
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

  it should "encode and decode MiningRequest correctly" in {
    val request = MiningRequest(Seq.empty, validPkHex)

    val json = request.asJson
    val decodedTxs = json.hcursor.downField("txs").as[Seq[Json]]
    val decodedPk = json.hcursor.downField("pk").as[String]

    decodedTxs shouldBe 'right
    decodedPk shouldBe Right(validPkHex)
  }

}
