package org.ergoplatform.api.routes

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.pattern.ask
import akka.testkit.TestDuration
import akka.util.Timeout
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.Version
import org.ergoplatform.api.InfoRoute
import org.ergoplatform.local.ErgoStatsCollector.GetNodeInfo
import org.ergoplatform.local.NodeInfo
import org.ergoplatform.local.ErgoStatsCollectorRef
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.ChangedHistory
import scorex.core.utils.NetworkTime.Time
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class InfoRoutesSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {

  import jsonEncoders._

  val fakeTimeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.scorexSettings.ntp) {
    override def time(): Time = 123
  }

  implicit val actorTimeout = Timeout(15.seconds.dilated)
  implicit val routeTimeout = RouteTestTimeout(15.seconds.dilated)
  val statsCollector: ActorRef = ErgoStatsCollectorRef(nodeViewRef, peerManagerRef, settings, fakeTimeProvider)
  val route = InfoRoute(statsCollector, settings.scorexSettings.restApi, fakeTimeProvider).route
  val requiredDifficulty = BigInt(320000000)


  override def beforeAll: Unit = {
    Await.ready(initDifficulty(requiredDifficulty), actorTimeout.duration)
  }

  it should "return info" in {
    Get("/info") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received node info: $json")
      val c = json.hcursor
      c.downField("name").as[String] shouldEqual Right(settings.scorexSettings.network.nodeName)
      c.downField("appVersion").as[String] shouldEqual Right(Version.VersionString)
      c.downField("stateType").as[String] shouldEqual Right(settings.nodeSettings.stateType.stateTypeName)
      c.downField("isMining").as[Boolean] shouldEqual Right(settings.nodeSettings.mining)
      c.downField("launchTime").as[Long] shouldEqual Right(fakeTimeProvider.time())
    }
  }

  it should "should return non-exponential difficulty in json response" in {
    Get("/info") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val res = responseEntity.toString
      log.info(s"Received node info: $res")
      res should include regex """\"difficulty\" : \d+,"""
    }
  }

  "difficulty" should "be encoded with non-exponential form " in {
    val res = difficultyEncoder(requiredDifficulty)
    res.toString shouldEqual requiredDifficulty.toString
  }

  private def initDifficulty(difficulty: Difficulty): Future[Option[Difficulty]] = {
    val emptyHistory = generateHistory(
      verifyTransactions = settings.nodeSettings.verifyTransactions,
      stateType = settings.nodeSettings.stateType,
      PoPoWBootstrap = settings.nodeSettings.PoPoWBootstrap,
      blocksToKeep = settings.nodeSettings.blocksToKeep
    )
    val nBits = RequiredDifficulty.encodeCompactBits(difficulty)
    val chain = genChain(height = 5, emptyHistory, nBits)
    val history = applyChain(emptyHistory, chain)
    val generatedDifficulty = history.bestFullBlockOpt
      .map(_.header.requiredDifficulty)
      .map(difficultyEncoder.apply)
    log.info(s"Generated difficulty: $generatedDifficulty")
    statsCollector ! ChangedHistory(history)
    (statsCollector ? GetNodeInfo).mapTo[NodeInfo] map { nodeInfo =>
      val difficulty = nodeInfo.bestFullBlockOpt.map(_.header.requiredDifficulty)
      log.info(s"Set difficulty to: $difficulty")
      difficulty
    }
  }
}

