package org.ergoplatform.it

import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers, Suite}
import scorex.core.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.collection.immutable.IndexedSeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class TestFourNodesSuite extends FreeSpec with BeforeAndAfterAll with ScorexLogging with Matchers {

  private val nonGeneratingPeerConfig = ConfigFactory.parseString(
    """
      |ergo.node.mining=no
    """.stripMargin
  )

  private val docker = Docker(getClass)
  private val nodesCount = 4
  private val dockerConfigs = Random.shuffle(Docker.NodeConfigs.getConfigList("nodes").asScala).take(nodesCount)
  private val nodeConfigs = Seq(nonGeneratingPeerConfig.withFallback(dockerConfigs.head)) ++ dockerConfigs.tail


  private val allNodes = nodeConfigs.map(docker.startNode)
  private val notMiner = allNodes.head

  override protected def beforeAll(): Unit = {
    log.debug("Waiting for nodes to start")
    Await.result(Future.traverse(allNodes)(_.status), 1.minute)

    log.debug("Waiting for nodes to connect")
    val peersCounts = Await.result(
      for {
        count <- Future.traverse(allNodes)(_.waitForPeers(nodesCount - 1))
      } yield count, 1.minute
    )

    peersCounts.foreach(c => log.info(s"Connected peers: $c"))

    all(peersCounts.map(_.length)) shouldEqual nodesCount - 1

    log.debug("Starting tests")
  }

  override def nestedSuites: IndexedSeq[Suite] = IndexedSeq(
    new SyncronizationTestSpec(allNodes)
  )
  override protected def afterAll(): Unit = docker.close()
}
