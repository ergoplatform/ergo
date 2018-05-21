package org.ergoplatform.it

import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers, Suite}
import scorex.core.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.collection.immutable.IndexedSeq
import scala.util.Random

class TestFourNodesSuite extends FreeSpec with BeforeAndAfterAll with ScorexLogging with Matchers {

  private val nonGeneratingPeerConfig = ConfigFactory.parseString(
    """
      |ergo.node.mining=no
    """.stripMargin
  )

  private val docker: Docker = {
    Docker.cleanupResourcesIfNeeded()
    Docker(getClass)
  }
  private val nodesCount = 4
  private val dockerConfigs = Random.shuffle(Docker.NodeConfigs.getConfigList("nodes").asScala).take(nodesCount)
  private val nodeConfigs = Seq(nonGeneratingPeerConfig.withFallback(dockerConfigs.head)) ++ dockerConfigs.tail


  private val allNodes = docker.startNodes(nodeConfigs)
  private val notMiner = allNodes.head

  override protected def beforeAll(): Unit = {
    log.debug("Starting tests")
  }

  override def nestedSuites: IndexedSeq[Suite] = IndexedSeq(
    new SynchronizationTestSpec(allNodes)
  )

  override protected def afterAll(): Unit = docker.close()
}
