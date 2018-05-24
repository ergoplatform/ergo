package org.ergoplatform.it

import com.typesafe.config.ConfigFactory
import org.ergoplatform.it.util.IntegrationTest
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Suite}

import scala.collection.JavaConverters._
import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.util.control.NonFatal

class TestFourNodesSuite extends FreeSpec with BeforeAndAfterAll with IntegrationTest {

  private val nonGeneratingPeerConfig = ConfigFactory.parseString(
    """
      |ergo.node.mining=no
    """.stripMargin
  )

  private val docker: Docker = Docker(getClass)

  private val nodesCount = 4
  private val dockerConfigs = Random.shuffle(Docker.nodeConfigs.getConfigList("nodes").asScala).take(nodesCount)
  private val nodeConfigs = nonGeneratingPeerConfig.withFallback(dockerConfigs.head) +: dockerConfigs.tail
  private val futureNodes: Future[Seq[Node]] = docker.startNodes(nodeConfigs)

  override protected def beforeAll(): Unit = {
    log.debug("Starting tests")
  }

  override def nestedSuites: IndexedSeq[Suite] = {
    val futureSuits = futureNodes.map(nodeSuites)
    futureSuits.recover {
      case NonFatal(e) => fail(e); IndexedSeq.empty
    }
    Await.result(futureSuits, 3.minutes)
  }

  private def nodeSuites(nodes: Seq[Node]): IndexedSeq[Suite] = IndexedSeq(
    new SynchronizationTestSpec(nodes)
  )

  override protected def afterAll(): Unit = docker.close()
}
