package org.ergoplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import org.ergoplatform.it.util.IntegrationTest
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Suite}

import scala.collection.immutable.IndexedSeq
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

trait IntegrationSuite extends FreeSpec with BeforeAndAfterAll with IntegrationTest {

  protected def nodeConfigs: Seq[Config]
  protected def nodeSuites(nodes: Seq[Node]): IndexedSeq[Suite]

  protected val docker: Docker = Docker(getClass)
  protected lazy val futureNodes: Future[Seq[Node]] = docker.startNodes(nodeConfigs)

  override protected def beforeAll(): Unit = {
    log.debug("Starting tests")
  }

  override def nestedSuites: IndexedSeq[Suite] = {
    val futureSuits = futureNodes
      .map(nodeSuites)
      .recover {
        case e =>
          fail(e)
          IndexedSeq.empty
      }
    Await.result(futureSuits, 3.minutes)
  }

  override protected def afterAll(): Unit = docker.close()

  protected val nonGeneratingPeerConfig = ConfigFactory.parseString(
    """
      |ergo.node.mining=false
    """.stripMargin
  )

  protected val onlineGeneratingPeerConfig = ConfigFactory.parseString(
    """
      |ergo.node.mining=true
      |ergo.node.offlineGeneration=false
    """.stripMargin
  )

}
