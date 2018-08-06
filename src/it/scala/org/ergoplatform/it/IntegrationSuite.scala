package org.ergoplatform.it

import com.typesafe.config.ConfigFactory
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{BeforeAndAfterAll, Suite, TryValues}
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext

trait IntegrationSuite
  extends  BeforeAndAfterAll
  with ErgoTestHelpers
  with TryValues
  with ScalaFutures
  with IntegrationPatience
  with ScorexLogging { this: Suite =>

  implicit def executionContext: ExecutionContext = ErgoTestHelpers.defaultExecutionContext

  protected val docker: Docker = Docker(getClass)

  override protected def beforeAll(): Unit = {
    log.debug("Starting tests")
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

  protected val noDelayConfig = ConfigFactory.parseString(
    """
      |ergo.node.miningDelay=5ms
    """.stripMargin
  )

}
