package org.ergoplatform.it.container

import com.typesafe.config.ConfigFactory
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{BeforeAndAfterAll, OptionValues, Suite, TryValues}
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContext

trait IntegrationSuite
  extends  BeforeAndAfterAll
  with IntegrationTestConstants
  with ErgoTestHelpers
  with TryValues
  with OptionValues
  with ScalaFutures
  with IntegrationPatience
  with ScorexLogging { this: Suite =>

  implicit def executionContext: ExecutionContext = ErgoTestHelpers.defaultExecutionContext

  protected val docker: Docker = new Docker(tag = getClass.getSimpleName)

  override protected def beforeAll(): Unit = {
    log.debug("Starting tests")
  }

  override protected def afterAll(): Unit = docker.close()

  override val nonGeneratingPeerConfig = ConfigFactory.parseString(
    """
      |ergo.node.mining=false
    """.stripMargin
  )

  override val onlineGeneratingPeerConfig = ConfigFactory.parseString(
    """
      |ergo.node.mining=true
      |ergo.node.offlineGeneration=false
    """.stripMargin
  )

  override val noDelayConfig = ConfigFactory.parseString(
    """
      |ergo.node.miningDelay=5ms
    """.stripMargin
  )

}
