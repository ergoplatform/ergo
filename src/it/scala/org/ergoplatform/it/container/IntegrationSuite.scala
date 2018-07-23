package org.ergoplatform.it.container

import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{BeforeAndAfterAll, OptionValues, Suite, TryValues}
import scorex.core.utils.ScorexLogging

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

}
