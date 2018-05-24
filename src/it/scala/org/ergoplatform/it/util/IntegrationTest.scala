package org.ergoplatform.it.util

import java.util.concurrent.Executors

import org.scalatest.Matchers
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext

trait IntegrationTest extends Matchers with ScalaFutures with IntegrationPatience with ScorexLogging {

  implicit def ec: ExecutionContext = IntegrationTest.defaultExecutionContext

}

object IntegrationTest {

  implicit val defaultExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

}
