package org.ergoplatform.it.util

import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext

trait IntegrationTest extends ErgoTestHelpers with ScalaFutures with IntegrationPatience with ScorexLogging {

  implicit def ec: ExecutionContext = ErgoTestHelpers.defaultExecutionContext

}
