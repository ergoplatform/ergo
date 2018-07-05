package org.ergoplatform.it.util

import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.concurrent.ExecutionContext

trait IntegrationTest extends ErgoTestHelpers with ScalaFutures with IntegrationPatience {

  implicit def ec: ExecutionContext = ErgoTestHelpers.defaultExecutionContext

}
