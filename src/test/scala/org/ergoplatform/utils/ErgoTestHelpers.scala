package org.ergoplatform.utils

import java.util.concurrent.Executors

import org.ergoplatform.settings.ErgoSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexEncoding, ScorexLogging}

import scala.concurrent.ExecutionContext

trait ErgoTestHelpers extends ValidBlocksGenerators with ScorexLogging with ScorexEncoding {

  val timeProvider: NetworkTimeProvider = ErgoTestHelpers.defaultTimeProvider

}

object ErgoTestHelpers {

  implicit val defaultExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
  val defaultTimeProvider: NetworkTimeProvider = new NetworkTimeProvider(ErgoSettings.read(None).scorexSettings.ntp)
}
