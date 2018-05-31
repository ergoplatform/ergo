package org.ergoplatform.utils

import java.util.concurrent.Executors

import akka.actor.ActorRef
import org.ergoplatform.nodeView.state.{BoxHolder, DigestState, ErgoState, UtxoState}
import org.ergoplatform.settings.ErgoSettings
import org.scalatest.Matchers
import scorex.core.VersionTag
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest
import scorex.testkit.TestkitHelpers
import scorex.testkit.utils.FileUtils

import scala.concurrent.ExecutionContext

trait ErgoTestHelpers extends ValidBlocksGenerators {

  val timeProvider: NetworkTimeProvider = ErgoTestHelpers.defaultTimeProvider

}

object ErgoTestHelpers {

  implicit val defaultExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
  val defaultTimeProvider: NetworkTimeProvider = new NetworkTimeProvider(ErgoSettings.read(None).scorexSettings.ntp)
}