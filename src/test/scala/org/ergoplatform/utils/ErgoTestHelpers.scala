package org.ergoplatform.utils

import java.util.concurrent.Executors

import org.ergoplatform.ErgoBox
import org.ergoplatform.nodeView.state.{BoxHolder, DigestState, UtxoState}
import org.ergoplatform.settings.ErgoSettings
import org.scalatest.Matchers
import scorex.core.VersionTag
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest
import scorex.testkit.TestkitHelpers
import scorex.testkit.utils.FileUtils
import sigmastate.Values.TrueLeaf

import scala.concurrent.ExecutionContext

trait ErgoTestHelpers extends TestkitHelpers with FileUtils with Matchers with ChainGenerator with ErgoGenerators {

  val timeProvider: NetworkTimeProvider = ErgoTestHelpers.defaultTimeProvider

  def createUtxoState(): (UtxoState, BoxHolder) = {
    val boxes = (0 until 200).map(i => ErgoBox(i * 100000000L, TrueLeaf))
    val bh = BoxHolder(boxes)
    (UtxoState.fromBoxHolder(bh, createTempDir, None), bh)
  }

  def createUtxoState(bh: BoxHolder): UtxoState = UtxoState.fromBoxHolder(bh, createTempDir, None)

  def createDigestState(version: VersionTag, digest: ADDigest): DigestState =
    DigestState.create(Some(version), Some(digest), createTempDir, ErgoSettings.read(None).nodeSettings)

}

object ErgoTestHelpers {

  implicit val defaultExecutionContext:ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
  val defaultTimeProvider: NetworkTimeProvider = new NetworkTimeProvider(ErgoSettings.read(None).scorexSettings.ntp)
}