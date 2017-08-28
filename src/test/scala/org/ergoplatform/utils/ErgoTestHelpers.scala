package org.ergoplatform.utils

import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.nodeView.state.{BoxHolder, DigestState, UtxoState}
import scorex.testkit.TestkitHelpers


trait ErgoTestHelpers extends TestkitHelpers {

  def createTempDir: java.io.File = {
    val randomString = scala.util.Random.alphanumeric.take(10).mkString
    val dir = java.nio.file.Files.createTempDirectory(randomString).toFile
    dir.deleteOnExit()
    dir
  }

  def createUtxoState: UtxoState = new UtxoState(createTempDir)

  def createUtxoState(bh: BoxHolder): UtxoState = UtxoState.fromBoxHolder(bh, createTempDir)

  def createDigestState(digest: Digest): DigestState = DigestState.create(digest, createTempDir).get

}
