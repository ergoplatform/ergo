package org.ergoplatform.utils

import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.nodeView.state.{BoxHolder, DigestState, UtxoState}
import scorex.testkit.TestkitHelpers
import scorex.testkit.utils.FileUtils


trait ErgoTestHelpers extends TestkitHelpers with FileUtils {

  def createUtxoState: UtxoState = new UtxoState(createTempDir)

  def createUtxoState(bh: BoxHolder): UtxoState = UtxoState.fromBoxHolder(bh, createTempDir)

  def createDigestState(digest: Digest): DigestState = DigestState.create(digest, createTempDir).get

}
