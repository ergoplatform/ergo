package org.ergoplatform.utils

import org.ergoplatform.nodeView.state.{BoxHolder, DigestState, UtxoState}
import scorex.core.VersionTag
import scorex.crypto.authds.ADDigest
import scorex.testkit.TestkitHelpers
import scorex.testkit.utils.FileUtils


trait ErgoTestHelpers extends TestkitHelpers with FileUtils {

  def createUtxoState: UtxoState = UtxoState.create(createTempDir)

  def createUtxoState(bh: BoxHolder): UtxoState = UtxoState.fromBoxHolder(bh, createTempDir)

  def createDigestState(version: VersionTag, digest: ADDigest): DigestState =
    DigestState.create(Some(version), Some(digest), createTempDir).get
}
