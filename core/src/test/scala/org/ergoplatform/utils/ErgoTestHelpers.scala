package org.ergoplatform.utils

import org.ergoplatform.nodeView.state.{BoxHolder, DigestState, UtxoState}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.VersionTag
import scorex.crypto.authds.ADDigest
import scorex.testkit.TestkitHelpers
import scorex.testkit.utils.FileUtils


trait ErgoTestHelpers extends TestkitHelpers with FileUtils {

  def createUtxoState: UtxoState = UtxoState.create(createTempDir, None)

  def createUtxoState(bh: BoxHolder): UtxoState = UtxoState.fromBoxHolder(bh, createTempDir, None)

  def createDigestState(version: VersionTag, digest: ADDigest): DigestState =
    DigestState.create(Some(version), Some(digest), createTempDir, ErgoSettings.read(None).nodeSettings)
}
