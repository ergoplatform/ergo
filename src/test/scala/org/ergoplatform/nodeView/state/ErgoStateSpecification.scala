package org.ergoplatform.nodeView.state

import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.{Matchers, PropSpec}


class ErgoStateSpecification extends PropSpec with Matchers with ErgoTestHelpers {

  property("generateGenesisUtxoState & generateGenesisDigestState are compliant") {
    val settings = ErgoSettings.read(None)
    val dir = createTempDir
    val rootHash = ErgoState.generateGenesisUtxoState(dir)._1.rootHash
    val expectedRootHash = ErgoState.generateGenesisDigestState(dir, settings.nodeSettings).rootHash
    rootHash shouldBe expectedRootHash
  }
}
