package org.ergoplatform.nodeView.state

import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoPropertyTest


class ErgoStateSpecification extends ErgoPropertyTest {

  property("generateGenesisUtxoState & generateGenesisDigestState are compliant") {
    val settings = ErgoSettings.read(None)
    val dir = createTempDir
    val rootHash = ErgoState.generateGenesisUtxoState(dir, None)._1.rootHash
    val expectedRootHash = ErgoState.generateGenesisDigestState(dir, settings.nodeSettings).rootHash
    rootHash shouldBe expectedRootHash
  }
}
