package org.ergoplatform.nodeView.state

import org.ergoplatform.utils.{ErgoGenerators, ErgoTestHelpers}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}


class ErgoStateSpecification  extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with ErgoTestHelpers {

  ignore("generateGenesisUtxoState & generateGenesisDigestState are compliant") {
    withDir("/tmp/ergostate1") {dir =>
      ErgoState.generateGenesisUtxoState(dir).rootHash shouldBe ErgoState.generateGenesisDigestState(dir).rootHash
    }
  }
}
