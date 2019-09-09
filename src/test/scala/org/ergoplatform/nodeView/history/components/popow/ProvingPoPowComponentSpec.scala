package org.ergoplatform.nodeView.history.components.popow

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.{ErgoTestConstants, HistoryTestHelpers}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ProvingPoPowComponentSpec
  extends PropSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with ErgoTestConstants
    with HistoryTestHelpers {

  property("Chain proving") {
    forAll(Gen.chooseNum(1000, 2000)) { height =>
      val poPowParams = settings.nodeSettings.poPowSettings.params
      val history = generateHistory(
        verifyTransactions = true, StateType.Utxo, poPoWBootstrap = false, poPowProve = true, BlocksToKeep)

      history.prove(poPowParams) shouldBe 'failure

      genChain(height, history).flatMap(x => Seq(x.header, x.extension)).foreach(history.append)

      history.prove(poPowParams) shouldBe 'success
    }
  }

}
