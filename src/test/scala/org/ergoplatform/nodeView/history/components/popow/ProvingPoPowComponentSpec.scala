package org.ergoplatform.nodeView.history.components.popow

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.nodeView.history.components.EmptyBlockSectionComponent
import org.ergoplatform.nodeView.history.{ErgoHistoryReader, InMemoryHistoryStorage}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoTestConstants, HistoryTestHelpers}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.core.utils.NetworkTimeProvider

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

  property("Produce valid proof") {
    val ntp = timeProvider
    val cfg = settings
    val validator = new ErgoHistoryReader with EmptyBlockSectionComponent with PoPowBootstrapComponent {
      override protected val settings: ErgoSettings = cfg
      override protected[history] val storage: InMemoryHistoryStorage = new InMemoryHistoryStorage()
      override val powScheme: AutolykosPowScheme = chainSettings.powScheme
      override protected val timeProvider: NetworkTimeProvider = ntp
    }
    val minHeight = cfg.nodeSettings.poPowSettings.params.m + cfg.nodeSettings.poPowSettings.params.k
    forAll(Gen.chooseNum(minHeight, 2000)) { height =>
      val poPowParams = settings.nodeSettings.poPowSettings.params
      val history = generateHistory(
        verifyTransactions = true, StateType.Utxo, poPoWBootstrap = false, poPowProve = true, BlocksToKeep)
      genChain(height, history).flatMap(x => Seq(x.header, x.extension)).foreach(history.append)

      val proof = history.prove(poPowParams).get

      validator.validate(proof) shouldBe 'success
    }
  }

}
