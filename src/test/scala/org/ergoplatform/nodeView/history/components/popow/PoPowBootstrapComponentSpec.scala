package org.ergoplatform.nodeView.history.components.popow

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.nodeView.history.components.EmptyBlockSectionComponent
import org.ergoplatform.nodeView.history.{ErgoHistoryReader, InMemoryHistoryStorage}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoTestConstants
import org.ergoplatform.utils.generators.ErgoGenerators
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.core.utils.NetworkTimeProvider
import scorex.util.bytesToId

class PoPowBootstrapComponentSpec
  extends PropSpec
    with Matchers
    with ErgoTestConstants
    with ErgoGenerators
    with GeneratorDrivenPropertyChecks {

  private val ntp = timeProvider

  private def bakeComponent(cfg: ErgoSettings) =
    new ErgoHistoryReader with EmptyBlockSectionComponent with PoPowBootstrapComponent {
      override protected val settings: ErgoSettings = cfg
      override protected[history] val historyStorage: InMemoryHistoryStorage = new InMemoryHistoryStorage()
      override val powScheme: AutolykosPowScheme = chainSettings.powScheme
      override protected val timeProvider: NetworkTimeProvider = ntp
    }

  property("Best proof application") {
    val cfg = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        stateType = StateType.Digest,
        poPowSettings = settings.nodeSettings.poPowSettings.copy(minProofsToCheck = 1)
      ),
    )
    forAll(validNiPoPowProofGen(cfg.nodeSettings.poPowSettings.params.k)) { proof =>
      val history = bakeComponent(cfg)
      history.process(proof)

      history.historyStorage
        .asInstanceOf[InMemoryHistoryStorage]
        .indexes.get(history.BestHeaderKey).map(bytesToId) shouldBe Some(proof.suffix.chain.last.id)
    }
  }

}
