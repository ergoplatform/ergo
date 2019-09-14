package org.ergoplatform.nodeView.history.components.popow

import com.google.common.primitives.Ints
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.nodeView.history.components.{EmptyBlockSectionComponent, VoidLogging}
import org.ergoplatform.nodeView.history.storage.StorageKeys
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

  private def makeComponent(cfg: ErgoSettings) =
    new ErgoHistoryReader with EmptyBlockSectionComponent with PoPowBootstrapComponent with VoidLogging {
      override protected val settings: ErgoSettings = cfg
      override protected[history] val storage: InMemoryHistoryStorage = new InMemoryHistoryStorage()
      override val powScheme: AutolykosPowScheme = chainSettings.powScheme
      override protected val timeProvider: NetworkTimeProvider = ntp
    }

  private def makeCfg(proofsToCheck: Int): ErgoSettings =
    settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        stateType = StateType.Digest,
        poPowSettings = settings.nodeSettings.poPowSettings.copy(minProofsToCheck = proofsToCheck)
      )
    )

  property("Indexes are updated correctly when history is PoPow bootstrapped") {
    val cfg = makeCfg(proofsToCheck = 1)
    val poPowParams = cfg.nodeSettings.poPowSettings.params
    forAll(validNiPoPowProofGen(poPowParams.m, poPowParams.k)(poPowParams)) { proof =>
      val history = makeComponent(cfg)
      history.process(proof)

      val bestHeader = proof.suffix.chain.last
      val historyStorage = history.storage.asInstanceOf[InMemoryHistoryStorage]

      historyStorage.indexes.get(StorageKeys.BestHeaderKey).map(bytesToId) shouldBe Some(bestHeader.id)

      historyStorage.indexes
        .get(history.headerHeightKey(bestHeader.id)).map(Ints.fromByteArray) shouldBe Some(bestHeader.height)

      history.isEmpty shouldBe false
    }
  }

  property("Indexes are updated correctly when better proof is applied") {
    val cfg = makeCfg(proofsToCheck = 2)
    val poPowParams = cfg.nodeSettings.poPowSettings.params
    forAll(validNiPoPowProofGen(poPowParams.m, poPowParams.k)(poPowParams),
      validNiPoPowProofGen(poPowParams.m, poPowParams.k)(poPowParams)) { (proof0, proof1) =>
      val history = makeComponent(cfg)
      history.process(proof0)

      val storage = history.storage.asInstanceOf[InMemoryHistoryStorage]

      storage.indexes.get(StorageKeys.BestHeaderKey).map(bytesToId) shouldBe None

      storage.indexes
        .get(StorageKeys.BestProofIdKey).map(bytesToId) shouldBe Some(proof0.id)

      history.isEmpty shouldBe true

      history.process(proof1)

      history.isEmpty shouldBe false
    }
  }

  property("Proof validation") {
    val cfg = makeCfg(proofsToCheck = 1)
    val poPowParams = cfg.nodeSettings.poPowSettings.params

    // valid proof
    forAll(validNiPoPowProofGen(poPowParams.m, poPowParams.k)(poPowParams)) { proof =>
      val history = makeComponent(cfg)
      history.validate(proof) shouldBe 'success
    }

    // invalid proof (invalid suffix)
    forAll(validNiPoPowProofGen(poPowParams.m, poPowParams.k)(poPowParams)) { proof =>
      val history = makeComponent(cfg)
      val invalidProof = proof.copy(suffix = proof.suffix.copy(chain = proof.suffix.chain.init))
      history.validate(invalidProof) shouldBe 'failure
    }

    // invalid proof (invalid prefix)
    forAll(validNiPoPowProofGen(poPowParams.m, poPowParams.k)(poPowParams)) { proof =>
      val history = makeComponent(cfg)
      val invalidProof = proof.copy(prefix = proof.prefix.copy(chain = proof.prefix.chain.tail))
      history.validate(invalidProof) shouldBe 'failure
    }
  }

}
