package org.ergoplatform.nodeView.history.components.popow

import org.ergoplatform.modifiers.history.PoPowAlgos.maxLevelOf
import org.ergoplatform.modifiers.history.{PoPowProof, PoPowProofPrefix}
import org.ergoplatform.nodeView.history.HistoryTestHelpers
import org.ergoplatform.nodeView.history.storage.StorageKeys
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoTestConstants
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.core.validation.ModifierValidator

import scala.util.Try

class ProvingPoPowComponentSpec
  extends PropSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with ErgoTestConstants
    with HistoryTestHelpers {

  private val minHeight: Int = poPowParams.m + poPowParams.k

  property("Chain proving") {
    forAll(Gen.chooseNum(minHeight, minHeight + 200)) { height =>
      val history = generateHistory(
        verifyTransactions = true, StateType.Utxo, poPowProve = true, BlocksToKeep)

      history.prove(poPowParams) shouldBe 'failure

      genChain(height, history).flatMap(x => Seq(x.header, x.extension)).foreach(history.append)

      val result = history.prove(poPowParams)

      result shouldBe 'success
      history.asInstanceOf[ProvingPoPowComponent].getLastProof shouldBe Some(result.get)
    }
  }

  property("Produce valid proof") {
    forAll(Gen.chooseNum(minHeight, minHeight + 200)) { height =>
      val history = generateHistory(
        verifyTransactions = true, StateType.Utxo, poPowProve = true, BlocksToKeep)
      genChain(height, history).flatMap(x => Seq(x.header, x.extension)).foreach(history.append)

      val proof = history.prove(poPowParams).get

      validate(proof) shouldBe 'success
    }
  }

  private def validPrefix(prefix: PoPowProofPrefix): Boolean = {
    val maxLevel = prefix.headersChain.tail.map(maxLevelOf).max
    assert(maxLevel < 256)
    (0 to maxLevel).exists(l => prefix.headersChain.count(h => maxLevelOf(h) >= l) >= prefix.m)
  }

  private def validate(m: PoPowProof): Try[Unit] =
    ModifierValidator.failFast
      .demand(
        m.suffix.chain.lengthCompare(m.suffix.k) == 0,
        s"Invalid suffix length, given: ${m.suffix.chain}, required: ${m.suffix.k}"
      )
      .demand(validPrefix(m.prefix), s"Invalid prefix length")
      .demand(
        m.prefix.chain.tail.forall(_.interlinks.headOption.contains(m.prefix.chain.head.id)),
        "Chain is not anchored"
      )
      .demand(
        m.prefix.chain.headOption.exists(_.header.requiredDifficulty == settings.chainSettings.initialDifficulty),
        "Wrong genesis difficulty"
      )
      .result
      .toTry

}
