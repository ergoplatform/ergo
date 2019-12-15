package org.ergoplatform.nodeView.history.components.popow

import org.ergoplatform.modifiers.history.popow.PoPowAlgos.maxLevelOf
import org.ergoplatform.modifiers.history.popow.{PoPowProof, PoPowProofPrefix}
import org.ergoplatform.nodeView.history.HistoryTestHelpers
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoTestConstants
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.util.{Failure, Success, Try}

class ProvingPoPowProcessorSpec
    extends PropSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with ErgoTestConstants
    with HistoryTestHelpers {

  private val minHeight: Int = poPowParams.m + poPowParams.k

  property("Chain proving") {
    forAll(Gen.chooseNum(minHeight, minHeight + 200)) { height =>
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        poPowProve = true,
        BlocksToKeep
      )

      history.proveSuffix(poPowParams) shouldBe 'failure

      genChain(height, history)
        .flatMap(x => Seq(x.header, x.extension))
        .foreach(history.append)

      val result = history.proveSuffix(poPowParams)

      result shouldBe 'success
      history.getLastProof shouldBe Some(result.get)

      // Repeated proving
      val result2 = history.proveSuffix(poPowParams)

      result2 shouldBe 'success
      history.getLastProof shouldBe Some(result.get)

    }
  }

  property("Produce valid proof") {
    forAll(Gen.chooseNum(minHeight, minHeight + 200)) { height =>
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        poPowProve = true,
        BlocksToKeep
      )
      genChain(height, history)
        .flatMap(x => Seq(x.header, x.extension))
        .foreach(history.append)

      val proof = history.proveSuffix(poPowParams).get

      validate(proof) shouldBe 'success
    }
  }

  private def validPrefix(prefix: PoPowProofPrefix): Boolean = {
    val maxLevel = prefix.headersChain.tail.map(maxLevelOf).max
    assert(maxLevel < 256)
    (0 to maxLevel).exists(
      l => prefix.headersChain.count(h => maxLevelOf(h) >= l) >= prefix.m
    )
  }

  private def validate(m: PoPowProof): Try[Unit] =
    if (m.suffix.chain.lengthCompare(m.suffix.k) != 0) {
      Failure(
        new Exception(
          s"Invalid suffix length, given: ${m.suffix.chain}, required: ${m.suffix.k}"
        )
      )
    } else if (!validPrefix(m.prefix)) {
      Failure(new Exception(s"Invalid prefix length"))
    } else if (!m.prefix.chain.tail.forall(
                 _.interlinks.headOption.contains(m.prefix.chain.head.id)
               )) {
      Failure(new Exception("Chain is not anchored"))
    } else if (!m.prefix.chain.headOption.exists(
                 _.header.requiredDifficulty == settings.chainSettings.initialDifficulty
               )) {
      Failure(new Exception("Wrong genesis difficulty"))
    } else {
      Success(())
    }

}
