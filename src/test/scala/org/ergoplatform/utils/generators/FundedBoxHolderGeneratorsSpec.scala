package org.ergoplatform.utils.generators

import org.ergoplatform.utils.BoxUtils
import org.ergoplatform.utils.generators.ErgoCoreGenerators.trueLeafGen
import org.ergoplatform.utils.ErgoNodeTestConstants.extendedParameters
import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sigmastate.eval.Extensions._

class FundedBoxHolderGeneratorsSpec extends AnyFlatSpec with Matchers {
  "Generated box holders" should "fund small input groups and conserve their value" in {
    val minimum = BoxUtils.sufficientAmount(extendedParameters)
    // This seed includes a value below the transaction generator's conservative floor.
    val holder = boxesHolderGenOfSize(5).pureApply(Gen.Parameters.default, Seed(803L))
    holder.size shouldBe 5
    val boxes = holder.boxes.values.toIndexedSeq
    Seq(1, 2).foreach { inputCount =>
      boxes.grouped(inputCount).foreach { inputs =>
        val tx = validUnsignedTransactionFromBoxes(inputs, issueNew = false)
        tx.inputs.map(_.boxId.toSeq) shouldBe inputs.map(_.id.toSeq)
        tx.outputCandidates.map(_.value).sum shouldBe inputs.map(_.value).sum
        tx.outputCandidates.foreach { output =>
          output.value should be >= minimum
          output.additionalTokens.length shouldBe 0
        }
      }
    }
    boxes.foreach(_.value should be >= minimum)
  }

  it should "preserve supplied tokens when the node generator funds a box" in {
    val token = Array.fill[Byte](32)(1).toTokenId
    val box = ergoBoxGenForTokens(Seq(token -> 7L), trueLeafGen)
      .pureApply(Gen.Parameters.default, Seed(803L))
    val tx = validUnsignedTransactionFromBoxes(IndexedSeq(box), issueNew = false)
    val tokens = tx.outputCandidates.flatMap(_.additionalTokens.toArray)
    box.value should be >= BoxUtils.sufficientAmount(extendedParameters)
    tx.outputCandidates.map(_.value).sum shouldBe box.value
    tokens.map(_._1.toArray.toSeq).distinct shouldBe Seq(token.toArray.toSeq)
    tokens.map(_._2).sum shouldBe 7L
  }
}
