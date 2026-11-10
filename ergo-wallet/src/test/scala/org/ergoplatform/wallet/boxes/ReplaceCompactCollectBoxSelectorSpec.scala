package org.ergoplatform.wallet.boxes

import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.{ErgoBoxAssets, ErgoBoxAssetsHolder, ErgoLikeTransaction}
import org.ergoplatform.sdk.wallet.TokensMap
import org.ergoplatform.wallet.boxes.BoxSelector.{BoxSelectionError, BoxSelectionResult}
import scorex.util.ModifierId
import scala.collection.mutable
import sigmastate.helpers.TestingHelpers._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigma.ast.{ErgoTree, TrueLeaf}
import sigma.ast.syntax.SigmaPropValue

class ReplaceCompactCollectBoxSelectorSpec extends AnyPropSpec with Matchers with EitherValues {

  private val noFilter: TrackedBox => Boolean = _ => true
  val parentTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq())


  def box(value:Long) = testBox(value, ErgoTree.fromProposition(TrueLeaf.toSigmaProp), 0)
  def trackedBox(value:Long) = TrackedBox(parentTx, 0, None, box(value), Set(PaymentsScanId))

  property("legacy selection and recalculation preserve original helper overrides") {
    class LegacySelector(maxInputs: Int, optimalInputs: Int)
      extends ReplaceCompactCollectBoxSelector(maxInputs, optimalInputs, None) {
      val calls: mutable.Map[String, Int] = mutable.Map.empty.withDefaultValue(0)
      private def record(name: String): Unit = calls.update(name, calls(name) + 1)

      override def formChangeBoxes(foundBalance: Long,
                                   targetBalance: Long,
                                   foundBoxAssets: mutable.Map[ModifierId, Long],
                                   targetBoxAssets: TokensMap): Either[BoxSelectionError, Seq[ErgoBoxAssets]] = {
        record("formChange")
        super.formChangeBoxes(foundBalance, targetBalance, foundBoxAssets, targetBoxAssets)
      }

      override protected[boxes] def calcChange[T <: ErgoBoxAssets](boxes: Seq[T],
                                                                   targetBalance: Long,
                                                                   targetAssets: TokensMap): Either[BoxSelectionError, Seq[ErgoBoxAssets]] = {
        record("calcChange")
        super.calcChange(boxes, targetBalance, targetAssets)
      }

      override protected[boxes] def replace[T <: ErgoBoxAssets](bsr: BoxSelectionResult[T],
                                                                tail: Seq[T],
                                                                targetBalance: Long,
                                                                targetAssets: TokensMap): Either[BoxSelectionError, BoxSelectionResult[T]] = {
        record("replace")
        super.replace(bsr, tail, targetBalance, targetAssets)
      }

      override protected[boxes] def compress[T <: ErgoBoxAssets](bsr: BoxSelectionResult[T],
                                                                 targetBalance: Long,
                                                                 targetAssets: TokensMap): Either[BoxSelectionError, BoxSelectionResult[T]] = {
        record("compress")
        super.compress(bsr, targetBalance, targetAssets)
      }

      override protected[boxes] def collectDust[T <: ErgoBoxAssets](bsr: BoxSelectionResult[T],
                                                                    tail: Seq[T],
                                                                    targetBalance: Long,
                                                                    targetAssets: TokensMap): Either[BoxSelectionError, BoxSelectionResult[T]] = {
        record("dust")
        super.collectDust(bsr, tail, targetBalance, targetAssets)
      }

      override def select[T <: ErgoBoxAssets](inputBoxes: Iterator[T],
                                               filterFn: T => Boolean,
                                               targetBalance: Long,
                                               targetAssets: TokensMap,
                                               keepChangeToken: ModifierId => Boolean): Either[BoxSelectionError, BoxSelectionResult[T]] = {
        record("policySelect")
        super.select(inputBoxes, filterFn, targetBalance, targetAssets, keepChangeToken)
      }
    }

    val cases = Seq(
      (2, 1, Seq(2L, 3L, 4L, 10L), 8L, Seq(10L), Map("replace" -> 1)),
      (2, 1, Seq(2L, 3L, 10L), 10L, Seq(10L), Map("replace" -> 1, "compress" -> 1)),
      (10, 2, Seq(10L, 2L, 20L), 8L, Seq(10L, 2L), Map("dust" -> 1))
    )
    cases.foreach { case (max, optimal, values, target, expected, helperCalls) =>
      val legacy = new LegacySelector(max, optimal)
      val inputs = values.map(value => ErgoBoxAssetsHolder(value * 1000000L))
      val result = legacy.select(inputs.iterator, target * 1000000L, Map.empty).right.value
      result.inputBoxes.map(_.value) shouldBe expected.map(_ * 1000000L)
      result.changeBoxes.map(_.value).sum shouldBe (expected.sum - target) * 1000000L
      legacy.calls.toMap shouldBe (helperCalls ++ Map("formChange" -> 1, "calcChange" -> 1))
    }
  }

  property("compress() done properly") {
    val selector = new ReplaceCompactCollectBoxSelector(3, 2, None)

    val inputValues = Seq(100L, 1L, 2L, 200L, 1000L)
    val targetBalance = 1300

    val boxSelectionResult = new BoxSelectionResult(
      inputValues.map(trackedBox), Seq(), None
    )
    val res = selector.compress(boxSelectionResult, targetBalance, Map()).right.value
    res.inputBoxes.length shouldBe 3
    res.inputBoxes.map(_.value) shouldBe Seq(100L, 200L, 1000L)

    //now we test that compress works under select
    val sr = selector.select(inputValues.map(trackedBox).toIterator, noFilter, targetBalance, Map()).right.value
    sr.inputBoxes shouldBe res.inputBoxes
    sr.changeBoxes shouldBe res.changeBoxes
    sr.payToReemissionBox shouldBe res.payToReemissionBox
  }

  property("replace() - no candidates") {
    val selector = new ReplaceCompactCollectBoxSelector(3, 2, None)
    val inputValues = Seq(100L, 1L, 2L, 200L, 1000L)
    val targetBalance = 1303
    val boxSelectionResult = new BoxSelectionResult(inputValues.map(trackedBox), Seq(), None)
    val res = selector.replace(boxSelectionResult, Seq(), targetBalance, Map()).right.value
    res.inputBoxes.map(_.value) shouldBe inputValues
  }

  property("replace() done - partial replacement") {
    val selector = new ReplaceCompactCollectBoxSelector(3, 2, None)
    val inputValues = Seq(100L, 1L, 2L, 200L, 1000L)
    val targetBalance = 1303
    val boxSelectionResult = new BoxSelectionResult(inputValues.map(trackedBox), Seq(), None)
    val res = selector.replace(boxSelectionResult, Seq(trackedBox(300), trackedBox(200)), targetBalance, Map()).right.value
    res.inputBoxes.length shouldBe 3
    res.inputBoxes.map(_.value) shouldBe Seq(200L, 1000L, 300L)
  }

  property("replace() done - full replacement") {
    val selector = new ReplaceCompactCollectBoxSelector(3, 2, None)
    val inputValues = Seq(100L, 1L, 2L, 200L, 1000L)
    val targetBalance = 1303
    val boxSelectionResult = new BoxSelectionResult(inputValues.map(trackedBox), Seq(), None)
    val res = selector.replace(boxSelectionResult, Seq(trackedBox(2000)), targetBalance, Map()).right.value
    res.inputBoxes.length shouldBe 1
    res.inputBoxes.map(_.value) shouldBe Seq(2000L)
  }

  property("compact() and replace() under select()"){
    val selector = new ReplaceCompactCollectBoxSelector(3, 3, None)
    val inputValues = (1 to 10).map(v => trackedBox(v))

    {
      val targetBalance = 6
      val res = selector.select(inputValues.toIterator, noFilter, targetBalance, Map()).right.value
      res.inputBoxes.map(_.value) shouldBe Seq(1, 2, 3)
    }

    {
      val targetBalance = 17
      val res = selector.select(inputValues.toIterator, noFilter, targetBalance, Map()).right.value
      res.inputBoxes.map(_.value) shouldBe Seq(10, 9, 8)
    }

    {
      val targetBalance = 25
      val res = selector.select(inputValues.toIterator, noFilter, targetBalance, Map()).right.value
      res.inputBoxes.map(_.value) shouldBe Seq(10, 9, 8)
    }

    {
      val targetBalance = 27
      val res = selector.select(inputValues.toIterator, noFilter, targetBalance, Map()).right.value
      res.inputBoxes.map(_.value) shouldBe Seq(10, 9, 8)
    }
  }

  property("dust collection under select()") {
    val optimalInputs = 5
    val selector = new ReplaceCompactCollectBoxSelector(20, optimalInputs, None)
    val inputValues = (1 to 10).map(v => trackedBox(v))

    {
      val targetBalance = 6
      val res = selector.select(inputValues.toIterator, noFilter, targetBalance, Map()).right.value
      res.inputBoxes.length shouldBe optimalInputs
    }

    {
      val targetBalance = 1
      val res = selector.select(inputValues.toIterator, noFilter, targetBalance, Map()).right.value
      res.inputBoxes.length shouldBe res.inputBoxes.distinct.length
      res.inputBoxes.length shouldBe optimalInputs
    }
  }
}
