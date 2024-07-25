package org.ergoplatform.wallet.boxes

import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.ErgoLikeTransaction
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import sigmastate.Values
import sigmastate.Values.SigmaPropValue
import sigmastate.helpers.TestingHelpers._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class ReplaceCompactCollectBoxSelectorSpec extends AnyPropSpec with Matchers with EitherValues {

  private val noFilter: TrackedBox => Boolean = _ => true
  val parentTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq())

  val TrueLeaf: SigmaPropValue = Values.TrueLeaf.toSigmaProp

  def box(value:Long) = testBox(value, TrueLeaf, 0)
  def trackedBox(value:Long) = TrackedBox(parentTx, 0, None, box(value), Set(PaymentsScanId))

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
