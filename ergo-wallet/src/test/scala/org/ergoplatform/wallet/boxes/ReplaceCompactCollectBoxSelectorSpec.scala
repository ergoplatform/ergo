package org.ergoplatform.wallet.boxes

import org.ergoplatform.{ErgoBox, ErgoLikeTransaction}
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import org.scalatest.{Matchers, PropSpec}
import sigmastate.Values
import sigmastate.Values.SigmaPropValue

class ReplaceCompactCollectBoxSelectorSpec extends PropSpec with Matchers {

  private val noFilter: TrackedBox => Boolean = _ => true
  val parentTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq())

  val TrueLeaf: SigmaPropValue = Values.TrueLeaf.toSigmaProp

  def box(value:Long) = ErgoBox(value, TrueLeaf, 0)
  def trackedBox(value:Long) = TrackedBox(parentTx, 0, None, box(value), BoxCertainty.Certain, 1)

  property("compress() done propery") {
    val selector = new ReplaceCompactCollectBoxSelector(3, 2)

    val inputValues = Seq(100L, 1L, 2L, 200L, 1000L)
    val targetBalance = 1300

    val boxSelectionResult = BoxSelectionResult(
      inputValues.map(box), Seq()
    )
    val res = selector.compress(boxSelectionResult, targetBalance, Map()).get
    res.boxes.length shouldBe 3
    res.boxes.map(_.value) shouldBe Seq(100L, 200L, 1000L)

    //now we test that compress works under select
    val sr = selector.select(inputValues.map(trackedBox).toIterator, noFilter, targetBalance, Map()).get
    sr shouldBe res
  }

  property("replace() - no candidates") {
    val selector = new ReplaceCompactCollectBoxSelector(3, 2)
    val inputValues = Seq(100L, 1L, 2L, 200L, 1000L)
    val targetBalance = 1303
    val boxSelectionResult = BoxSelectionResult(inputValues.map(box), Seq())
    val res = selector.replace(boxSelectionResult, Seq(), targetBalance, Map()).get
    res.boxes.map(_.value) shouldBe inputValues
  }

  property("replace() done - partial replacement") {
    val selector = new ReplaceCompactCollectBoxSelector(3, 2)
    val inputValues = Seq(100L, 1L, 2L, 200L, 1000L)
    val targetBalance = 1303
    val boxSelectionResult = BoxSelectionResult(inputValues.map(box), Seq())
    val res = selector.replace(boxSelectionResult, Seq(trackedBox(300), trackedBox(200)), targetBalance, Map()).get
    res.boxes.length shouldBe 3
    res.boxes.map(_.value) shouldBe Seq(200L, 1000L, 300L)
  }

  property("replace() done - full replacement") {
    val selector = new ReplaceCompactCollectBoxSelector(3, 2)
    val inputValues = Seq(100L, 1L, 2L, 200L, 1000L)
    val targetBalance = 1303
    val boxSelectionResult = BoxSelectionResult(inputValues.map(box), Seq())
    val res = selector.replace(boxSelectionResult, Seq(trackedBox(2000)), targetBalance, Map()).get
    res.boxes.length shouldBe 1
    res.boxes.map(_.value) shouldBe Seq(2000L)
  }

  property("compact() and replace() under select()"){
    val selector = new ReplaceCompactCollectBoxSelector(3, 3)
    val inputValues = (1 to 10).map(v => trackedBox(v))

    {
      val targetBalance = 6
      val res = selector.select(inputValues.toIterator, noFilter, targetBalance, Map()).get
      res.boxes.map(_.value) shouldBe Seq(1, 2, 3)
    }

    {
      val targetBalance = 17
      val res = selector.select(inputValues.toIterator, noFilter, targetBalance, Map()).get
      res.boxes.map(_.value) shouldBe Seq(10, 9, 8)
    }

    {
      val targetBalance = 25
      val res = selector.select(inputValues.toIterator, noFilter, targetBalance, Map()).get
      res.boxes.map(_.value) shouldBe Seq(10, 9, 8)
    }

    {
      val targetBalance = 27
      val res = selector.select(inputValues.toIterator, noFilter, targetBalance, Map()).get
      res.boxes.map(_.value) shouldBe Seq(10, 9, 8)
    }
  }

  property("dust collection under select()") {
    val optimalInputs = 5
    val selector = new ReplaceCompactCollectBoxSelector(20, optimalInputs)
    val inputValues = (1 to 10).map(v => trackedBox(v))

    {
      val targetBalance = 6
      val res = selector.select(inputValues.toIterator, noFilter, targetBalance, Map()).get
      res.boxes.length shouldBe optimalInputs
    }
  }
}
