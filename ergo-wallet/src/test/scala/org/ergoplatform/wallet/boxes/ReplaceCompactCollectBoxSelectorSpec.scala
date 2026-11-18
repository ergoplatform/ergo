package org.ergoplatform.wallet.boxes

import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.{ErgoBoxAssetsHolder, ErgoLikeTransaction}
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import org.ergoplatform.wallet.boxes.DefaultBoxSelector.NotEnoughCoinsForChangeBoxesError
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector.MaxInputsExceededError
import scorex.util.bytesToId
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

  property("optional token dust cannot replace a valid selection with insufficient change") {
    val token = bytesToId(Array.fill[Byte](32)(1))
    val funding = ErgoBoxAssetsHolder(BoxSelector.MinBoxValue)
    val dust = ErgoBoxAssetsHolder(1L, Map(token -> 1L))
    val inputs = Seq(funding, dust)
    val accept: ErgoBoxAssetsHolder => Boolean = _ => true
    val selector = new ReplaceCompactCollectBoxSelector(3, 2, None)
    val initial = new DefaultBoxSelector(None).select(inputs.iterator, accept, funding.value, Map.empty).right.value
    initial.inputBoxes shouldBe Seq(funding)
    selector.calcChange(inputs, funding.value, Map.empty).left.value shouldBe a[NotEnoughCoinsForChangeBoxesError]
    val result = selector.select(inputs.iterator, accept, funding.value, Map.empty).right.value
    result.inputBoxes shouldBe initial.inputBoxes
    result.changeBoxes shouldBe initial.changeBoxes
    result.payToReemissionBox shouldBe initial.payToReemissionBox
    selector.collectDust(initial, Seq(dust), funding.value, Map.empty).right.value should be theSameInstanceAs initial
  }

  property("optional token dust is collected when its change is funded") {
    val token = bytesToId(Array.fill[Byte](32)(2))
    val funding = ErgoBoxAssetsHolder(BoxSelector.MinBoxValue)
    val dust = ErgoBoxAssetsHolder(BoxSelector.MinBoxValue, Map(token -> 1L))
    val selector = new ReplaceCompactCollectBoxSelector(3, 2, None)
    val accept: ErgoBoxAssetsHolder => Boolean = _ => true
    val result = selector.select(Seq(funding, dust).iterator, accept, funding.value, Map.empty).right.value
    result.inputBoxes shouldBe Seq(funding, dust)
    result.changeBoxes shouldBe Seq(dust)
    result.payToReemissionBox shouldBe None
  }

  property("required token inputs above the limit return the typed maximum input error") {
    val token = bytesToId(Array.fill[Byte](32)(3))
    val inputs = (1 to 3).map(n => ErgoBoxAssetsHolder(BoxSelector.MinBoxValue * n, Map(token -> 1L)))
    val target = inputs.map(_.value).sum
    val accept: ErgoBoxAssetsHolder => Boolean = _ => true
    new DefaultBoxSelector(None).select(inputs.iterator, accept, target, Map(token -> 3L)).isRight shouldBe true
    val selector = new ReplaceCompactCollectBoxSelector(2, 2, None)
    selector.select(inputs.iterator, accept, target, Map(token -> 3L)).left.value shouldBe a[MaxInputsExceededError]
  }

  property("compression preserves a selection with no removable inputs") {
    val selector = new ReplaceCompactCollectBoxSelector(2, 2, None)
    val empty = new BoxSelectionResult[ErgoBoxAssetsHolder](Seq.empty, Seq.empty, None)
    selector.compress(empty, 0L, Map.empty).right.value shouldBe empty
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
