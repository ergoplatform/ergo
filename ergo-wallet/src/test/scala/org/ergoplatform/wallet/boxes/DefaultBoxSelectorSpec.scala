package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.ErgoLikeTransaction
import org.ergoplatform.SigmaConstants.MaxBoxSize
import org.ergoplatform.sdk.wallet.Constants.MaxAssetsPerBox
import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.wallet.boxes.DefaultBoxSelector.{NotEnoughErgsError, NotEnoughTokensError}
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.hash.Blake2b256
import scorex.util.bytesToId
import sigmastate.Values
import sigmastate.Values.SigmaPropValue
import sigmastate.eval.Extensions._
import sigmastate.helpers.TestingHelpers._
import sigmastate.utils.Extensions._
import special.collection.Extensions._

import scala.util.Random

class DefaultBoxSelectorSpec extends AnyPropSpec with Matchers with EitherValues {
  import BoxSelector.MinBoxValue

  private val noFilter: TrackedBox => Boolean = _ => true
  private val onChainFilter = {box: TrackedBox => box.chainStatus.onChain}
  private val parentTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq())

  private val TrueLeaf: SigmaPropValue = Values.TrueLeaf.toSigmaProp
  private val StartHeight: Int = 0

  private def genTokens(count: Int) = {
    (0 until count).map { i => Blake2b256(i.toString).toTokenId -> i.toLong }
  }

  private val selector = new DefaultBoxSelector(None)

  property("returns error when it is impossible to select coins") {
    val box = testBox(1, TrueLeaf, creationHeight = StartHeight)
    val uBox = TrackedBox(parentTx, 0, None, box, Set(PaymentsScanId))

    //target amount is too high
    selector.select(Seq(uBox).toIterator, noFilter, 10, Map()).left.value shouldBe a [NotEnoughErgsError]

    //filter(which is about selecting only onchain boxes) is preventing from picking the proper box
    selector.select(Seq(uBox).toIterator, onChainFilter, 1, Map()).left.value shouldBe a [NotEnoughErgsError]

    //no target asset in the input box
    selector.select(Seq(uBox).toIterator, noFilter, 1, Map(bytesToId(Array.fill(32)(0: Byte)) -> 1L)).left.value shouldBe
      a [NotEnoughTokensError]

    //otherwise, everything is fine
    selector.select(Seq(uBox).toIterator, noFilter, 1, Map()) shouldBe 'right
  }

  property("properly selects coins - simple case with no assets") {
    val box1 = testBox(1, TrueLeaf, creationHeight = StartHeight)
    val box2 = testBox(10, TrueLeaf, creationHeight = StartHeight)
    val box3 = testBox(100, TrueLeaf, creationHeight = StartHeight)

    val uBox1 = TrackedBox(parentTx, 0, Option(100), box1, Set(PaymentsScanId))
    val uBox2 = TrackedBox(parentTx, 1, None, box2, Set(PaymentsScanId))
    val uBox3 = TrackedBox(parentTx, 2, Option(100), box3, Set(PaymentsScanId))

    val uBoxes = Seq(uBox1, uBox2, uBox3)

    val s1 = selector.select(uBoxes.toIterator, noFilter, 1, Map())
    s1 shouldBe 'right
    s1.right.get.changeBoxes.isEmpty shouldBe true
    s1.right.get.inputBoxes.head shouldBe uBox1

    val s2 = selector.select(uBoxes.toIterator, noFilter, 10, Map())
    s2 shouldBe 'right
    s2.right.get.changeBoxes.size == 1
    s2.right.get.changeBoxes.head.value shouldBe 1
    s2.right.get.inputBoxes shouldBe Seq(uBox1, uBox2)

    val s3 = selector.select(uBoxes.toIterator, noFilter, 11, Map())
    s3 shouldBe 'right
    s3.right.get.changeBoxes.isEmpty shouldBe true
    s3.right.get.inputBoxes shouldBe Seq(uBox1, uBox2)

    //box2 should be filtered out
    val s4 = selector.select(uBoxes.toIterator, onChainFilter, 11, Map())
    s4 shouldBe 'right
    s4.right.get.changeBoxes.size == 1
    s4.right.get.changeBoxes.head.value shouldBe 90
    s4.right.get.inputBoxes shouldBe Seq(uBox1, uBox3)

    val s5 = selector.select(uBoxes.toIterator, noFilter, 61, Map())
    s5 shouldBe 'right
    s5.right.get.changeBoxes.size == 1
    s5.right.get.changeBoxes.head.value shouldBe 50
    s5.right.get.inputBoxes shouldBe Seq(uBox1, uBox2, uBox3)
  }

  property("properly selects coins - assets w. 1 change box") {

    val assetId1 = bytesToId(Blake2b256("hello"))
    val assetId2 = bytesToId(Blake2b256("world"))

    val parentTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq())
    val box1 = testBox(1 * MinBoxValue, TrueLeaf, StartHeight, Seq(assetId1.toTokenId -> 1))
    val box2 = testBox(10 * MinBoxValue, TrueLeaf, StartHeight, Seq(assetId2.toTokenId -> 10))
    val box3 = testBox(100 * MinBoxValue, TrueLeaf, StartHeight, Seq(assetId1.toTokenId -> 100))

    val uBox1 = TrackedBox(parentTx, 0, Some(100), box1, Set(PaymentsScanId))
    val uBox2 = TrackedBox(parentTx, 1, None, box2, Set(PaymentsScanId))
    val uBox3 = TrackedBox(parentTx, 2, Some(100), box3, Set(PaymentsScanId))

    val uBoxes = Seq(uBox1, uBox2, uBox3)

    val s1 = selector.select(uBoxes.toIterator, noFilter, 1 * MinBoxValue, Map(assetId1 -> 1))
    s1 shouldBe 'right
    s1.right.get.changeBoxes.isEmpty shouldBe true
    s1.right.get.inputBoxes.head shouldBe uBox1

    val s2 = selector.select(uBoxes.toIterator, noFilter, 1 * MinBoxValue, Map(assetId1 -> 11))
    s2 shouldBe 'right
    s2.right.get.changeBoxes.size == 1
    s2.right.get.changeBoxes.head.value shouldBe 100 * MinBoxValue
    s2.right.get.changeBoxes.head.tokens(assetId1) shouldBe 90
    s2.right.get.inputBoxes shouldBe Seq(uBox1, uBox3)

    selector.select(uBoxes.toIterator, onChainFilter, 1, Map(assetId2 -> 1)).left.value shouldBe a [NotEnoughTokensError]
    selector.select(uBoxes.toIterator, noFilter, 1, Map(assetId2 -> 11)).left.value shouldBe a [NotEnoughTokensError]
    selector.select(uBoxes.toIterator, noFilter, 1, Map(assetId1 -> 1000)).left.value shouldBe a [NotEnoughTokensError]

    val s3 = selector.select(uBoxes.toIterator, noFilter, 1 * MinBoxValue, Map(assetId1 -> 11, assetId2 -> 1))
    s3 shouldBe 'right
    s3.right.get.changeBoxes.size == 1
    s3.right.get.changeBoxes.head.value shouldBe 110 * MinBoxValue
    s3.right.get.changeBoxes.head.tokens(assetId1) shouldBe 90
    s3.right.get.changeBoxes.head.tokens(assetId2) shouldBe 9
    s3.right.get.inputBoxes shouldBe Seq(uBox1, uBox2, uBox3)

    selector.select(uBoxes.toIterator, onChainFilter, 1 * MinBoxValue, Map(assetId1 -> 11, assetId2 -> 1)).left.value shouldBe
      a [NotEnoughTokensError]
  }

  property("properly selects coins - assets w. multiple change boxes") {
    val assetId1 = bytesToId(Blake2b256("1"))
    val assetId2 = bytesToId(Blake2b256("2"))
    val assetId3 = bytesToId(Blake2b256("3"))
    val assetId4 = bytesToId(Blake2b256("4"))
    val assetId5 = bytesToId(Blake2b256("5"))
    val assetId6 = bytesToId(Blake2b256("6"))
    val assetId7 = bytesToId(Blake2b256("7"))
    val assetId8 = bytesToId(Blake2b256("8"))

    val box1 = testBox(
      1 * MinBoxValue, TrueLeaf, StartHeight,
      Seq(assetId1.toTokenId -> 1, assetId2.toTokenId -> 1,
          assetId3.toTokenId -> 1, assetId4.toTokenId -> 1))

    val box2 = testBox(
      10 * MinBoxValue, TrueLeaf, StartHeight,
      Seq(assetId5.toTokenId -> 10, assetId6.toTokenId -> 10,
          assetId7.toTokenId -> 10, assetId8.toTokenId -> 10))

    val box3 = testBox(
      100 * MinBoxValue, TrueLeaf, StartHeight,
      Seq(assetId3.toTokenId -> 100, assetId4.toTokenId -> 100,
        assetId5.toTokenId -> 100, assetId6.toTokenId -> 100))

    val uBox1 = TrackedBox(parentTx, 0, Some(100), box1, Set(PaymentsScanId))
    val uBox2 = TrackedBox(parentTx, 1, None, box2, Set(PaymentsScanId))
    val uBox3 = TrackedBox(parentTx, 2, Some(100), box3, Set(PaymentsScanId))

    val uBoxes = Seq(uBox1, uBox2, uBox3)

    val s1 = selector.select(uBoxes.toIterator, noFilter, 1 * MinBoxValue, Map(assetId3 -> 11))
    s1 shouldBe 'right

    s1.right.get.inputBoxes.size shouldBe 3
    s1.right.get.inputBoxes should contain theSameElementsAs(Seq(uBox1, uBox2, uBox3))

    s1.right.get.changeBoxes.size shouldBe 1
    s1.right.get.changeBoxes(0).value shouldBe 110 * MinBoxValue
    s1.right.get.changeBoxes(0).tokens(assetId1) shouldBe 1
    s1.right.get.changeBoxes(0).tokens(assetId2) shouldBe 1
    s1.right.get.changeBoxes(0).tokens(assetId3) shouldBe 90
    s1.right.get.changeBoxes(0).tokens(assetId4) shouldBe 101
    s1.right.get.changeBoxes(0).tokens(assetId5) shouldBe 110
    s1.right.get.changeBoxes(0).tokens(assetId6) shouldBe 110

    val s2 = selector.select(uBoxes.toIterator, noFilter, 10 * MinBoxValue,
      Map(assetId1 -> 1, assetId2 -> 1, assetId3 -> 1, assetId4 -> 1))
    s2 shouldBe 'right
    s2.right.get.changeBoxes.size == 1
    s2.right.get.changeBoxes(0).tokens(assetId5) shouldBe 10
    s2.right.get.changeBoxes(0).tokens(assetId6) shouldBe 10
    s2.right.get.changeBoxes(0).tokens(assetId7) shouldBe 10
    s2.right.get.changeBoxes(0).tokens(assetId8) shouldBe 10

    selector.select(uBoxes.toIterator, noFilter, 1 * MinBoxValue, Map(assetId1 -> 1)).isRight shouldBe true
  }

  property("Size of a box with MaxAssetsPerBox tokens should not cross MaxBoxSize") {
    val tokens = (0 until MaxAssetsPerBox).map { _ =>
      (scorex.util.Random.randomBytes(TokenId.size).toTokenId, Random.nextInt(100000000).toLong)
    }
    val box = testBox(1 * MinBoxValue, TrueLeaf, StartHeight, tokens)
    assert(box.bytes.length <= MaxBoxSize.value)
  }

  property("Select boxes such that change boxes are grouped by MaxAssetsPerBox") {
    // make selection such that '2 * MaxAssetsPerBox + 1' tokens generates exactly 2 change boxes with MaxAssetsPerBox tokens
    val box1 = testBox(4 * MinBoxValue, TrueLeaf, StartHeight, genTokens(2 * MaxAssetsPerBox + 1))
    val uBox1 = TrackedBox(parentTx, 0, Some(100), box1, Set(PaymentsScanId))
    val s1 = selector.select(Iterator(uBox1), noFilter, 1 * MinBoxValue, Map(bytesToId(Blake2b256("1")) -> 1))
    s1 shouldBe 'right
    s1.right.get.changeBoxes.size shouldBe 2
    s1.right.get.changeBoxes.forall(_.tokens.size == MaxAssetsPerBox) shouldBe true

    // make selection such that '2 * MaxAssetsPerBox + 2' tokens generates 3 change boxes, one with just a single token
    val box2 = testBox(4 * MinBoxValue, TrueLeaf, StartHeight, genTokens(2 * MaxAssetsPerBox + 2))
    val uBox2 = TrackedBox(parentTx, 0, Some(100), box2, Set(PaymentsScanId))
    val s2 = selector.select(Iterator(uBox2), noFilter, 1 * MinBoxValue, Map(bytesToId(Blake2b256("1")) -> 1))
    s2 shouldBe 'right
    s2.right.get.changeBoxes.size shouldBe 3
    s2.right.get.changeBoxes.exists(_.tokens.size == 1) shouldBe true
  }

  // test which shows that https://github.com/ergoplatform/ergo/issues/1644 fixed
  property("i1644: should collect needed inputs when needed for change in presence of assets") {
    val tokenData = genTokens(3).last
    tokenData._2 shouldBe 2

    val tokenId = tokenData._1.toModifierId

    val ergValue = 10 * MinBoxValue

    val box1 = testBox(ergValue, TrueLeaf, StartHeight, Seq(tokenData))
    val uBox1 = TrackedBox(parentTx, 0, Some(100), box1, Set(PaymentsScanId))
    val box2 = testBox(ergValue, TrueLeaf, StartHeight)
    val uBox2 = TrackedBox(parentTx, 0, Some(100), box2, Set(PaymentsScanId))

    val s1 = selector.select(Iterator(uBox1, uBox2), noFilter, ergValue, Map.empty)
    s1 shouldBe 'right
    s1.right.get.changeBoxes.size shouldBe 1
    s1.right.get.changeBoxes.head.tokens(tokenId) shouldBe 2

    val box3 = testBox(ergValue, TrueLeaf, StartHeight)
    val uBox3 = TrackedBox(parentTx, 0, Some(100), box3, Set(PaymentsScanId))

    val s2 = selector.select(Iterator(uBox2, uBox3), noFilter, ergValue, Map.empty)
    s2 shouldBe 'right
    s2.right.get.changeBoxes.size shouldBe 0

    val s3 = selector.select(Iterator(uBox1, uBox2), noFilter, ergValue, Map(tokenId -> 1))
    s3 shouldBe 'right
    s3.right.get.changeBoxes.size shouldBe 1

  }


  property("Properly handle EIP-27") {
    val ts = genTokens(2)
    val reemissionNftId = ts(0)._1
    val reemissionTokenId = ts(1)._1
    val selector = new DefaultBoxSelector(
      Some(ReemissionData(reemissionNftId.toModifierId, reemissionTokenId.toModifierId)))

    val fullValue = 2000000000L
    val reemissionAmt = fullValue / 2

    val inputBox = testBox(fullValue, TrueLeaf, StartHeight, Array((reemissionTokenId, reemissionAmt)))
    val uBox = TrackedBox(parentTx, 0, Some(100), inputBox, Set(PaymentsScanId))

    val s1 = selector.select(Iterator(uBox), noFilter, fullValue - reemissionAmt + 1, Map.empty)

    s1.left.get.isInstanceOf[NotEnoughErgsError] shouldBe true

    val s2 = selector.select(Iterator(uBox), noFilter, (fullValue - reemissionAmt) / 4, Map.empty)

    val cb2 = s2.right.get.payToReemissionBox

    cb2.get.value shouldBe reemissionAmt
  }
}
