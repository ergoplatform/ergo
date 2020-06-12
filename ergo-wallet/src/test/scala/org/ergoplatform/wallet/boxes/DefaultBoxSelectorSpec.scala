package org.ergoplatform.wallet.boxes

import org.ergoplatform.wallet.Constants.PaymentsAppId
import org.ergoplatform.{ErgoBox, ErgoLikeTransaction}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.Values
import sigmastate.Values.SigmaPropValue
import scorex.util.{bytesToId, idToBytes}
import org.scalatest.EitherValues
import org.ergoplatform.wallet.boxes.DefaultBoxSelector.NotEnoughErgsError
import org.ergoplatform.wallet.boxes.DefaultBoxSelector.NotEnoughTokensError
import org.ergoplatform.wallet.boxes.DefaultBoxSelector.NotEnoughCoinsForChangeBoxesError

class DefaultBoxSelectorSpec extends PropSpec with Matchers with EitherValues {
  import DefaultBoxSelector.select
  import BoxSelector.MinBoxValue

  private val noFilter: TrackedBox => Boolean = _ => true
  private val onChainFilter = {box: TrackedBox => box.chainStatus.onChain}
  val parentTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq())

  val TrueLeaf: SigmaPropValue = Values.TrueLeaf.toSigmaProp
  val StartHeight: Int = 0

  property("returns error when it is impossible to select coins") {
    val box = ErgoBox(1, TrueLeaf, creationHeight = StartHeight)
    val uBox = TrackedBox(parentTx, 0, None, box, Set(PaymentsAppId))

    //target amount is too high
    select(Seq(uBox).toIterator, noFilter, 10, Map()).left.value shouldBe a [NotEnoughErgsError]

    //filter(which is about selecting only onchain boxes) is preventing from picking the proper box
    select(Seq(uBox).toIterator, onChainFilter, 1, Map()).left.value shouldBe a [NotEnoughErgsError]

    //no target asset in the input box
    select(Seq(uBox).toIterator, noFilter, 1, Map(bytesToId(Array.fill(32)(0: Byte)) -> 1L)).left.value shouldBe
      a [NotEnoughTokensError]

    //otherwise, everything is fine
    select(Seq(uBox).toIterator, noFilter, 1, Map()) shouldBe 'right
  }

  property("properly selects coins - simple case with no assets") {
    val box1 = ErgoBox(1, TrueLeaf, creationHeight = StartHeight)
    val box2 = ErgoBox(10, TrueLeaf, creationHeight = StartHeight)
    val box3 = ErgoBox(100, TrueLeaf, creationHeight = StartHeight)

    val uBox1 = TrackedBox(parentTx, 0, Option(100), box1, Set(PaymentsAppId))
    val uBox2 = TrackedBox(parentTx, 1, None, box2, Set(PaymentsAppId))
    val uBox3 = TrackedBox(parentTx, 2, Option(100), box3, Set(PaymentsAppId))

    val uBoxes = Seq(uBox1, uBox2, uBox3)

    val s1 = select(uBoxes.toIterator, noFilter, 1, Map())
    s1 shouldBe 'right
    s1.right.get.changeBoxes.isEmpty shouldBe true
    s1.right.get.boxes.head shouldBe uBox1

    val s2 = select(uBoxes.toIterator, noFilter, 10, Map())
    s2 shouldBe 'right
    s2.right.get.changeBoxes.size == 1
    s2.right.get.changeBoxes.head.value shouldBe 1
    s2.right.get.boxes shouldBe Seq(uBox1, uBox2)

    val s3 = select(uBoxes.toIterator, noFilter, 11, Map())
    s3 shouldBe 'right
    s3.right.get.changeBoxes.isEmpty shouldBe true
    s3.right.get.boxes shouldBe Seq(uBox1, uBox2)

    //box2 should be filtered out
    val s4 = select(uBoxes.toIterator, onChainFilter, 11, Map())
    s4 shouldBe 'right
    s4.right.get.changeBoxes.size == 1
    s4.right.get.changeBoxes.head.value shouldBe 90
    s4.right.get.boxes shouldBe Seq(uBox1, uBox3)

    val s5 = select(uBoxes.toIterator, noFilter, 61, Map())
    s5 shouldBe 'right
    s5.right.get.changeBoxes.size == 1
    s5.right.get.changeBoxes.head.value shouldBe 50
    s5.right.get.boxes shouldBe Seq(uBox1, uBox2, uBox3)
  }

  property("properly selects coins - assets w. 1 change box") {

    val assetId1 = bytesToId(Blake2b256("hello"))
    val assetId2 = bytesToId(Blake2b256("world"))

    val parentTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq())
    val box1 = ErgoBox(1 * MinBoxValue, TrueLeaf, StartHeight, Seq(Digest32 @@ idToBytes(assetId1) -> 1))
    val box2 = ErgoBox(10 * MinBoxValue, TrueLeaf, StartHeight, Seq(Digest32 @@ idToBytes(assetId2) -> 10))
    val box3 = ErgoBox(100 * MinBoxValue, TrueLeaf, StartHeight, Seq(Digest32 @@ idToBytes(assetId1) -> 100))

    val uBox1 = TrackedBox(parentTx, 0, Some(100), box1, Set(PaymentsAppId))
    val uBox2 = TrackedBox(parentTx, 1, None, box2, Set(PaymentsAppId))
    val uBox3 = TrackedBox(parentTx, 2, Some(100), box3, Set(PaymentsAppId))

    val uBoxes = Seq(uBox1, uBox2, uBox3)

    val s1 = select(uBoxes.toIterator, noFilter, 1 * MinBoxValue, Map(assetId1 -> 1))
    s1 shouldBe 'right
    s1.right.get.changeBoxes.isEmpty shouldBe true
    s1.right.get.boxes.head shouldBe uBox1

    val s2 = select(uBoxes.toIterator, noFilter, 1 * MinBoxValue, Map(assetId1 -> 11))
    s2 shouldBe 'right
    s2.right.get.changeBoxes.size == 1
    s2.right.get.changeBoxes.head.value shouldBe 100 * MinBoxValue
    s2.right.get.changeBoxes.head.tokens(assetId1) shouldBe 90
    s2.right.get.boxes shouldBe Seq(uBox1, uBox3)

    select(uBoxes.toIterator, onChainFilter, 1, Map(assetId2 -> 1)).left.value shouldBe a [NotEnoughTokensError]
    select(uBoxes.toIterator, noFilter, 1, Map(assetId2 -> 11)).left.value shouldBe a [NotEnoughTokensError]
    select(uBoxes.toIterator, noFilter, 1, Map(assetId1 -> 1000)).left.value shouldBe a [NotEnoughTokensError]

    val s3 = select(uBoxes.toIterator, noFilter, 1 * MinBoxValue, Map(assetId1 -> 11, assetId2 -> 1))
    s3 shouldBe 'right
    s3.right.get.changeBoxes.size == 1
    s3.right.get.changeBoxes.head.value shouldBe 110 * MinBoxValue
    s3.right.get.changeBoxes.head.tokens(assetId1) shouldBe 90
    s3.right.get.changeBoxes.head.tokens(assetId2) shouldBe 9
    s3.right.get.boxes shouldBe Seq(uBox1, uBox2, uBox3)

    select(uBoxes.toIterator, onChainFilter, 1 * MinBoxValue, Map(assetId1 -> 11, assetId2 -> 1)).left.value shouldBe 
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

    val box1 = ErgoBox(1 * MinBoxValue, TrueLeaf, StartHeight,
      Seq(Digest32 @@ idToBytes(assetId1) -> 1, Digest32 @@ idToBytes(assetId2) -> 1,
        Digest32 @@ idToBytes(assetId3) -> 1, Digest32 @@ idToBytes(assetId4) -> 1))

    val box2 = ErgoBox(10 * MinBoxValue, TrueLeaf, StartHeight,
      Seq(Digest32 @@ idToBytes(assetId5) -> 10, Digest32 @@ idToBytes(assetId6) -> 10,
        Digest32 @@ idToBytes(assetId7) -> 10, Digest32 @@ idToBytes(assetId8) -> 10))

    val box3 = ErgoBox(100 * MinBoxValue, TrueLeaf, StartHeight,
      Seq(Digest32 @@ idToBytes(assetId3) -> 100, Digest32 @@ idToBytes(assetId4) -> 100,
        Digest32 @@ idToBytes(assetId5) -> 100, Digest32 @@ idToBytes(assetId6) -> 100))

    val uBox1 = TrackedBox(parentTx, 0, Some(100), box1, Set(PaymentsAppId))
    val uBox2 = TrackedBox(parentTx, 1, None, box2, Set(PaymentsAppId))
    val uBox3 = TrackedBox(parentTx, 2, Some(100), box3, Set(PaymentsAppId))

    val uBoxes = Seq(uBox1, uBox2, uBox3)

    val s1 = select(uBoxes.toIterator, noFilter, 1 * MinBoxValue, Map(assetId3 -> 11))
    s1 shouldBe 'right

    s1.right.get.boxes.size shouldBe 2
    s1.right.get.boxes should contain theSameElementsAs(Seq(uBox1, uBox3))

    s1.right.get.changeBoxes.size shouldBe 1
    s1.right.get.changeBoxes(0).value shouldBe 100 * MinBoxValue
    s1.right.get.changeBoxes(0).tokens(assetId1) shouldBe 1
    s1.right.get.changeBoxes(0).tokens(assetId2) shouldBe 1
    s1.right.get.changeBoxes(0).tokens(assetId3) shouldBe 90
    s1.right.get.changeBoxes(0).tokens(assetId4) shouldBe 101
    s1.right.get.changeBoxes(0).tokens(assetId5) shouldBe 100
    s1.right.get.changeBoxes(0).tokens(assetId6) shouldBe 100

    s1.right.get.boxes shouldBe Seq(uBox1, uBox3)

    val s2 = select(uBoxes.toIterator, noFilter, 10 * MinBoxValue,
      Map(assetId1 -> 1, assetId2 -> 1, assetId3 -> 1, assetId4 -> 1))
    s2 shouldBe 'right
    s2.right.get.changeBoxes.size == 1
    s2.right.get.changeBoxes(0).tokens(assetId5) shouldBe 10
    s2.right.get.changeBoxes(0).tokens(assetId6) shouldBe 10
    s2.right.get.changeBoxes(0).tokens(assetId7) shouldBe 10
    s2.right.get.changeBoxes(0).tokens(assetId8) shouldBe 10

    //todo: should selector fail in this case (if there's no monetary value to create a new box w. assets) ?
    select(uBoxes.toIterator, noFilter, 1 * MinBoxValue, Map(assetId1 -> 1)).left.value shouldBe a [NotEnoughCoinsForChangeBoxesError]
  }

}
