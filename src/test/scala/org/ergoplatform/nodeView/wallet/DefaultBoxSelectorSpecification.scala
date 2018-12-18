package org.ergoplatform.nodeView.wallet


import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.{bytesToId, idToBytes}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.Values

class DefaultBoxSelectorSpecification extends ErgoPropertyTest {
  import DefaultBoxSelector.select

  private val noFilter: TrackedBox => Boolean = _ => true

  property("returns None when it is impossible to select coins") {
    val parentTx = ErgoTransaction(IndexedSeq(), IndexedSeq())
    val box = ErgoBox(1, Values.TrueLeaf, creationHeight = startHeight)
    val uBox = TrackedBox(parentTx, 0, None, box, BoxCertainty.Certain)

    //target amount is too high
    select(Seq(uBox).toIterator, noFilter, 10, Map()) shouldBe None

    //filter(which is about selecting only onchain boxes) is preventing from picking the proper box
    select(Seq(uBox).toIterator, box => box.chainStatus.onchain, 1, Map()) shouldBe None

    //no target asset in the input box
    select(Seq(uBox).toIterator, noFilter, 1, Map(bytesToId(Array.fill(32)(0: Byte)) -> 1L)) shouldBe None

    //otherwise, everything is fine
    select(Seq(uBox).toIterator, noFilter, 1, Map()).isDefined shouldBe true
  }

  property("properly selects coins - simple case with no assets") {
    val parentTx = ErgoTransaction(IndexedSeq(), IndexedSeq())
    val box1 = ErgoBox(1, Values.TrueLeaf, creationHeight = startHeight)
    val box2 = ErgoBox(10, Values.TrueLeaf, creationHeight = startHeight)
    val box3 = ErgoBox(100, Values.TrueLeaf, creationHeight = startHeight)

    val uBox1 = TrackedBox(parentTx, 0, Option(100), box1, BoxCertainty.Certain)
    val uBox2 = TrackedBox(parentTx, 1, None, box2, BoxCertainty.Certain)
    val uBox3 = TrackedBox(parentTx, 2, Option(100), box3, BoxCertainty.Certain)

    val uBoxes = Seq(uBox1, uBox2, uBox3)

    val s1 = select(uBoxes.toIterator, noFilter, 1, Map())
    s1.isDefined shouldBe true
    s1.get.changeBoxes.isEmpty shouldBe true
    s1.get.boxes.head shouldBe box1

    val s2 = select(uBoxes.toIterator, noFilter, 10, Map())
    s2.isDefined shouldBe true
    s2.get.changeBoxes.size == 1
    s2.get.changeBoxes.head._1 shouldBe 1
    s2.get.boxes shouldBe Seq(box1, box2)

    val s3 = select(uBoxes.toIterator, noFilter, 11, Map())
    s3.isDefined shouldBe true
    s3.get.changeBoxes.isEmpty shouldBe true
    s3.get.boxes shouldBe Seq(box1, box2)

    //box2 should be filtered out
    val s4 = select(uBoxes.toIterator, box => box.chainStatus.onchain, 11, Map())
    s4.isDefined shouldBe true
    s4.get.changeBoxes.size == 1
    s4.get.changeBoxes.head._1 shouldBe 90
    s4.get.boxes shouldBe Seq(box1, box3)

    val s5 = select(uBoxes.toIterator, noFilter, 61, Map())
    s5.isDefined shouldBe true
    s5.get.changeBoxes.size == 1
    s5.get.changeBoxes.head._1 shouldBe 50
    s5.get.boxes shouldBe Seq(box1, box2, box3)
  }

  property("properly selects coins - assets w. 1 change box") {

    val assetId1 = bytesToId(Blake2b256("hello"))
    val assetId2 = bytesToId(Blake2b256("world"))

    val parentTx = ErgoTransaction(IndexedSeq(), IndexedSeq())
    val box1 = ErgoBox(1, Values.TrueLeaf, startHeight, Seq(Digest32 @@ idToBytes(assetId1) -> 1))
    val box2 = ErgoBox(10, Values.TrueLeaf, startHeight, Seq(Digest32 @@ idToBytes(assetId2) -> 10))
    val box3 = ErgoBox(100, Values.TrueLeaf, startHeight, Seq(Digest32 @@ idToBytes(assetId1) -> 100))

    val uBox1 = TrackedBox(parentTx, 0, Some(100), box1, BoxCertainty.Certain)
    val uBox2 = TrackedBox(parentTx, 1, None, box2, BoxCertainty.Certain)
    val uBox3 = TrackedBox(parentTx, 2, Some(100), box3, BoxCertainty.Certain)

    val uBoxes = Seq(uBox1, uBox2, uBox3)

    val s1 = select(uBoxes.toIterator, noFilter, 1, Map(assetId1 -> 1))
    s1.isDefined shouldBe true
    s1.get.changeBoxes.isEmpty shouldBe true
    s1.get.boxes.head shouldBe box1

    val s2 = select(uBoxes.toIterator, noFilter, 1, Map(assetId1 -> 11))
    s2.isDefined shouldBe true
    s2.get.changeBoxes.size == 1
    s2.get.changeBoxes.head._1 shouldBe 100
    s2.get.changeBoxes.head._2(assetId1) shouldBe 90
    s2.get.boxes shouldBe Seq(box1, box3)

    select(uBoxes.toIterator, box => box.chainStatus.onchain, 1, Map(assetId2 -> 1)) shouldBe None
    select(uBoxes.toIterator, noFilter, 1, Map(assetId2 -> 11)) shouldBe None
    select(uBoxes.toIterator, noFilter, 1, Map(assetId1 -> 1000)) shouldBe None

    val s3 = select(uBoxes.toIterator, noFilter, 1, Map(assetId1 -> 11, assetId2 -> 1))
    s3.isDefined shouldBe true
    s3.get.changeBoxes.size == 1
    s3.get.changeBoxes.head._1 shouldBe 110
    s3.get.changeBoxes.head._2(assetId1) shouldBe 90
    s3.get.changeBoxes.head._2(assetId2) shouldBe 9
    s3.get.boxes shouldBe Seq(box1, box2, box3)

    select(uBoxes.toIterator, box => box.chainStatus.onchain, 1, Map(assetId1 -> 11, assetId2 -> 1)) shouldBe None
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

    val parentTx = ErgoTransaction(IndexedSeq(), IndexedSeq())
    val box1 = ErgoBox(1, Values.TrueLeaf, startHeight,
      Seq(Digest32 @@ idToBytes(assetId1) -> 1, Digest32 @@ idToBytes(assetId2) -> 1,
        Digest32 @@ idToBytes(assetId3) -> 1, Digest32 @@ idToBytes(assetId4) -> 1))

    val box2 = ErgoBox(10, Values.TrueLeaf, startHeight,
      Seq(Digest32 @@ idToBytes(assetId5) -> 10, Digest32 @@ idToBytes(assetId6) -> 10,
        Digest32 @@ idToBytes(assetId7) -> 10, Digest32 @@ idToBytes(assetId8) -> 10))

    val box3 = ErgoBox(100, Values.TrueLeaf, startHeight,
      Seq(Digest32 @@ idToBytes(assetId3) -> 100, Digest32 @@ idToBytes(assetId4) -> 100,
        Digest32 @@ idToBytes(assetId5) -> 100, Digest32 @@ idToBytes(assetId6) -> 100))

    val uBox1 = TrackedBox(parentTx, 0, Some(100), box1, BoxCertainty.Certain)
    val uBox2 = TrackedBox(parentTx, 1, None, box2, BoxCertainty.Certain)
    val uBox3 = TrackedBox(parentTx, 2, Some(100), box3, BoxCertainty.Certain)

    val uBoxes = Seq(uBox1, uBox2, uBox3)

    val s1 = select(uBoxes.toIterator, noFilter, 1, Map(assetId3 -> 11))
    s1.isDefined shouldBe true
    s1.get.changeBoxes.size == 2

    s1.get.changeBoxes(0)._1 shouldBe 50
    s1.get.changeBoxes(0)._2(assetId1) shouldBe 1
    s1.get.changeBoxes(0)._2(assetId2) shouldBe 1
    s1.get.changeBoxes(0)._2(assetId3) shouldBe 90
    s1.get.changeBoxes(0)._2(assetId4) shouldBe 101

    s1.get.changeBoxes(1)._1 shouldBe 50
    s1.get.changeBoxes(1)._2(assetId5) shouldBe 100
    s1.get.changeBoxes(1)._2(assetId6) shouldBe 100

    s1.get.boxes shouldBe Seq(box1, box3)

    val s2 = select(uBoxes.toIterator, noFilter, 10,
      Map(assetId1 -> 1, assetId2 -> 1, assetId3 -> 1, assetId4 -> 1))
    s2.isDefined shouldBe true
    s2.get.changeBoxes.size == 1
    s2.get.changeBoxes(0)._2(assetId5) shouldBe 10
    s2.get.changeBoxes(0)._2(assetId6) shouldBe 10
    s2.get.changeBoxes(0)._2(assetId7) shouldBe 10
    s2.get.changeBoxes(0)._2(assetId8) shouldBe 10

    //todo: should selector fail in this case (if there's no monetary value to create a new box w. assets) ?
    select(uBoxes.toIterator, noFilter, 1, Map(assetId1 -> 1)) shouldBe None
  }
}
