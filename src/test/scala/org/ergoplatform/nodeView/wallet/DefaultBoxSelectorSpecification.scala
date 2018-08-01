package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.utils.ErgoPropertyTest
import sigmastate.Values

class DefaultBoxSelectorSpecification extends ErgoPropertyTest {
  import DefaultBoxSelector.select

  property("returns None when it is impossible to select coins") {
    val parentTx = ErgoTransaction(IndexedSeq(), IndexedSeq())
    val box = ErgoBox(1, Values.TrueLeaf)
    val uBox = UnspentOffchainBox(parentTx, 0, box, BoxCertainty.Certain)

    //target amount is too high
    select(Seq(uBox).toIterator, _ => true, 10, Map()) shouldBe None

    //filter(which is about selecting only onchain boxes) is preventing from picking the proper box
    select(Seq(uBox).toIterator, box => box.onchain, 1, Map()) shouldBe None

    //no target asset in the input box
    select(Seq(uBox).toIterator, _ => true, 1, Map(ByteArrayWrapper(Array.fill(32)(0:Byte)) -> 1L)) shouldBe None

    //otherwise, everything is fine
    select(Seq(uBox).toIterator, _ => true, 1, Map()).isDefined shouldBe true
  }

  property("properly selects coins - simple case with no assets") {
    val parentTx = ErgoTransaction(IndexedSeq(), IndexedSeq())
    val box1 = ErgoBox(1, Values.TrueLeaf)
    val box2 = ErgoBox(10, Values.TrueLeaf)
    val box3 = ErgoBox(100, Values.TrueLeaf)

    val uBox1 = UnspentOnchainBox(parentTx, 0, 100, box1, BoxCertainty.Certain)
    val uBox2 = UnspentOffchainBox(parentTx, 1, box2, BoxCertainty.Certain)
    val uBox3 = UnspentOnchainBox(parentTx, 2, 100, box3, BoxCertainty.Certain)

    val uBoxes = Seq(uBox1, uBox2, uBox3)

    val s1 = select(uBoxes.toIterator, _ => true, 1, Map())
    s1.isDefined shouldBe true
    s1.get.changeBoxes.isEmpty shouldBe true
    s1.get.boxes.head shouldBe box1

    val s2 = select(uBoxes.toIterator, _ => true, 10, Map())
    s2.isDefined shouldBe true
    s2.get.changeBoxes.size == 1
    s2.get.changeBoxes.head._1 shouldBe 1
    s2.get.boxes shouldBe Seq(box1, box2)

    val s3 = select(uBoxes.toIterator, _ => true, 11, Map())
    s3.isDefined shouldBe true
    s3.get.changeBoxes.isEmpty shouldBe true
    s3.get.boxes shouldBe Seq(box1, box2)

    //box2 should be filtered out
    val s4 = select(uBoxes.toIterator, box => box.onchain, 11, Map())
    s4.isDefined shouldBe true
    s4.get.changeBoxes.size == 1
    s4.get.changeBoxes.head._1 shouldBe 90
    s4.get.boxes shouldBe Seq(box1, box3)

    val s5 = select(uBoxes.toIterator, _ => true, 61, Map())
    s5.isDefined shouldBe true
    s5.get.changeBoxes.size == 1
    s5.get.changeBoxes.head._1 shouldBe 50
    s5.get.boxes shouldBe Seq(box1, box2, box3)
  }
}
