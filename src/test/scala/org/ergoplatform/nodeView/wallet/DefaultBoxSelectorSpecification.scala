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

  property("properly selects coins") {

  }
}
