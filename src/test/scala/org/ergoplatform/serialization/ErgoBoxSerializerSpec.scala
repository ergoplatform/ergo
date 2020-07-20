package org.ergoplatform.serialization

import org.ergoplatform.ErgoBox
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.util.serialization.VLQByteStringWriter

class ErgoBoxSerializerSpec extends ErgoPropertyTest {

  property("ErgoBox serialization") {
    forAll(ergoBoxGen) { b: ErgoBox =>
      val bs = ErgoBoxSerializer.toBytes(b)
      val b2 = ErgoBoxSerializer.parseBytes(bs)
      b shouldBe b2
    }
  }

  property("creation height overflow") {
    forAll(ergoBoxGen){b: ErgoBox =>
      val h = Int.MaxValue
      val ob = ErgoBox(b.value, b.ergoTree, h, b.additionalTokens.toMap.toSeq, b.additionalRegisters, b.transactionId, b.index)
      val ob2 = ErgoBoxSerializerSpec.overflowHeight(ob)
      (ob2.creationHeight < 0) shouldBe true
    }
  }

}

object ErgoBoxSerializerSpec {

  // helper method which creates bypassing Scala API, via changing binary representation
  def overflowHeight(box: ErgoBox): ErgoBox = {
    val hBytes = (new VLQByteStringWriter).putUInt(box.creationHeight).toBytes

    val bs = ErgoBoxSerializer.toBytes(box)
    val pos = bs.indexOfSlice(hBytes, 0)

    val before = bs.slice(0, pos)
    val after = bs.slice(pos + hBytes.length, bs.length)

    val overflowHeight = 0xFFFFFFFFL
    val overBytes = (new VLQByteStringWriter).putUInt(overflowHeight).toBytes

    ErgoBoxSerializer.parseBytes(before ++ overBytes ++ after)
  }

}
