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

      val hBytes = (new VLQByteStringWriter).putUInt(h).toBytes

      val bs = ErgoBoxSerializer.toBytes(ob)
      val pos = bs.indexOfSlice(hBytes, 0)

      val before = bs.slice(0, pos)
      val after = bs.slice(pos + hBytes.length, bs.length)

      // Check that slicing and encoding done properly
      ErgoBoxSerializer.parseBytes(before ++ hBytes ++ after) shouldBe ob

      val overflowHeight = 0xFFFFFFFFL
      val overBytes = (new VLQByteStringWriter).putUInt(overflowHeight).toBytes

      val ob2 = ErgoBoxSerializer.parseBytes(before ++ overBytes ++ after)
      println("ch: " + ob2.creationHeight)
    }
  }

}
