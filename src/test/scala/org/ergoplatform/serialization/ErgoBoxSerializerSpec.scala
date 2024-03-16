package org.ergoplatform.serialization

import org.ergoplatform.ErgoBox
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.util.serialization.VLQByteStringWriter
import scala.util.Try

class ErgoBoxSerializerSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.wallet.utils.WalletGenerators._

  property("ErgoBox serialization") {
    forAll(ergoBoxGen) { b: ErgoBox =>
      val bs = ErgoBoxSerializer.toBytes(b)
      val b2 = ErgoBoxSerializer.parseBytes(bs)
      b shouldBe b2
    }
  }

  property("creation height overflow") {
    // helper method which creates bypassing Scala API, via changing binary representation
    def overflowHeight(box: ErgoBox): Try[ErgoBox] = {
      val hBytes = (new VLQByteStringWriter).putUInt(box.creationHeight).toBytes

      val bs = ErgoBoxSerializer.toBytes(box)
      val pos = bs.indexOfSlice(hBytes, 0)

      val before = bs.slice(0, pos)
      val after = bs.slice(pos + hBytes.length, bs.length)

      val overflowHeight = 0xFFFFFFFFL
      val overBytes = (new VLQByteStringWriter).putUInt(overflowHeight).toBytes

      ErgoBoxSerializer.parseBytesTry(before ++ overBytes ++ after)
    }

    forAll(ergoBoxGen) { b: ErgoBox =>
      val h = Int.MaxValue
      val ob = new ErgoBox(b.value, b.ergoTree, b.additionalTokens, b.additionalRegisters, b.transactionId, b.index, h)
      // starting from 5.0.0 sigma interpreter, Int overflow is thrown
      overflowHeight(ob).isFailure shouldBe true
    }
  }

}
