package org.ergoplatform.modifiers

import org.ergoplatform.utils.ErgoCorePropertyTest

class ErgoFullBlockSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  property("size counts every block section, including the mandatory extension") {
    forAll(invalidErgoFullBlockGen) { fb =>
      val expected = fb.header.size + fb.blockTransactions.size + fb.extension.size +
        fb.adProofs.map(_.size).getOrElse(0)
      fb.size shouldBe expected
      // regression guard: the extension is a mandatory section and must be counted
      // (fails on the pre-fix formula that summed only header + transactions + adProofs)
      fb.size should be > (fb.header.size + fb.blockTransactions.size + fb.adProofs.map(_.size).getOrElse(0))
    }
  }
}
