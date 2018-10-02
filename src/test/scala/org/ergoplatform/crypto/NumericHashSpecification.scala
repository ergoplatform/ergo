package org.ergoplatform.crypto

import org.bouncycastle.util.BigIntegers
import org.ergoplatform.crypto.pow.NumericHash
import org.ergoplatform.utils.ErgoPropertyTest

class NumericHashSpecification extends ErgoPropertyTest {

  property("Hash value belongs to an interval") {
    forAll(genBoundedBytes(1, 32)) { message: Array[Byte] =>
      val p = BigIntegers.fromUnsignedByteArray(message)
      val hash = new NumericHash(p)
      val resp = hash.hash(message)
      resp >= 0 shouldBe true
      resp < p shouldBe true
    }
  }


}
