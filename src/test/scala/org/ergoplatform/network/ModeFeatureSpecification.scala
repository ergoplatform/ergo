package org.ergoplatform.network

import org.ergoplatform.utils.ErgoPropertyTest
import scala.util.Try


class ModeFeatureSpecification extends ErgoPropertyTest {

  property("serialization roundtrip") {
    forAll(modeFeatureGen) {mf =>
      mf.serializer.parseBytes(mf.serializer.toBytes(mf)) shouldEqual mf
    }
  }

  property("additional bytes can be added") {
    forAll(modeFeatureGen) { mf =>
      val bs = mf.serializer.toBytes(mf)
      mf.serializer.parseBytes(bs ++ Array(1: Byte, 2: Byte, 3: Byte)) shouldEqual mf
    }
  }

  property("serialization of too big byte array fails") {
    forAll(modeFeatureGen) { mf =>
      val bs = mf.serializer.toBytes(mf)
      Try(mf.serializer.parseBytes(bs ++ Array.fill(512)(0: Byte))).isFailure shouldBe true
    }
  }

}
