package org.ergoplatform.network

import org.ergoplatform.utils.ErgoCorePropertyTest
import scala.util.Try

class ModePeerFeatureSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  //roundtrip test is provided in SerializationTests

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
