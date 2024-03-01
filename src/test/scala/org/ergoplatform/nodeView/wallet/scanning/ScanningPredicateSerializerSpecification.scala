package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.utils.ErgoCorePropertyTest

class ScanningPredicateSerializerSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._

  property("complex or roundtrip") {
    forAll(scanningPredicateGen) { p =>
      val bs = ScanningPredicateSerializer.toBytes(p)
      ScanningPredicateSerializer.parseBytes(bs) == p
    }
  }

  property("complex and roundtrip") {
    forAll(scanningPredicateGen) { p =>
      val bs = ScanningPredicateSerializer.toBytes(p)
      ScanningPredicateSerializer.parseBytes(bs) == p
    }
  }

}
