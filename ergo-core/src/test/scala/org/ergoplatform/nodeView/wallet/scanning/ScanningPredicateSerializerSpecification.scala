package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.WalletGenerators

class ScanningPredicateSerializerSpecification extends ErgoPropertyTest with WalletGenerators {

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
