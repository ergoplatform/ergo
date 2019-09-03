package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.ErgoTransactionGenerators

class ScanningPredicateSerializerSpecification extends ErgoPropertyTest with ErgoTransactionGenerators {

  property("complex or roundtrip") {
    val p = ScanningPredicateTestVectors.complexOr
    val bs = ScanningPredicateSerializer.toBytes(p)
    ScanningPredicateSerializer.parseBytes(bs) == p
  }

  property("complex and roundtrip") {
    val p = ScanningPredicateTestVectors.complexAnd
    val bs = ScanningPredicateSerializer.toBytes(p)
    ScanningPredicateSerializer.parseBytes(bs) == p
  }

}
