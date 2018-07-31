package org.ergoplatform.nodeView.wallet

import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoPropertyTest

class ErgoAddressSpecification extends ErgoPropertyTest {
  property("P2PKH roundtrip") { forAll(proveDlogGen) { pk =>
    val settings = ErgoSettings.read(None)
    val p2pkh = P2PKHAddress(pk)
    val encoder = ErgoAddressEncoder(settings)
    encoder.fromString(encoder.toString(p2pkh)).get shouldBe p2pkh
  }}

  property("P2PK roundtrip") { forAll(proveDlogGen) { pk =>
    val settings = ErgoSettings.read(None)
    val p2pk = P2PKAddress(pk)
    val encoder = ErgoAddressEncoder(settings)
    encoder.fromString(encoder.toString(p2pk)).get shouldBe p2pk
  }}
}
