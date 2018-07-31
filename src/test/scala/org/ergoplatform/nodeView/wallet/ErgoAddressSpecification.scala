package org.ergoplatform.nodeView.wallet

import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalatest.Assertion

class ErgoAddressSpecification extends ErgoPropertyTest {

  def addressRoundtrip(addr:ErgoAddress): Assertion = {
    val settings = ErgoSettings.read(None)
    val encoder = ErgoAddressEncoder(settings)
    encoder.fromString(encoder.toString(addr)).get shouldBe addr
  }

  property("P2PKH roundtrip") { forAll(proveDlogGen) { pk =>
    addressRoundtrip(P2PKHAddress(pk))
  }}

  property("P2PK roundtrip") { forAll(proveDlogGen) { pk =>
    addressRoundtrip(P2PKAddress(pk))
  }}

  property("SHA roundtrip") { forAll(ergoPropositionGen) { pk =>
    addressRoundtrip(ScriptHashAddress(pk))
  }}

  property("SA roundtrip") { forAll(ergoPropositionGen) { pk =>
    addressRoundtrip(ScriptAddress(pk))
  }}
}