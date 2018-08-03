package org.ergoplatform.nodeView.wallet

import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalatest.Assertion
import sigmastate.serialization.ValueSerializer

class ErgoAddressSpecification extends ErgoPropertyTest {

  def addressRoundtrip(addr: ErgoAddress): Assertion = {
    val settings = ErgoSettings.read(None)
    val encoder = ErgoAddressEncoder(settings)
    encoder.fromString(encoder.toString(addr)).get shouldBe addr
  }

  property("P2PK roundtrip") {
    forAll(proveDlogGen) { pk =>
      addressRoundtrip(P2PKAddress(pk))
    }
  }

  property("SHA roundtrip") {
    forAll(ergoPropositionGen) { pk =>
      addressRoundtrip(ScriptHashAddress(pk))
    }
  }

  property("SA roundtrip") {
    forAll(ergoPropositionGen) { pk =>
      addressRoundtrip(ScriptAddress(pk))
    }
  }

  property("P2SH proper bytes to track") {
    forAll(ergoPropositionGen) { s =>
      val p2sh = ScriptHashAddress(s)
      ValueSerializer.serialize(p2sh.script).containsSlice(p2sh.contentBytes) shouldBe true
    }
  }

  property("P2S proper bytes to track") {
    forAll(ergoPropositionGen) { s =>
      val p2s = ScriptAddress(s)
      ValueSerializer.serialize(p2s.script).containsSlice(p2s.contentBytes) shouldBe true
    }
  }
}