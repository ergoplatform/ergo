package org.ergoplatform.nodeView.wallet

import org.ergoplatform._
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalatest.Assertion
import sigmastate.serialization.ValueSerializer

class ErgoAddressSpecification extends ErgoPropertyTest {

  private implicit val ergoAddressEncoder = new ErgoAddressEncoder(settings.chainSettings.addressPrefix)

  def addressRoundtrip(addr: ErgoAddress): Assertion = {
    ergoAddressEncoder.fromString(ergoAddressEncoder.toString(addr)).get shouldBe addr
  }

  property("P2PK roundtrip") {
    forAll(proveDlogGen) { pk =>
      addressRoundtrip(P2PKAddress(pk))
    }
  }

  property("SHA roundtrip") {
    forAll(ergoPropositionGen) { pk =>
      addressRoundtrip(Pay2SHAddress(pk))
    }
  }

  property("SA roundtrip") {
    forAll(ergoPropositionGen) { pk =>
      addressRoundtrip(Pay2SAddress(pk))
    }
  }

  property("P2SH proper bytes to track") {
    forAll(ergoPropositionGen) { s =>
      val p2sh = Pay2SHAddress(s)

      //search we're doing to find a box potentially corresponding to some address
      ValueSerializer.serialize(p2sh.script).containsSlice(p2sh.contentBytes) shouldBe true
    }
  }

  property("P2S proper bytes to track") {
    forAll(ergoPropositionGen) { s =>
      val p2s = Pay2SAddress(s)

      //search we're doing to find a box potentially corresponding to some address
      ValueSerializer.serialize(p2s.script).containsSlice(p2s.contentBytes) shouldBe true
    }
  }
}
