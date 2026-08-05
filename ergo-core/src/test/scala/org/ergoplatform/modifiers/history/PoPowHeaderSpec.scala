package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.history.popow.PoPowHeader
import org.ergoplatform.modifiers.history.popow.PoPowHeader.checkInterlinksProof
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen
import org.scalacheck.Gen
import scorex.util.{ModifierId, bytesToId}

class PoPowHeaderSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private def deterministicId(value: Byte): ModifierId = bytesToId(Array.fill(32)(value))

  property("Check interlinks proof should be true") {
    forAll(Gen.nonEmptyListOf(modifierIdGen)) { interlinks =>
      val interlinksProof = NipopowAlgos.proofForInterlinkVector(nipopowAlgos.interlinksToExtension(interlinks)).get
      checkInterlinksProof(interlinks, interlinksProof) shouldBe true
    }
  }

  property("Check invalid interlinks proof should be false") {
    forAll(Gen.nonEmptyListOf(modifierIdGen), Gen.nonEmptyListOf(modifierIdGen)) { (interlinks1, interlinks2) =>
        val interlinksProof = NipopowAlgos.proofForInterlinkVector(nipopowAlgos.interlinksToExtension(interlinks2)).get
        checkInterlinksProof(interlinks1, interlinksProof) shouldBe false
    }
  }

  property("empty interlinks proof is accepted for genesis") {
    val extension = nipopowAlgos.interlinksToExtension(Seq.empty)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get

    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 1, extensionRoot = extension.digest), Seq.empty, proof)
        .checkInterlinksProof() shouldBe true
    }
  }

  property("empty interlinks proof is rejected for non-genesis headers") {
    val extension = nipopowAlgos.interlinksToExtension(Seq.empty)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get

    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = extension.digest), Seq.empty, proof)
        .checkInterlinksProof() shouldBe false
    }
  }

  property("a canonical run of 255 identical interlinks is accepted") {
    val interlinks = Seq.fill(255)(deterministicId(1))
    val extension = nipopowAlgos.interlinksToExtension(interlinks)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get

    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = extension.digest), interlinks, proof)
        .checkInterlinksProof() shouldBe true
    }
  }

  property("a run of 256 identical interlinks is rejected") {
    val interlinks = Seq.fill(256)(deterministicId(1))
    val extension = nipopowAlgos.interlinksToExtension(interlinks)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get

    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = extension.digest), interlinks, proof)
        .checkInterlinksProof() shouldBe false
    }
  }

  property("a new interlink run beginning at position 256 is rejected") {
    val first = deterministicId(1)
    val second = deterministicId(2)
    val third = deterministicId(3)
    val interlinks = Seq.fill(255)(first) ++ Seq(second, third)
    val extension = nipopowAlgos.interlinksToExtension(interlinks)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get

    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = extension.digest), interlinks, proof)
        .checkInterlinksProof() shouldBe false
    }
  }

  property("a closed interlink id cannot reopen in a later run") {
    val first = deterministicId(1)
    val second = deterministicId(2)
    val interlinks = Seq(first, second, first)
    val extension = nipopowAlgos.interlinksToExtension(interlinks)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get

    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = extension.digest), interlinks, proof)
        .checkInterlinksProof() shouldBe false
    }
  }
}
