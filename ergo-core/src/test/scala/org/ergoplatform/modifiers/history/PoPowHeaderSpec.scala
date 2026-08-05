package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.history.popow.PoPowHeader
import org.ergoplatform.modifiers.history.popow.PoPowHeader.checkInterlinksProof
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen
import org.scalacheck.Gen
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId}

class PoPowHeaderSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private def deterministicId(value: Byte): ModifierId = bytesToId(Array.fill(32)(value))

  private def mixedExtension(interlinks: Seq[ModifierId]): ExtensionCandidate = {
    nipopowAlgos.interlinksToExtension(interlinks) ++ ExtensionCandidate(Seq(
      Array[Byte](2, 0) -> Array[Byte](1)
    ))
  }

  property("Check interlinks proof should be true") {
    forAll(Gen.nonEmptyListOf(modifierIdGen)) { interlinks =>
      val extension = nipopowAlgos.interlinksToExtension(interlinks)
      val interlinksProof = NipopowAlgos.proofForInterlinkVector(extension).get
      checkInterlinksProof(interlinks, interlinksProof, extension.digest) shouldBe true
    }
  }

  property("Check invalid interlinks proof should be false") {
    forAll(Gen.nonEmptyListOf(modifierIdGen), Gen.nonEmptyListOf(modifierIdGen)) { (interlinks1, interlinks2) =>
        val extension = nipopowAlgos.interlinksToExtension(interlinks2)
        val interlinksProof = NipopowAlgos.proofForInterlinkVector(extension).get
        checkInterlinksProof(interlinks1, interlinksProof, extension.digest) shouldBe false
    }
  }

  property("a mixed-extension interlinks proof is accepted against the complete header root") {
    val interlinks = Seq(deterministicId(1), deterministicId(2))
    val extension = mixedExtension(interlinks)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get

    proof.valid(extension.digest) shouldBe true
    proof.valid(extension.interlinksDigest) shouldBe false
    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = extension.digest), interlinks, proof)
        .checkInterlinksProof() shouldBe true
    }
  }

  property("a one-byte header extension root mutation is rejected") {
    val interlinks = Seq(deterministicId(1), deterministicId(2))
    val extension = mixedExtension(interlinks)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get
    val wrongRootBytes = extension.digest.clone()
    wrongRootBytes(0) = (wrongRootBytes(0) ^ 1).toByte
    val wrongRoot = Digest32 @@ wrongRootBytes

    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = wrongRoot), interlinks, proof)
        .checkInterlinksProof() shouldBe false
    }
  }

  property("an interlink mutation retaining the original full-root proof is rejected") {
    val interlinks = Seq(deterministicId(1), deterministicId(2))
    val extension = mixedExtension(interlinks)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get
    val mutatedInterlinks = interlinks.updated(1, deterministicId(3))

    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = extension.digest), mutatedInterlinks, proof)
        .checkInterlinksProof() shouldBe false
    }
  }

  property("an incomplete interlink disclosure is rejected even when it proves the full root") {
    val interlinks = Seq(deterministicId(1), deterministicId(2))
    val extension = mixedExtension(interlinks)
    val interlinkKeys = NipopowAlgos.packInterlinks(interlinks).map(_._1)
    val incompleteProof = extension.batchProofFor(interlinkKeys.head).get

    incompleteProof.valid(extension.digest) shouldBe true
    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = extension.digest), interlinks, incompleteProof)
        .checkInterlinksProof() shouldBe false
    }
  }

  property("an extra disclosed extension leaf is rejected even when it proves the full root") {
    val interlinks = Seq(deterministicId(1), deterministicId(2))
    val extension = mixedExtension(interlinks)
    val allKeys = extension.fields.map(_._1)
    val overcompleteProof = extension.batchProofFor(allKeys: _*).get

    overcompleteProof.valid(extension.digest) shouldBe true
    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = extension.digest), interlinks, overcompleteProof)
        .checkInterlinksProof() shouldBe false
    }
  }

  property("an interlinks-only proof is rejected under a mixed extension root") {
    val interlinks = Seq(deterministicId(1), deterministicId(2))
    val interlinksOnlyExtension = nipopowAlgos.interlinksToExtension(interlinks)
    val mixed = mixedExtension(interlinks)
    val legacyProof = NipopowAlgos.proofForInterlinkVector(interlinksOnlyExtension).get

    legacyProof.valid(interlinksOnlyExtension.digest) shouldBe true
    legacyProof.valid(mixed.digest) shouldBe false
    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = mixed.digest), interlinks, legacyProof)
        .checkInterlinksProof() shouldBe false
    }
  }

  property("a zero-length source run is rejected after unpacking") {
    val canonicalFields = NipopowAlgos.packInterlinks(Seq(deterministicId(1), deterministicId(2)))
    val zeroLengthValue = canonicalFields.head._2.clone()
    zeroLengthValue(0) = 0
    val malformedFields = (canonicalFields.head._1 -> zeroLengthValue) +: canonicalFields.tail
    val extension = ExtensionCandidate(malformedFields)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get
    val unpacked = NipopowAlgos.unpackInterlinks(malformedFields).get

    unpacked shouldBe Seq(deterministicId(2))
    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = extension.digest), unpacked, proof)
        .checkInterlinksProof() shouldBe false
    }
  }

  property("a displaced source run-start key is rejected after unpacking") {
    val interlinks = Seq(deterministicId(1), deterministicId(2))
    val canonicalFields = NipopowAlgos.packInterlinks(interlinks)
    val displacedKey = canonicalFields(1)._1.clone()
    displacedKey(1) = 42
    val malformedFields = Seq(canonicalFields.head, displacedKey -> canonicalFields(1)._2)
    val extension = ExtensionCandidate(malformedFields)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get
    val unpacked = NipopowAlgos.unpackInterlinks(malformedFields).get

    unpacked shouldBe interlinks
    forAll(defaultHeaderGen) { header =>
      PoPowHeader(header.copy(height = 2, extensionRoot = extension.digest), unpacked, proof)
        .checkInterlinksProof() shouldBe false
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
