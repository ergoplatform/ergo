package org.ergoplatform.modifiers.history

import io.circe.{Decoder, HCursor}
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.history.popow.PoPowHeader
import org.ergoplatform.modifiers.history.popow.PoPowHeader.checkInterlinksProof
import org.ergoplatform.modifiers.history.popow.PoPowHeaderSerializer
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen
import org.scalacheck.Gen
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.hash.Digest32
import scorex.util.serialization.VLQByteBufferWriter
import scorex.util.{ByteArrayBuilder, ModifierId, bytesToId, idToBytes}
import scorex.util.encode.Base16

import java.nio.ByteBuffer
import java.security.MessageDigest
import scala.io.Source

class PoPowHeaderSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private def deterministicId(value: Byte): ModifierId = bytesToId(Array.fill(32)(value))

  private val MaxHeaderBytes = PoPowHeaderSerializer.MaxHeaderBytes
  private val MaxInterlinks = PoPowHeaderSerializer.MaxInterlinks
  private val MaxMerkleProofBytes = PoPowHeaderSerializer.MaxMerkleProofBytes

  private def resourceCursor(resource: String): HCursor = {
    val stream = Option(getClass.getClassLoader.getResourceAsStream(resource))
      .getOrElse(throw new IllegalArgumentException(s"Missing resource: $resource"))
    val source = Source.fromInputStream(stream, "UTF-8")
    val text = try source.mkString finally source.close()
    io.circe.parser.parse(text).fold(error => throw error, value => value.hcursor)
  }

  private def fixtureValue[A: Decoder](fixture: HCursor, field: String): A =
    fixture.get[A](field).fold(error => throw error, value => value)

  private lazy val sampleHeader: PoPowHeader = {
    val fixture = resourceCursor("nipopow-full-root-mixed-popow-header.json")
    val bytes = Base16.decode(fixtureValue[String](fixture, "bytes_hex")).get
    PoPowHeaderSerializer.parseBytes(bytes)
  }

  private def serializeWithNestedPayloads(value: PoPowHeader,
                                          headerPayload: Array[Byte],
                                          proofPayload: Array[Byte]): Array[Byte] = {
    writerBytes { writer =>
      writer.putUInt(headerPayload.length.toLong)
      writer.putBytes(headerPayload)
      writer.putUInt(value.interlinks.length.toLong)
      value.interlinks.foreach(id => writer.putBytes(idToBytes(id)))
      writer.putUInt(proofPayload.length.toLong)
      writer.putBytes(proofPayload)
    }
  }

  private def writerBytes(write: VLQByteBufferWriter => Unit): Array[Byte] = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder)
    write(writer)
    writer.result().toBytes
  }

  private def minimalPoPowHeader(proofPayload: Array[Byte]): Array[Byte] = {
    val headerPayload = sampleHeader.header.bytes
    writerBytes { writer =>
      writer.putUInt(headerPayload.length.toLong)
      writer.putBytes(headerPayload)
      writer.putUInt(0)
      writer.putUInt(proofPayload.length.toLong)
      writer.putBytes(proofPayload)
    }
  }

  private def assertParseFailureContains(bytes: Array[Byte], expected: String): Unit = {
    val failure = PoPowHeaderSerializer.parseBytesTry(bytes).failed.get
    failure.toString should include(expected)
  }

  private def intBytes(value: Int): Array[Byte] = ByteBuffer.allocate(4).putInt(value).array()

  private def merkleProofPayload(indices: Seq[Int], proofCount: Int): Array[Byte] = {
    val serializedIndices: Array[Byte] = indices.iterator
      .flatMap(index => (intBytes(index) ++ Array.fill[Byte](32)(1)).iterator)
      .toArray

    intBytes(indices.size) ++
      intBytes(proofCount) ++
      serializedIndices ++
      Array.fill[Byte](proofCount * 33)(0)
  }

  private def singletonProofPayload(depth: Int): Array[Byte] =
    merkleProofPayload(Seq(0), depth)

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

  property("the cross-runtime full-root fixture round-trips and rejects mutations") {
    val fixture = resourceCursor("nipopow-full-root-mixed-popow-header.json")
    val bytes = Base16.decode(fixtureValue[String](fixture, "bytes_hex")).get

    bytes.length shouldBe fixtureValue[Int](fixture, "length")
    Base16.encode(MessageDigest.getInstance("SHA-256").digest(bytes)) shouldBe
      fixtureValue[String](fixture, "sha256")

    val parsed = PoPowHeaderSerializer.parseBytes(bytes)
    PoPowHeaderSerializer.toBytes(parsed) shouldBe bytes
    Base16.encode(parsed.header.extensionRoot) shouldBe
      fixtureValue[String](fixture, "extension_root")
    parsed.checkInterlinksProof() shouldBe true

    val wrongRootBytes = parsed.header.extensionRoot.clone()
    wrongRootBytes(0) = (wrongRootBytes(0) ^ 1).toByte
    parsed.copy(header = parsed.header.copy(extensionRoot = Digest32 @@ wrongRootBytes))
      .checkInterlinksProof() shouldBe false

    parsed.copy(interlinks = parsed.interlinks.updated(1, deterministicId(3)))
      .checkInterlinksProof() shouldBe false
  }

  property("a nested header payload accepts trailing padding") {
    val headerPayload = sampleHeader.header.bytes :+ 0x7f.toByte
    val proofPayload = PoPowHeaderSerializer.merkleProofSerializer.serialize(sampleHeader.interlinksProof)
    val bytes = serializeWithNestedPayloads(sampleHeader, headerPayload, proofPayload)

    PoPowHeaderSerializer.parseBytes(bytes) shouldBe sampleHeader
  }

  property("a nested Merkle proof payload rejects trailing padding") {
    val headerPayload = sampleHeader.header.bytes
    val proofPayload =
      PoPowHeaderSerializer.merkleProofSerializer.serialize(sampleHeader.interlinksProof) :+ 0x7f.toByte
    val bytes = serializeWithNestedPayloads(sampleHeader, headerPayload, proofPayload)

    assertParseFailureContains(bytes, "Merkle proof counts")
  }

  property("PoPowHeader rejects an oversized nested header before reading its payload") {
    val bytes = writerBytes(_.putUInt(MaxHeaderBytes + 1L))

    assertParseFailureContains(bytes, "header length")
  }

  property("PoPowHeader rejects an oversized interlink count before reading ids") {
    val headerPayload = sampleHeader.header.bytes
    val bytes = writerBytes { writer =>
      writer.putUInt(headerPayload.length.toLong)
      writer.putBytes(headerPayload)
      writer.putUInt(MaxInterlinks + 1L)
    }

    assertParseFailureContains(bytes, "interlink count")
  }

  property("PoPowHeader rejects an oversized Merkle proof before reading its payload") {
    val headerPayload = sampleHeader.header.bytes
    val bytes = writerBytes { writer =>
      writer.putUInt(headerPayload.length.toLong)
      writer.putBytes(headerPayload)
      writer.putUInt(0)
      writer.putUInt(MaxMerkleProofBytes + 1L)
    }

    assertParseFailureContains(bytes, "Merkle proof length")
  }

  property("PoPowHeader rejects an index count that cannot fit its proof payload") {
    val proofPayload = intBytes(1) ++ intBytes(0)

    assertParseFailureContains(minimalPoPowHeader(proofPayload), "Merkle proof counts")
  }

  property("PoPowHeader rejects a proof-node count that cannot fit its proof payload") {
    val proofPayload = intBytes(0) ++ intBytes(1)

    assertParseFailureContains(minimalPoPowHeader(proofPayload), "Merkle proof counts")
  }

  property("PoPowHeader rejects a singleton proof deeper than the extension key space") {
    val impossibleDepth = java.lang.Byte.SIZE * 2 + 1

    assertParseFailureContains(
      minimalPoPowHeader(singletonProofPayload(impossibleDepth)),
      "Merkle proof structure"
    )
  }

  property("PoPowHeader accepts a singleton proof at the extension key-space depth") {
    val maximumDepth = java.lang.Byte.SIZE * 2

    PoPowHeaderSerializer.parseBytes(
      minimalPoPowHeader(singletonProofPayload(maximumDepth))
    ).interlinksProof.proofs.size shouldBe maximumDepth
  }

  property("PoPowHeader rejects a Merkle index outside the extension key space") {
    val firstInvalidIndex = 1 << PoPowHeaderSerializer.MaxMerkleProofDepth

    assertParseFailureContains(
      minimalPoPowHeader(merkleProofPayload(Seq(firstInvalidIndex), 0)),
      "Merkle proof structure"
    )
  }

  property("PoPowHeader rejects duplicate Merkle indices") {
    assertParseFailureContains(
      minimalPoPowHeader(merkleProofPayload(Seq(0, 0), 0)),
      "Merkle proof structure"
    )
  }

  property("PoPowHeader rejects extreme Merkle counts with checked arithmetic") {
    val proofPayload = intBytes(Int.MaxValue) ++ intBytes(Int.MaxValue)

    assertParseFailureContains(minimalPoPowHeader(proofPayload), "Merkle proof counts")
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

  property("Merkle library validation exceptions reject the interlinks proof") {
    val interlinks = Seq(deterministicId(1), deterministicId(2))
    val extension = mixedExtension(interlinks)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get
    proof.proofs should not be empty
    val malformedProof = BatchMerkleProof[Digest32](
      proof.indices,
      (null.asInstanceOf[Digest32] -> proof.proofs.head._2) +: proof.proofs.tail
    )(org.ergoplatform.settings.Algos.hash)

    checkInterlinksProof(interlinks, malformedProof, extension.digest) shouldBe false
  }

  property("missing Merkle proof nodes reject the interlinks proof") {
    val interlinks = Seq(deterministicId(1), deterministicId(2))
    val extension = mixedExtension(interlinks)
    val proof = NipopowAlgos.proofForInterlinkVector(extension).get
    val incompleteProof = BatchMerkleProof[Digest32](
      proof.indices,
      Seq.empty
    )(org.ergoplatform.settings.Algos.hash)

    checkInterlinksProof(interlinks, incompleteProof, extension.digest) shouldBe false
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
