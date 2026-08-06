package org.ergoplatform.modifiers.history

import io.circe.Decoder
import io.circe.HCursor
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.history.popow.NipopowProof
import org.ergoplatform.modifiers.history.popow.NipopowProofSerializer
import org.ergoplatform.modifiers.history.popow.PoPowHeader
import org.ergoplatform.modifiers.history.popow.PoPowHeaderSerializer
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.ModifierId
import scorex.util.bytesToId
import scorex.util.encode.Base16

import java.security.MessageDigest
import scala.io.Source
import scala.util.Try

class NipopowProofInteropSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private case class ByteRange(start: Int, endExclusive: Int)

  private val FixtureResource = "nipopow-full-root-mixed-nipopow-proof.json"
  private val HeaderFixtureResource = "nipopow-full-root-mixed-popow-header.json"
  private val FixtureFormat = "scorex-nipopow-proof-with-jvm-mode-v1"

  private val serializer = new NipopowProofSerializer(nipopowAlgos)

  private def resourceCursor(resource: String): HCursor = {
    val stream = Option(getClass.getClassLoader.getResourceAsStream(resource))
      .getOrElse(throw new IllegalArgumentException(s"Missing resource: $resource"))
    val source = Source.fromInputStream(stream, "UTF-8")
    val text = try source.mkString finally source.close()
    io.circe.parser.parse(text).fold(error => throw error, _.hcursor)
  }

  private lazy val fixture = resourceCursor(FixtureResource)

  private def fixtureValue[A: Decoder](field: String): A =
    fixture.get[A](field).fold(error => throw error, identity)

  private lazy val fixtureBytes: Array[Byte] =
    Base16.decode(fixtureValue[String]("bytes_hex")).get

  private lazy val rustCoreLength: Int = fixtureValue[Int]("rust_core_length")

  private lazy val terminalMode: Int = fixtureValue[Int]("terminal_continuous_byte")

  private def deterministicId(value: Byte): ModifierId = bytesToId(Array.fill(32)(value))

  private def deterministicProof(): NipopowProof = {
    val mixedFixture = resourceCursor(HeaderFixtureResource)
    val mixedBytes = Base16.decode(
      mixedFixture.get[String]("bytes_hex").fold(error => throw error, identity)).get
    val mixedHeader = PoPowHeaderSerializer.parseBytes(mixedBytes)

    val emptyExtension = nipopowAlgos.interlinksToExtension(Seq.empty)
    val emptyProof = NipopowAlgos.proofForInterlinkVector(emptyExtension).get
    val genesisHeader: Header = mixedHeader.header.copy(
      parentId = deterministicId(0),
      height = 1,
      extensionRoot = emptyExtension.digest,
      sizeOpt = None
    )
    val genesis = PoPowHeader(genesisHeader, Seq.empty, emptyProof)
    val suffixHead = mixedHeader.copy(header = mixedHeader.header.copy(
      parentId = genesis.id,
      height = 2,
      sizeOpt = None
    ))
    val suffixTail = mixedHeader.header.copy(
      parentId = suffixHead.id,
      height = 3,
      sizeOpt = None
    )

    NipopowProof(
      nipopowAlgos,
      m = 1,
      k = 2,
      prefix = Seq(genesis),
      suffixHead = suffixHead,
      suffixTail = Seq(suffixTail),
      continuous = false
    )
  }

  /** Validate the fixture envelope without changing generic parser semantics. */
  private def parseFixtureEnvelope(bytes: Array[Byte],
                                   coreLength: Int,
                                   expectedTerminalMode: Int): Try[NipopowProof] = Try {
    require(expectedTerminalMode == 0 || expectedTerminalMode == 1,
      s"invalid JVM terminal mode $expectedTerminalMode")
    require(bytes.length == coreLength + 1,
      s"expected one terminal byte after $coreLength core bytes, got ${bytes.length}")
    val actualTerminalMode = bytes(coreLength) & 0xff
    require(actualTerminalMode == expectedTerminalMode,
      s"JVM terminal mode $actualTerminalMode does not match $expectedTerminalMode")

    val parsed = serializer.parseBytes(bytes)
    require(parsed.continuous == (expectedTerminalMode == 1),
      "parsed JVM terminal mode differs from the fixture envelope")
    require(serializer.toBytes(parsed).sameElements(bytes),
      "JVM proof does not reserialize to the exact fixture envelope")
    require(parsed.isValid, "parsed NiPoPoW proof is invalid")
    parsed
  }

  private def readFixtureVlq(bytes: Array[Byte], initialOffset: Int): (Long, Int) = {
    var value = 0L
    var offset = initialOffset
    var shift = 0
    while (shift < 35) {
      require(offset < bytes.length, "truncated fixture VLQ")
      val next = bytes(offset) & 0xff
      offset += 1
      value |= (next & 0x7f).toLong << shift
      if ((next & 0x80) == 0) return value -> offset
      shift += 7
    }
    throw new IllegalArgumentException("fixture VLQ exceeds u32")
  }

  private def suffixHeadRange(bytes: Array[Byte]): ByteRange = {
    var offset = 0
    offset = readFixtureVlq(bytes, offset)._2
    offset = readFixtureVlq(bytes, offset)._2
    val (prefixCount, afterPrefixCount) = readFixtureVlq(bytes, offset)
    offset = afterPrefixCount
    (0L until prefixCount).foreach { _ =>
      val (frameLength, afterFrameLength) = readFixtureVlq(bytes, offset)
      offset = afterFrameLength
      require(frameLength <= bytes.length - offset, "prefix frame exceeds fixture bytes")
      offset += frameLength.toInt
    }
    val (suffixHeadLength, suffixHeadStart) = readFixtureVlq(bytes, offset)
    require(suffixHeadLength <= bytes.length - suffixHeadStart,
      "suffix-head frame exceeds fixture bytes")
    ByteRange(suffixHeadStart, suffixHeadStart + suffixHeadLength.toInt)
  }

  private def singleSubsliceOffset(bytes: Array[Byte],
                                   range: ByteRange,
                                   needle: Array[Byte]): Int = {
    require(needle.nonEmpty, "fixture mutation target cannot be empty")
    require(needle.length <= range.endExclusive - range.start,
      "fixture mutation target exceeds its search range")
    val offsets = (range.start to range.endExclusive - needle.length).filter { offset =>
      bytes.slice(offset, offset + needle.length).sameElements(needle)
    }
    require(offsets.length == 1, "fixture mutation target must be unique")
    offsets.head
  }

  property("the JVM producer reproduces the complete frozen NiPoPoW fixture") {
    val produced = serializer.toBytes(deterministicProof())

    produced shouldBe fixtureBytes
    Base16.encode(MessageDigest.getInstance("SHA-256").digest(produced)) shouldBe
      fixtureValue[String]("sha256")
  }

  property("the complete fixture round-trips at the explicit Rust core boundary") {
    fixtureValue[String]("format") shouldBe FixtureFormat
    fixtureBytes.length shouldBe rustCoreLength + 1
    (fixtureBytes(rustCoreLength) & 0xff) shouldBe terminalMode

    val parsed = parseFixtureEnvelope(fixtureBytes, rustCoreLength, terminalMode).get
    parsed.m shouldBe fixtureValue[Int]("m")
    parsed.k shouldBe fixtureValue[Int]("k")
    parsed.prefix.size shouldBe fixtureValue[Int]("prefix_count")
    parsed.suffixHeaders.size shouldBe fixtureValue[Int]("suffix_count")
    parsed.suffixTail.size shouldBe fixtureValue[Int]("suffix_tail_count")
    Base16.encode(parsed.suffixHead.header.extensionRoot) shouldBe
      fixtureValue[String]("extension_root")
    parsed.suffixHead.interlinks shouldBe
      Seq(deterministicId(0x11), deterministicId(0x22))
    parsed.prefix.head.checkInterlinksProof() shouldBe true
    parsed.suffixHead.checkInterlinksProof() shouldBe true
    parsed.hasValidParams shouldBe true
    parsed.isValid shouldBe true
  }

  property("the complete fixture rejects an extension-root mutation") {
    val mutated = fixtureBytes.clone()
    val extensionRoot = Base16.decode(fixtureValue[String]("extension_root")).get
    val rootOffset =
      singleSubsliceOffset(mutated, suffixHeadRange(mutated), extensionRoot)
    mutated(rootOffset) = (mutated(rootOffset) ^ 1).toByte

    parseFixtureEnvelope(mutated, rustCoreLength, terminalMode).isFailure shouldBe true
  }

  property("the complete fixture rejects a disclosed-interlink mutation") {
    val mutated = fixtureBytes.clone()
    val interlink = Array.fill(32)(0x22.toByte)
    val interlinkOffset =
      singleSubsliceOffset(mutated, suffixHeadRange(mutated), interlink)
    mutated(interlinkOffset) = (mutated(interlinkOffset) ^ 1).toByte

    parseFixtureEnvelope(mutated, rustCoreLength, terminalMode).isFailure shouldBe true
  }

  property("the complete fixture rejects an m mutation") {
    val mutated = fixtureBytes.clone()
    mutated(0) shouldBe 1.toByte
    mutated(0) = 0

    parseFixtureEnvelope(mutated, rustCoreLength, terminalMode).isFailure shouldBe true
  }

  property("the complete fixture rejects a k mutation") {
    val mutated = fixtureBytes.clone()
    mutated.take(2) shouldBe Array[Byte](1, 2)
    mutated(1) = 1

    parseFixtureEnvelope(mutated, rustCoreLength, terminalMode).isFailure shouldBe true
  }

  property("the complete fixture rejects a nested header-frame mutation") {
    val mutated = fixtureBytes.clone()
    val nestedSizeOffset = suffixHeadRange(mutated).start
    readFixtureVlq(mutated, nestedSizeOffset)._1 shouldBe 218L
    mutated(nestedSizeOffset) shouldBe 0xda.toByte
    mutated(nestedSizeOffset) = 0xd9.toByte

    parseFixtureEnvelope(mutated, rustCoreLength, terminalMode).isFailure shouldBe true
  }

  property("the complete fixture rejects a missing terminal byte") {
    parseFixtureEnvelope(fixtureBytes.dropRight(1), rustCoreLength, terminalMode)
      .isFailure shouldBe true
  }

  property("the complete fixture rejects an extra terminal byte") {
    parseFixtureEnvelope(
      fixtureBytes :+ terminalMode.toByte, rustCoreLength, terminalMode)
      .isFailure shouldBe true
  }

  property("the complete fixture rejects terminal-mode mutations") {
    val mutated = fixtureBytes.clone()
    mutated(mutated.length - 1) = 1
    parseFixtureEnvelope(mutated, rustCoreLength, terminalMode).isFailure shouldBe true

    mutated(mutated.length - 1) = 2
    parseFixtureEnvelope(mutated, rustCoreLength, 2).isFailure shouldBe true
  }
}
