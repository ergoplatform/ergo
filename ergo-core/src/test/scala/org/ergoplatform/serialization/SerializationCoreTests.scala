package org.ergoplatform.serialization

import java.nio.ByteBuffer

import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionSerializer}
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.modifiers.mempool.ErgoTransactionSerializer
import org.ergoplatform.nodeView.history.ErgoSyncInfoSerializer
import org.ergoplatform.nodeView.state.ErgoStateContextSerializer
import org.ergoplatform.settings.{Constants, ErgoValidationSettings, ErgoValidationSettingsSerializer, ErgoValidationSettingsUpdateSerializer}
import org.ergoplatform.utils.{ErgoCorePropertyTest, SerializationTests}
import org.scalacheck.Gen
import org.scalatest.Assertion
import scorex.util.ModifierId
import scorex.util.serialization.VLQByteBufferReader

import scala.util.Try

class SerializationCoreTests extends ErgoCorePropertyTest with SerializationTests {
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  def checkSerializationRoundtripAndSize[A <: ErgoNodeViewModifier](generator: Gen[A],
                                                                    serializer: ErgoSerializer[A]): Assertion = {
    forAll(generator) { b: A =>
      val recovered = serializer.parseBytes(serializer.toBytes(b))
      val bytes = serializer.toBytes(b)
      bytes shouldEqual serializer.toBytes(recovered)
    }
  }

  property("Header serialization") {
    val serializer = HeaderSerializer
    forAll(invalidHeaderGen) { b: Header =>
      val recovered = serializer.parseBytes(serializer.toBytes(b))
      recovered shouldBe b
      recovered.size shouldBe serializer.toBytes(b).length
    }
  }

  property("ErgoStateContext serialization") {
    val serializer = ErgoStateContextSerializer(chainSettings)
    val b = ergoStateContextGen.sample.get
    val recovered = serializer.parseBytes(serializer.toBytes(b))
    serializer.toBytes(b) shouldEqual serializer.toBytes(recovered)
    b.lastHeaders.length shouldBe recovered.lastHeaders.length
    b.lastHeaders shouldBe recovered.lastHeaders
  }

  property("Extension serialization") {
    checkSerializationRoundtrip(extensionGen, ExtensionSerializer)
  }

  property("Extension parsing eagerly consumes every declared field") {
    val extension = Extension(
      ModifierId @@ ("00" * 32),
      Seq(
        Array[Byte](0, 1) -> Array.emptyByteArray,
        Array[Byte](0, 2) -> Array[Byte](1, 2),
        Array[Byte](0, 3) -> Array.fill[Byte](Extension.FieldValueMaxSize)(3)
      )
    )
    val bytes = ExtensionSerializer.toBytes(extension)
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
    val parsed = ExtensionSerializer.parse(reader)

    reader.remaining shouldBe 0
    parsed.size shouldBe bytes.length
    parsed.fields.size shouldBe extension.fields.size
    parsed.fields.zip(extension.fields).foreach { case ((parsedKey, parsedValue), (expectedKey, expectedValue)) =>
      parsedKey.sameElements(expectedKey) shouldBe true
      parsedValue.sameElements(expectedValue) shouldBe true
    }
    ExtensionSerializer.toBytes(parsed).sameElements(bytes) shouldBe true
    Try(ExtensionSerializer.parseBytes(bytes.dropRight(1))).isFailure shouldBe true

    val suffix = Array[Byte](0x55, 0x66)
    val readerWithSuffix = new VLQByteBufferReader(ByteBuffer.wrap(bytes ++ suffix))
    val parsedWithSuffix = ExtensionSerializer.parse(readerWithSuffix)
    readerWithSuffix.remaining shouldBe suffix.length
    ExtensionSerializer.toBytes(parsedWithSuffix).sameElements(bytes) shouldBe true
  }

  property("Extension parsing enforces the defensive maximum size") {
    val fieldCount = 15650
    val lastIndex = fieldCount - 1
    val prefixFields = (0 until lastIndex).map { i =>
      Array[Byte]((i >>> 8).toByte, i.toByte) ->
        Array.fill[Byte](Extension.FieldValueMaxSize)(i.toByte)
    }

    def serialized(lastValueLength: Int): Array[Byte] = {
      val lastField =
        Array[Byte]((lastIndex >>> 8).toByte, lastIndex.toByte) ->
          Array.fill[Byte](lastValueLength)(lastIndex.toByte)
      ExtensionSerializer.toBytes(
        Extension(ModifierId @@ ("00" * 32), prefixFields :+ lastField)
      )
    }

    val belowLimit = serialized(55)
    belowLimit.length shouldBe Constants.MaxExtensionSizeMax - 1
    Try(ExtensionSerializer.parseBytes(belowLimit)).isSuccess shouldBe true

    val atLimit = serialized(56)
    atLimit.length shouldBe Constants.MaxExtensionSizeMax
    Try(ExtensionSerializer.parseBytes(atLimit)).isFailure shouldBe true

    val aboveLimit = serialized(57)
    aboveLimit.length shouldBe Constants.MaxExtensionSizeMax + 1
    Try(ExtensionSerializer.parseBytes(aboveLimit)).isFailure shouldBe true
  }

  property("ErgoTransactionGen serialization") {
    checkSerializationRoundtripAndSize(invalidErgoTransactionGen, ErgoTransactionSerializer)
  }

  property("ErgoTransaction .bytes") {
    forAll(invalidErgoTransactionGen) { tx =>
      val bytes = tx.bytes
      val txRestored = ErgoTransactionSerializer.parseBytes(bytes)
      txRestored.bytes.sameElements(bytes) shouldBe true
    }
  }

  property("ErgoSyncInfo v1 serialization") {
    checkSerializationRoundtrip(ergoSyncInfoV1Gen, ErgoSyncInfoSerializer)
  }

  property("ErgoSyncInfo v2 serialization") {
    checkSerializationRoundtrip(ergoSyncInfoV2Gen, ErgoSyncInfoSerializer)
  }

  property("ErgoHeader serialization") {
    checkSerializationRoundtripAndSize(defaultHeaderGen, HeaderSerializer)
  }

  property("BlockTransactions serialization") {
    checkSerializationRoundtripAndSize(invalidBlockTransactionsGen, BlockTransactionsSerializer)
  }

  property("ADProofs serialization") {
    checkSerializationRoundtripAndSize(randomADProofsGen, ADProofsSerializer)
  }

  property("ModeFeature serialization") {
    forAll(modeFeatureGen) { mf =>
      mf.serializer.parseBytes(mf.serializer.toBytes(mf)) shouldEqual mf
    }
  }

  property("ErgoValidationSettings serialization") {
    val serializer = ErgoValidationSettingsSerializer
    forAll(ergoValidationSettingsGen) { vs =>
      // to bytes / from bytes
      serializer.parseBytes(serializer.toBytes(vs)) shouldEqual vs
      // to extension / from extension
      ErgoValidationSettings.parseExtension(vs.toExtensionCandidate).get shouldEqual vs
    }
  }

  property("ErgoValidationSettingsUpdate serialization") {
    val serializer = ErgoValidationSettingsUpdateSerializer
    forAll(ergoValidationSettingsUpdateGen) { vs =>
      serializer.parseBytes(serializer.toBytes(vs)) shouldEqual vs
    }
  }

}
