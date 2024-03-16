package org.ergoplatform.serialization

import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.ExtensionSerializer
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.modifiers.mempool.ErgoTransactionSerializer
import org.ergoplatform.nodeView.history.ErgoSyncInfoSerializer
import org.ergoplatform.nodeView.state.ErgoStateContextSerializer
import org.ergoplatform.settings.{ErgoValidationSettings, ErgoValidationSettingsSerializer, ErgoValidationSettingsUpdateSerializer}
import org.ergoplatform.utils.{ErgoCorePropertyTest, SerializationTests}
import org.scalacheck.Gen
import org.scalatest.Assertion

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
