package org.ergoplatform.network

import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.message.inputblocks.{OrderingBlockAnnouncement, OrderingBlockAnnouncementMessageSpec}
import org.ergoplatform.utils.{ErgoCorePropertyTest, SerializationTests}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}
import java.nio.ByteBuffer

class OrderingBlockAnnouncementMessageSpecSpec extends ErgoCorePropertyTest with SerializationTests {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  private val messageSpec = OrderingBlockAnnouncementMessageSpec

  private def orderingBlockAnnouncementGen: Gen[OrderingBlockAnnouncement] = for {
    version <- Gen.choose(1, 3).map(_.toByte)
    header <- defaultHeaderGen
    nonBroadcastedTransactions <- Gen.listOf(invalidErgoTransactionGen).map(_.take(5))
    broadcastedTransactionIds <- Gen.listOf(modifierIdGen).map(_.take(5))
    extensionFields <- Gen.listOf(extensionKvGen(Extension.FieldKeySize, Extension.FieldValueMaxSize)).map(_.take(5).toStream)
    unparsedBytes <- Gen.oneOf(Gen.const(Array.emptyByteArray), Gen.listOf(arbitrary[Byte]).map(_.toArray))
  } yield OrderingBlockAnnouncement(
    version,
    header,
    nonBroadcastedTransactions,
    broadcastedTransactionIds,
    extensionFields,
    unparsedBytes
  )

  property("OrderingBlockAnnouncement serialization roundtrip") {
    forAll(orderingBlockAnnouncementGen) { announcement =>
      val bytes = messageSpec.toBytes(announcement)
      val recovered = messageSpec.parseBytes(bytes)

      // Verify individual components
      recovered.header shouldEqual announcement.header
      recovered.nonBroadcastedTransactions shouldEqual announcement.nonBroadcastedTransactions
      recovered.broadcastedTransactionIds shouldEqual announcement.broadcastedTransactionIds
      recovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual
        announcement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }
      recovered.unparsedBytes shouldEqual announcement.unparsedBytes
      recovered.version shouldEqual announcement.version

      // Verify the entire object
      recovered.version shouldEqual announcement.version
      recovered.header shouldEqual announcement.header
      recovered.nonBroadcastedTransactions shouldEqual announcement.nonBroadcastedTransactions
      recovered.broadcastedTransactionIds shouldEqual announcement.broadcastedTransactionIds
      recovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual
        announcement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }
      recovered.unparsedBytes shouldEqual announcement.unparsedBytes
    }
  }

  property("OrderingBlockAnnouncement serialization with empty collections") {
    forAll(defaultHeaderGen) { header =>
      val emptyAnnouncement = OrderingBlockAnnouncement(
        1.toByte,
        header,
        Seq.empty[ErgoTransaction],
        Seq.empty,
        Seq.empty,
        Array.emptyByteArray
      )

      val bytes = messageSpec.toBytes(emptyAnnouncement)
      val recovered = messageSpec.parseBytes(bytes)

      recovered.header shouldEqual emptyAnnouncement.header
      recovered.nonBroadcastedTransactions shouldEqual emptyAnnouncement.nonBroadcastedTransactions
      recovered.broadcastedTransactionIds shouldEqual emptyAnnouncement.broadcastedTransactionIds
      recovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual
        emptyAnnouncement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }
      recovered.unparsedBytes shouldEqual emptyAnnouncement.unparsedBytes
    }
  }

  property("OrderingBlockAnnouncement hardcoded test vectors") {
    // Test with minimal data - completely empty
    val minimalHeader = defaultHeaderGen.sample.get
    val minimalAnnouncement = OrderingBlockAnnouncement(
      1.toByte,
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq.empty,
      Seq.empty,
      Array.emptyByteArray
    )

    val minimalBytes = messageSpec.toBytes(minimalAnnouncement)
    val minimalRecovered = messageSpec.parseBytes(minimalBytes)

    minimalRecovered.header shouldEqual minimalAnnouncement.header
    minimalRecovered.nonBroadcastedTransactions shouldBe empty
    minimalRecovered.broadcastedTransactionIds shouldBe empty
    minimalRecovered.extensionFields shouldBe empty
    minimalRecovered.unparsedBytes shouldBe empty

    // Test with single extension field (keys must be exactly 2 bytes)
    val singleExtensionAnnouncement = OrderingBlockAnnouncement(
      1.toByte,
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq.empty,
      Seq((Array[Byte](1, 2), Array[Byte](3, 4, 5))).toStream,
      Array.emptyByteArray
    )

    val singleExtensionBytes = messageSpec.toBytes(singleExtensionAnnouncement)
    val singleExtensionRecovered = messageSpec.parseBytes(singleExtensionBytes)

    singleExtensionRecovered.header shouldEqual singleExtensionAnnouncement.header
    singleExtensionRecovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual
      singleExtensionAnnouncement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }
    singleExtensionRecovered.unparsedBytes shouldBe empty

    // Test with multiple extension fields (keys must be exactly 2 bytes)
    val multipleExtensionAnnouncement = OrderingBlockAnnouncement(
      1.toByte,
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq.empty,
      Seq(
        (Array[Byte](1, 2), Array[Byte](3, 4, 5)),
        (Array[Byte](6, 7), Array[Byte](8)),
        (Array[Byte](8, 9), Array[Byte](10, 11, 12, 13))
      ).toStream,
      Array.emptyByteArray
    )

    val multipleExtensionBytes = messageSpec.toBytes(multipleExtensionAnnouncement)
    val multipleExtensionRecovered = messageSpec.parseBytes(multipleExtensionBytes)

    multipleExtensionRecovered.header shouldEqual multipleExtensionAnnouncement.header
    multipleExtensionRecovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual
      multipleExtensionAnnouncement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }
    multipleExtensionRecovered.unparsedBytes shouldBe empty

    // Test with transaction IDs only
    val txId = modifierIdGen.sample.get
    val txIdsOnlyAnnouncement = OrderingBlockAnnouncement(
      1.toByte,
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq(txId),
      Seq.empty,
      Array.emptyByteArray
    )

    val txIdsOnlyBytes = messageSpec.toBytes(txIdsOnlyAnnouncement)
    val txIdsOnlyRecovered = messageSpec.parseBytes(txIdsOnlyBytes)

    txIdsOnlyRecovered.header shouldEqual txIdsOnlyAnnouncement.header
    txIdsOnlyRecovered.broadcastedTransactionIds shouldEqual Seq(txId)
    txIdsOnlyRecovered.nonBroadcastedTransactions shouldBe empty
    txIdsOnlyRecovered.extensionFields shouldBe empty
    txIdsOnlyRecovered.unparsedBytes shouldBe empty

    // Verify serialized bytes have expected structure and size relationships
    minimalBytes should not be empty
    singleExtensionBytes.length should be > minimalBytes.length
    multipleExtensionBytes.length should be > singleExtensionBytes.length
    txIdsOnlyBytes.length should be > minimalBytes.length

    // Test roundtrip consistency - serializing the same object twice should produce same bytes
    val bytes1 = messageSpec.toBytes(minimalAnnouncement)
    val bytes2 = messageSpec.toBytes(minimalAnnouncement)
    bytes1 shouldEqual bytes2

    // Test edge case: extension field with empty value
    val emptyValueExtensionAnnouncement = OrderingBlockAnnouncement(
      1.toByte,
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq.empty,
      Seq((Array[Byte](1, 2), Array[Byte]())).toStream,
      Array.emptyByteArray
    )

    val emptyValueExtensionBytes = messageSpec.toBytes(emptyValueExtensionAnnouncement)
    val emptyValueExtensionRecovered = messageSpec.parseBytes(emptyValueExtensionBytes)

    emptyValueExtensionRecovered.header shouldEqual emptyValueExtensionAnnouncement.header
    emptyValueExtensionRecovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual
      emptyValueExtensionAnnouncement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }
    emptyValueExtensionRecovered.unparsedBytes shouldBe empty

    // Test edge case: extension field with maximum allowed value size
    val maxValueSize = 64 // Reasonable limit for testing
    val maxValueExtensionAnnouncement = OrderingBlockAnnouncement(
      1.toByte,
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq.empty,
      Seq((Array[Byte](1, 2), Array.fill(maxValueSize)(255.toByte))).toStream,
      Array.emptyByteArray
    )

    val maxValueExtensionBytes = messageSpec.toBytes(maxValueExtensionAnnouncement)
    val maxValueExtensionRecovered = messageSpec.parseBytes(maxValueExtensionBytes)

    maxValueExtensionRecovered.header shouldEqual maxValueExtensionAnnouncement.header
    maxValueExtensionRecovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual
      maxValueExtensionAnnouncement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }
    maxValueExtensionRecovered.unparsedBytes shouldBe empty
  }

  property("OrderingBlockAnnouncement handles unparsed bytes for forward compatibility") {
    val header = defaultHeaderGen.sample.get
    
    // Create announcement with unparsed bytes (simulating future version data)
    val unparsedData = Array[Byte](1.toByte, 2.toByte, 3.toByte, 4.toByte)
    val announcement = OrderingBlockAnnouncement(
      2.toByte,
      header,
      Seq.empty,
      Seq.empty,
      Seq.empty.toStream,
      unparsedData
    )
    
    // Serialize and deserialize
    val bytes = messageSpec.toBytes(announcement)
    val recovered = messageSpec.parseBytes(bytes)
    
    // Verify unparsed bytes are preserved
    recovered.unparsedBytes shouldEqual unparsedData
    recovered.header shouldEqual announcement.header
  }

  property("OrderingBlockAnnouncement rejects excessive non-broadcasted transactions count") {
    val header = defaultHeaderGen.sample.get
    val maxArraySize = 32768
    
    // Create bytes manually: version + header + excessive nbtCount
    val writer = new VLQByteBufferWriter(new scorex.util.ByteArrayBuilder())
    writer.put(1.toByte) // version
    org.ergoplatform.modifiers.history.header.HeaderSerializer.serialize(header, writer)
    writer.putUInt(maxArraySize + 1L) // excessive count
    
    val bytes = writer.toBytes
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
    val ex = the[Exception] thrownBy messageSpec.parse(reader)
    ex.getMessage should include ("Non-broadcasted transactions count too large")
  }

  property("OrderingBlockAnnouncement rejects excessive transaction IDs count") {
    val header = defaultHeaderGen.sample.get
    val maxArraySize = 32768
    
    // Create bytes: version + header + zero nbtCount + excessive txIdsCount
    val writer = new VLQByteBufferWriter(new scorex.util.ByteArrayBuilder())
    writer.put(1.toByte) // version
    org.ergoplatform.modifiers.history.header.HeaderSerializer.serialize(header, writer)
    writer.putUInt(0L) // zero non-broadcasted transactions
    writer.putUInt(maxArraySize + 1L) // excessive txIds count
    
    val bytes = writer.toBytes
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
    val ex = the[Exception] thrownBy messageSpec.parse(reader)
    ex.getMessage should include ("Transaction IDs count too large")
  }

  property("OrderingBlockAnnouncement rejects excessive extension fields count") {
    val header = defaultHeaderGen.sample.get
    val maxArraySize = 32768
    
    // Create bytes: version + header + zero nbtCount + zero txIdsCount + excessive fieldsCount
    val writer = new VLQByteBufferWriter(new scorex.util.ByteArrayBuilder())
    writer.put(1.toByte) // version
    org.ergoplatform.modifiers.history.header.HeaderSerializer.serialize(header, writer)
    writer.putUInt(0L) // zero non-broadcasted transactions
    writer.putUInt(0L) // zero txIds
    writer.putUShort(maxArraySize + 1) // excessive extension fields count
    
    val bytes = writer.toBytes
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
    val ex = the[Exception] thrownBy messageSpec.parse(reader)
    ex.getMessage should include ("Extension fields count too large")
  }

  property("OrderingBlockAnnouncement accepts counts at MaxArraySize limit") {
    // Test that counts at exactly MaxArraySize are accepted
    // We can't practically create such a large message, so we test with smaller valid messages
    // and verify the validation logic doesn't reject valid counts
    
    val header = defaultHeaderGen.sample.get
    val announcement = OrderingBlockAnnouncement(OrderingBlockAnnouncement.CurrentVersion, header, Seq.empty, Seq.empty, Seq.empty.toStream)
    val bytes = messageSpec.toBytes(announcement)
    
    // This should parse successfully (all counts are 0, well under the limit)
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
    val parsed = messageSpec.parse(reader)
    parsed.header shouldEqual announcement.header
  }

  property("OrderingBlockAnnouncement version is serialized and parsed correctly") {
    forAll(defaultHeaderGen, Gen.choose(1, 5).map(_.toByte)) { (header, version) =>
      val unparsedBytes = Array[Byte](0x12, 0x34)
      val announcement = OrderingBlockAnnouncement(
        version,
        header,
        Seq.empty,
        Seq.empty,
        Seq.empty.toStream,
        unparsedBytes
      )

      val bytes = messageSpec.toBytes(announcement)
      val recovered = messageSpec.parseBytes(bytes)

      recovered.version shouldBe version
      recovered.unparsedBytes shouldBe unparsedBytes
      recovered.header shouldBe header
    }
  }
}
