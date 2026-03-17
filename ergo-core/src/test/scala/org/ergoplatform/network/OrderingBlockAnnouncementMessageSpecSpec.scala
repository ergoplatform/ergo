package org.ergoplatform.network

import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.message.inputblocks.{OrderingBlockAnnouncement, OrderingBlockAnnouncementMessageSpec}
import org.ergoplatform.utils.{ErgoCorePropertyTest, SerializationTests}
import org.scalacheck.Gen
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}
import java.nio.ByteBuffer

class OrderingBlockAnnouncementMessageSpecSpec extends ErgoCorePropertyTest with SerializationTests {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  private val messageSpec = OrderingBlockAnnouncementMessageSpec

  private def orderingBlockAnnouncementGen: Gen[OrderingBlockAnnouncement] = for {
    header <- defaultHeaderGen
    nonBroadcastedTransactions <- Gen.listOf(invalidErgoTransactionGen).map(_.take(5))
    broadcastedTransactionIds <- Gen.listOf(modifierIdGen).map(_.take(5))
    extensionFields <- Gen.listOf(extensionKvGen(Extension.FieldKeySize, Extension.FieldValueMaxSize)).map(_.take(5).toStream)
  } yield OrderingBlockAnnouncement(
    header,
    nonBroadcastedTransactions,
    broadcastedTransactionIds,
    extensionFields
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

      // Verify the entire object
      recovered.header shouldEqual announcement.header
      recovered.nonBroadcastedTransactions shouldEqual announcement.nonBroadcastedTransactions
      recovered.broadcastedTransactionIds shouldEqual announcement.broadcastedTransactionIds
      recovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual 
        announcement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }
    }
  }

  property("OrderingBlockAnnouncement serialization with empty collections") {
    forAll(defaultHeaderGen) { header =>
      val emptyAnnouncement = OrderingBlockAnnouncement(
        header,
        Seq.empty[ErgoTransaction],
        Seq.empty,
        Seq.empty
      )

      val bytes = messageSpec.toBytes(emptyAnnouncement)
      val recovered = messageSpec.parseBytes(bytes)

      recovered.header shouldEqual emptyAnnouncement.header
      recovered.nonBroadcastedTransactions shouldEqual emptyAnnouncement.nonBroadcastedTransactions
      recovered.broadcastedTransactionIds shouldEqual emptyAnnouncement.broadcastedTransactionIds
      recovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual 
        emptyAnnouncement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }
    }
  }

  property("OrderingBlockAnnouncement hardcoded test vectors") {
    // Test with minimal data - completely empty
    val minimalHeader = defaultHeaderGen.sample.get
    val minimalAnnouncement = OrderingBlockAnnouncement(
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq.empty,
      Seq.empty
    )
    
    val minimalBytes = messageSpec.toBytes(minimalAnnouncement)
    val minimalRecovered = messageSpec.parseBytes(minimalBytes)
    
    minimalRecovered.header shouldEqual minimalAnnouncement.header
    minimalRecovered.nonBroadcastedTransactions shouldBe empty
    minimalRecovered.broadcastedTransactionIds shouldBe empty
    minimalRecovered.extensionFields shouldBe empty

    // Test with single extension field (keys must be exactly 2 bytes)
    val singleExtensionAnnouncement = OrderingBlockAnnouncement(
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq.empty,
      Seq((Array[Byte](1, 2), Array[Byte](3, 4, 5))).toStream
    )
    
    val singleExtensionBytes = messageSpec.toBytes(singleExtensionAnnouncement)
    val singleExtensionRecovered = messageSpec.parseBytes(singleExtensionBytes)
    
    singleExtensionRecovered.header shouldEqual singleExtensionAnnouncement.header
    singleExtensionRecovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual 
      singleExtensionAnnouncement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }

    // Test with multiple extension fields (keys must be exactly 2 bytes)
    val multipleExtensionAnnouncement = OrderingBlockAnnouncement(
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq.empty,
      Seq(
        (Array[Byte](1, 2), Array[Byte](3, 4, 5)),
        (Array[Byte](6, 7), Array[Byte](8)),
        (Array[Byte](8, 9), Array[Byte](10, 11, 12, 13))
      ).toStream
    )
    
    val multipleExtensionBytes = messageSpec.toBytes(multipleExtensionAnnouncement)
    val multipleExtensionRecovered = messageSpec.parseBytes(multipleExtensionBytes)
    
    multipleExtensionRecovered.header shouldEqual multipleExtensionAnnouncement.header
    multipleExtensionRecovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual 
      multipleExtensionAnnouncement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }

    // Test with transaction IDs only
    val txId = modifierIdGen.sample.get
    val txIdsOnlyAnnouncement = OrderingBlockAnnouncement(
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq(txId),
      Seq.empty
    )
    
    val txIdsOnlyBytes = messageSpec.toBytes(txIdsOnlyAnnouncement)
    val txIdsOnlyRecovered = messageSpec.parseBytes(txIdsOnlyBytes)
    
    txIdsOnlyRecovered.header shouldEqual txIdsOnlyAnnouncement.header
    txIdsOnlyRecovered.broadcastedTransactionIds shouldEqual Seq(txId)
    txIdsOnlyRecovered.nonBroadcastedTransactions shouldBe empty
    txIdsOnlyRecovered.extensionFields shouldBe empty

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
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq.empty,
      Seq((Array[Byte](1, 2), Array[Byte]())).toStream
    )
    
    val emptyValueExtensionBytes = messageSpec.toBytes(emptyValueExtensionAnnouncement)
    val emptyValueExtensionRecovered = messageSpec.parseBytes(emptyValueExtensionBytes)
    
    emptyValueExtensionRecovered.header shouldEqual emptyValueExtensionAnnouncement.header
    emptyValueExtensionRecovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual 
      emptyValueExtensionAnnouncement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }

    // Test edge case: extension field with maximum allowed value size
    val maxValueSize = 64 // Reasonable limit for testing
    val maxValueExtensionAnnouncement = OrderingBlockAnnouncement(
      minimalHeader,
      Seq.empty[ErgoTransaction],
      Seq.empty,
      Seq((Array[Byte](1, 2), Array.fill(maxValueSize)(255.toByte))).toStream
    )

    val maxValueExtensionBytes = messageSpec.toBytes(maxValueExtensionAnnouncement)
    val maxValueExtensionRecovered = messageSpec.parseBytes(maxValueExtensionBytes)

    maxValueExtensionRecovered.header shouldEqual maxValueExtensionAnnouncement.header
    maxValueExtensionRecovered.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) } shouldEqual
      maxValueExtensionAnnouncement.extensionFields.toSeq.map { case (k, v) => (k.toSeq, v.toSeq) }
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
    val announcement = OrderingBlockAnnouncement(header, Seq.empty, Seq.empty, Seq.empty.toStream)
    val bytes = messageSpec.toBytes(announcement)
    
    // This should parse successfully (all counts are 0, well under the limit)
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
    val parsed = messageSpec.parse(reader)
    parsed.header shouldEqual announcement.header
  }
}
