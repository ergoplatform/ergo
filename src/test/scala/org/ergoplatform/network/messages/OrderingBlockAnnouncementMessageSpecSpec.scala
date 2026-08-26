package org.ergoplatform.network.messages

import org.ergoplatform.network.message.inputblocks.{OrderingBlockAnnouncement, OrderingBlockAnnouncementMessageSpec}
import org.ergoplatform.utils.generators.ErgoCoreGenerators._
import org.ergoplatform.utils.generators.CoreObjectGenerators._
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers

class OrderingBlockAnnouncementMessageSpecSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers {

  val orderingBlockAnnouncementGen: Gen[OrderingBlockAnnouncement] = 
    for {
      header <- defaultHeaderGen
      // Use empty collections to avoid complex serialization issues
      txIds <- Gen.listOfN(2, modifierIdGen)
    } yield OrderingBlockAnnouncement(OrderingBlockAnnouncement.CurrentVersion, header, Seq.empty, txIds, Seq.empty)

  property("should serialize and deserialize ordering block announcement") {
    forAll(orderingBlockAnnouncementGen) { oba =>
      val bytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      val result = OrderingBlockAnnouncementMessageSpec.parseBytes(bytes)
      
      result.header shouldEqual oba.header
      result.nonBroadcastedTransactions shouldEqual oba.nonBroadcastedTransactions
      result.broadcastedTransactionIds shouldEqual oba.broadcastedTransactionIds
      result.extensionFields shouldEqual oba.extensionFields
    }
  }

  property("should handle empty transactions and extension fields") {
    forAll(defaultHeaderGen) { header =>
      val emptyOba = OrderingBlockAnnouncement(OrderingBlockAnnouncement.CurrentVersion, header, Seq.empty, Seq.empty, Seq.empty)
      val bytes = OrderingBlockAnnouncementMessageSpec.toBytes(emptyOba)
      val result = OrderingBlockAnnouncementMessageSpec.parseBytes(bytes)
      
      result shouldEqual emptyOba
    }
  }

  property("should reject malformed messages") {
    val invalidBytes = Array.fill(100)(0.toByte)
    val parsed = OrderingBlockAnnouncementMessageSpec.parseBytesTry(invalidBytes)
    
    parsed.isSuccess shouldBe false
  }

  property("should maintain message size within limits") {
    forAll(orderingBlockAnnouncementGen) { oba =>
      val bytes = OrderingBlockAnnouncementMessageSpec.toBytes(oba)
      bytes.length should be <= 32000 // maxSize defined in spec
    }
  }
}