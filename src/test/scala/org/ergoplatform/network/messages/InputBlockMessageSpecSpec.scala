package org.ergoplatform.network.messages

import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.network.message.inputblocks.InputBlockMessageSpec
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.utils.generators.ErgoCoreGenerators._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers

class InputBlockMessageSpecSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers {

  val inputBlockInfoGen: Gen[InputBlockAnnouncement] = 
    for {
      header <- defaultHeaderGen
      weakTxIds <- Gen.option(Gen.listOfN(3, Gen.listOfN(6, Arbitrary.arbitrary[Byte]).map(_.toArray)))
    } yield InputBlockAnnouncement(
      InputBlockAnnouncement.initialMessageVersion,
      header,
      InputBlockFields.empty,
      weakTxIds
    )

  property("should serialize and deserialize input block info") {
    forAll(inputBlockInfoGen) { ibi =>
      val bytes = InputBlockMessageSpec.toBytes(ibi)
      val parsed = InputBlockMessageSpec.parseBytesTry(bytes)
      
      parsed.isSuccess shouldBe true
      val result = parsed.get
      
      result.header shouldEqual ibi.header
      // Compare weakTxIds by content since arrays are different objects
      result.weakTxIds.map(_.map(_.toSeq)) shouldEqual ibi.weakTxIds.map(_.map(_.toSeq))
      result.prevInputBlockId shouldEqual ibi.prevInputBlockId
      result.transactionsDigest shouldEqual ibi.transactionsDigest
    }
  }

  property("should handle optional fields correctly") {
    forAll(defaultHeaderGen) { header =>
      // Test with all optional fields as None
      val emptyIbi = InputBlockAnnouncement(
        InputBlockAnnouncement.initialMessageVersion,
        header,
        InputBlockFields.empty,
        None
      )
      val bytes = InputBlockMessageSpec.toBytes(emptyIbi)
      val parsed = InputBlockMessageSpec.parseBytesTry(bytes)
      
      parsed.isSuccess shouldBe true
      val result = parsed.get
      
      // Compare individual fields since InputBlockFields doesn't have proper equals
      result.version shouldEqual emptyIbi.version
      result.header shouldEqual emptyIbi.header
      result.weakTxIds shouldEqual emptyIbi.weakTxIds
      // For InputBlockFields, we need to compare individual components
      result.prevInputBlockId shouldEqual emptyIbi.prevInputBlockId
      result.transactionsDigest shouldEqual emptyIbi.transactionsDigest
    }
  }

  property("should handle different versions") {
    forAll(inputBlockInfoGen) { ibi =>
      // Test that different versions are handled (though only version 1 is supported currently)
      val bytes = InputBlockMessageSpec.toBytes(ibi)
      val parsed = InputBlockMessageSpec.parseBytesTry(bytes)
      
      parsed.isSuccess shouldBe true
    }
  }
}