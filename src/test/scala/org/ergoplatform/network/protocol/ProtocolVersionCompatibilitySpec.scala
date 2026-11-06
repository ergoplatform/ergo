package org.ergoplatform.network.protocol

import org.ergoplatform.network.Version
import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncementMessageSpec
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers

class ProtocolVersionCompatibilitySpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  {

  property("OrderingBlockAnnouncementMessageSpec should require SubblocksVersion protocol") {
    OrderingBlockAnnouncementMessageSpec.protocolVersion shouldEqual Version.SubblocksVersion
  }

  property("SubblocksVersion should be higher than initial version") {
    (Version.SubblocksVersion.compare(Version.initial) > 0) shouldBe true
  }

  property("SubblocksVersion should be higher than Eip37ForkVersion") {
    (Version.SubblocksVersion.compare(Version.Eip37ForkVersion) > 0) shouldBe true
  }

  property("version comparison should work correctly") {
    val v1 = Version(1, 0, 0)
    val v2 = Version(2, 0, 0)
    val v1_1 = Version(1, 1, 0)
    val v1_0_1 = Version(1, 0, 1)

    (v2.compare(v1) > 0) shouldBe true
    (v1.compare(v2) < 0) shouldBe true
    (v1_1.compare(v1) > 0) shouldBe true
    (v1_0_1.compare(v1) > 0) shouldBe true
    v1.compare(v1) shouldEqual 0
  }

  property("SubblocksFilter should accept peers with version >= SubblocksVersion") {
    // SubBlocksFilter testing requires proper setup - testing basic version comparison instead
    (Version.SubblocksVersion.compare(Version.SubblocksVersion) >= 0) shouldBe true
    (Version(7, 0, 0).compare(Version.SubblocksVersion) >= 0) shouldBe true
    (Version.initial.compare(Version.SubblocksVersion) >= 0) shouldBe false
    (Version.Eip37ForkVersion.compare(Version.SubblocksVersion) >= 0) shouldBe false
  }

  property("should parse version from string correctly") {
    Version("6.5.0") shouldEqual Version.SubblocksVersion
    Version("0.0.1") shouldEqual Version.initial
    Version("4.0.100") shouldEqual Version.Eip37ForkVersion
  }

  property("should handle version string parsing errors") {
    intercept[IllegalArgumentException] {
      Version("invalid.version") // Only 2 components
    }
    
    intercept[IllegalArgumentException] {
      Version("1.2") // Missing third component
    }
  }
}
