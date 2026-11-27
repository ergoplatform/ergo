package org.ergoplatform.settings

import com.typesafe.config.{Config, ConfigFactory}
import org.ergoplatform.utils.ErgoCorePropertyTest

class VotingTargetsSpec extends ErgoCorePropertyTest {

  private def config(ids: Seq[Int]): Config = ConfigFactory.parseString(
    s"""ergo.voting {
       |  1 = 1000000
       |  rulesToDisable = [${ids.mkString(",")}]
       |}
       |""".stripMargin)

  property("rulesToDisable preserves numeric boundaries, order and duplicates") {
    val targets = VotingTargets.fromConfig(config(Seq(0, 215, 409, 32767, 215)))
    val expected = Seq[Short](0, 215, 409, Short.MaxValue, 215)

    targets.desiredUpdate.rulesToDisable shouldBe expected
    targets.targets shouldBe Map(Parameters.StorageFeeFactorIncrease -> 1000000)
    val bytes = ErgoValidationSettingsUpdateSerializer.toBytes(targets.desiredUpdate)
    val restored = ErgoValidationSettingsUpdateSerializer.parseBytes(bytes)

    restored.rulesToDisable shouldBe expected
    restored.statusUpdates shouldBe empty
  }

  property("rulesToDisable accepts an empty list") {
    VotingTargets.fromConfig(config(Seq.empty)).desiredUpdate shouldBe
      ErgoValidationSettingsUpdate.empty
  }

  Seq(-1, 32768, 65535, 65536, Int.MinValue, Int.MaxValue).foreach { id =>
    property(s"rulesToDisable rejects an ID outside its serializable range: $id") {
      val error = intercept[IllegalArgumentException] {
        VotingTargets.fromConfig(config(Seq(id)))
      }

      error.getMessage should include("ergo.voting.rulesToDisable")
      error.getMessage should include("0..32767")
      error.getMessage should include(id.toString)
    }
  }

  property("rulesToDisable reports all invalid IDs in a mixed list") {
    val error = intercept[IllegalArgumentException] {
      VotingTargets.fromConfig(config(Seq(215, 32768, -1, 32768)))
    }

    error.getMessage should include("invalid ID(s): -1, 32768")
  }
}
