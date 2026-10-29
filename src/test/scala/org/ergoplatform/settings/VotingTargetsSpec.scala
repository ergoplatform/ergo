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
    ErgoValidationSettingsUpdateSerializer.toBytes(targets.desiredUpdate).nonEmpty shouldBe true
  }

  property("rulesToDisable retains existing known-rule serialization roundtrips") {
    val update = VotingTargets.fromConfig(config(Seq(215, 409, 215))).desiredUpdate
    val bytes = ErgoValidationSettingsUpdateSerializer.toBytes(update)
    val restored = ErgoValidationSettingsUpdateSerializer.parseBytes(bytes)

    restored.rulesToDisable shouldBe Seq[Short](215, 409, 215)
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
    }
  }
}
