package org.ergoplatform.settings

import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.crypto.authds.ADDigest

class ParameterValidation60Specification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private val votingEpochLength = 2

  implicit val votingSettings: VotingSettings =
    VotingSettings(
      votingEpochLength,
      softForkEpochs = 2,
      activationEpochs = 3,
      version2ActivationHeight = 100,
      version2ActivationDifficultyHex = "01"
    )

  private val updSettings = chainSettings.copy(voting = votingSettings)

  property("matchParameters60 should allow larger parameter tables in received blocks") {
    // Create parameters with different sizes
    val localParams = Parameters(100, Map(
      Parameters.BlockVersion -> 4,
      Parameters.StorageFeeFactorIncrease -> 1000000
    ), ErgoValidationSettingsUpdate.empty)

    val blockParams = Parameters(100, Map(
      Parameters.BlockVersion -> 4,
      Parameters.StorageFeeFactorIncrease -> 1000000,
      Parameters.SubblocksPerBlockIncrease -> 2  // Additional parameter
    ), ErgoValidationSettingsUpdate.empty)

    // Original matchParameters should fail
    Parameters.matchParameters(localParams, blockParams).isFailure shouldBe true

    // New matchParameters60 should succeed for block version >= 4
    Parameters.matchParameters60(localParams, blockParams, 4).isSuccess shouldBe true
    Parameters.matchParameters60(localParams, blockParams, 3).isSuccess shouldBe true  // Should work for v3 too
  }

  property("matchParameters60 should fail if local parameters are larger than received") {
    val localParams = Parameters(100, Map(
      Parameters.BlockVersion -> 4,
      Parameters.StorageFeeFactorIncrease -> 1000000,
      Parameters.SubblocksPerBlockIncrease -> 2
    ), ErgoValidationSettingsUpdate.empty)

    val blockParams = Parameters(100, Map(
      Parameters.BlockVersion -> 4,
      Parameters.StorageFeeFactorIncrease -> 1000000
    ), ErgoValidationSettingsUpdate.empty)

    Parameters.matchParameters60(localParams, blockParams, 4).isFailure shouldBe true
  }

  property("matchParameters60 should validate parameter values correctly") {
    val localParams = Parameters(100, Map(
      Parameters.BlockVersion -> 4,
      Parameters.StorageFeeFactorIncrease -> 1000000
    ), ErgoValidationSettingsUpdate.empty)

    val blockParams = Parameters(100, Map(
      Parameters.BlockVersion -> 4,
      Parameters.StorageFeeFactorIncrease -> 2000000  // Different value
    ), ErgoValidationSettingsUpdate.empty)

    Parameters.matchParameters60(localParams, blockParams, 4).isFailure shouldBe true
  }

  property("matchParameters60 should validate height consistency") {
    val localParams = Parameters(100, Map(
      Parameters.BlockVersion -> 4,
      Parameters.StorageFeeFactorIncrease -> 1000000
    ), ErgoValidationSettingsUpdate.empty)

    val blockParams = Parameters(101, Map(
      Parameters.BlockVersion -> 4,
      Parameters.StorageFeeFactorIncrease -> 1000000
    ), ErgoValidationSettingsUpdate.empty)

    Parameters.matchParameters60(localParams, blockParams, 4).isFailure shouldBe true
  }



  property("Rule 409 should be disabled and rule 414 enabled for block version 4") {
    val updateForV4 = ErgoValidationSettingsUpdate(Seq(ValidationRules.exMatchParameters), Seq())
    val p: Parameters = Parameters(2, Map(Parameters.BlockVersion -> 4), updateForV4)
    val vr: VotingData = VotingData.empty
    
    // Update validation settings with the update from parameters
    val updatedValidationSettings = validationSettingsNoIl.updated(updateForV4)
    
    val esc = new ErgoStateContext(Seq(), None, ADDigest @@ Array.fill(33)(0: Byte), p, updatedValidationSettings, vr)(updSettings)
    
    // Rule 409 (exMatchParameters) should be disabled
    esc.validationSettings.isActive(ValidationRules.exMatchParameters) shouldBe false
    
    // Rule 414 (exMatchParameters60) should be enabled
    esc.validationSettings.isActive(ValidationRules.exMatchParameters60) shouldBe true
  }
}