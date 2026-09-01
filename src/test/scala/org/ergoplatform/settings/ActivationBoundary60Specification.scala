package org.ergoplatform.settings

import org.ergoplatform.utils.ErgoCorePropertyTest

class ActivationBoundary60Specification extends ErgoCorePropertyTest {
  import org.ergoplatform.settings.Parameters._

  property("matchParameters60 should allow additional parameters in received blocks for version 4") {
    val localParams = Parameters(100, Map(
      BlockVersion -> 4,
      StorageFeeFactorIncrease -> 1000000
    ), ErgoValidationSettingsUpdate.empty)

    val blockParams = Parameters(100, Map(
      BlockVersion -> 4,
      StorageFeeFactorIncrease -> 1000000,
      SubblocksPerBlockIncrease -> 2
    ), ErgoValidationSettingsUpdate.empty)

    // Original matchParameters should fail due to different sizes
    Parameters.matchParameters(localParams, blockParams).isFailure shouldBe true

    // matchParameters60 should succeed for block version >= 4
    Parameters.matchParameters60(localParams, blockParams, 4).isSuccess shouldBe true
  }

  property("matchParameters60 should validate parameter values correctly") {
    val localParams = Parameters(100, Map(
      BlockVersion -> 4,
      StorageFeeFactorIncrease -> 1000000
    ), ErgoValidationSettingsUpdate.empty)

    val blockParams = Parameters(100, Map(
      BlockVersion -> 4,
      StorageFeeFactorIncrease -> 2000000  // Different value
    ), ErgoValidationSettingsUpdate.empty)

    Parameters.matchParameters60(localParams, blockParams, 4).isFailure shouldBe true
  }

  property("matchParameters60 should validate height consistency") {
    val localParams = Parameters(100, Map(
      BlockVersion -> 4,
      StorageFeeFactorIncrease -> 1000000
    ), ErgoValidationSettingsUpdate.empty)

    val blockParams = Parameters(101, Map(
      BlockVersion -> 4,
      StorageFeeFactorIncrease -> 1000000
    ), ErgoValidationSettingsUpdate.empty)

    Parameters.matchParameters60(localParams, blockParams, 4).isFailure shouldBe true
  }

  property("matchParameters60 should fail if local parameters are larger than received") {
    val localParams = Parameters(100, Map(
      BlockVersion -> 4,
      StorageFeeFactorIncrease -> 1000000,
      SubblocksPerBlockIncrease -> 2
    ), ErgoValidationSettingsUpdate.empty)

    val blockParams = Parameters(100, Map(
      BlockVersion -> 4,
      StorageFeeFactorIncrease -> 1000000
    ), ErgoValidationSettingsUpdate.empty)

    Parameters.matchParameters60(localParams, blockParams, 4).isFailure shouldBe true
  }
}