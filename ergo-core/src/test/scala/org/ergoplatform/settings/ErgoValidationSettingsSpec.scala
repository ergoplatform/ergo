package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ScorexEncoding
import org.ergoplatform.validation.ModifierValidator

import scala.util.{Failure, Success}

class ErgoValidationSettingsSpec extends ErgoCorePropertyTest with ScorexEncoding {

  private val modifierId = scorex.util.bytesToId(Array.fill(32)(0: Byte))

  property("exMatchParameters60 stays active when absent from rulesSpec") {
    ValidationRules.rulesSpec.contains(ValidationRules.exMatchParameters60) shouldBe false

    val initial = ErgoValidationSettings.initial
    initial.isActive(ValidationRules.exMatchParameters60) shouldBe true

    val update = ErgoValidationSettingsUpdate(
      Seq(ValidationRules.exMatchParameters60),
      Seq.empty
    )
    val updated = initial.updated(update)
    updated.isActive(ValidationRules.exMatchParameters60) shouldBe true

    val parsed = ErgoValidationSettings.parseExtension(updated.toExtensionCandidate).get
    parsed.isActive(ValidationRules.exMatchParameters60) shouldBe true
    parsed.updateFromInitial.rulesToDisable should contain(
      ValidationRules.exMatchParameters60
    )
  }

  property("exMatchParameters60 validateNoFailure reports a named diagnostic") {
    val valid = ModifierValidator(ErgoValidationSettings.initial)
      .validateNoFailure(
        ValidationRules.exMatchParameters60,
        Success(()),
        modifierId,
        Extension.modifierTypeId
      )
      .result
    valid.isValid shouldBe true

    val invalid = ModifierValidator(ErgoValidationSettings.initial)
      .validateNoFailure(
        ValidationRules.exMatchParameters60,
        Failure(new IllegalArgumentException("missing parameter 130")),
        modifierId,
        Extension.modifierTypeId
      )
      .result

    invalid.isValid shouldBe false
    invalid.errors.head.isFatal shouldBe true
    invalid.errors.head.message should include("rule 414")
    invalid.errors.head.message should include("exMatchParameters60")
    invalid.errors.head.message should include("missing parameter 130")
  }

}
