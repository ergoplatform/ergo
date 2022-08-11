package scorex.core.validation

import scorex.core.ModifierTypeId
import scorex.core.validation.ValidationResult.Invalid
import scorex.util.ModifierId

/**
  * Specifies the strategy to by used (fail-fast or error-accumulative), a set of
  * activated validation rules with corresponding error messages
  */
abstract class ValidationSettings {
  val isFailFast: Boolean

  def getError(id: Short, e: Throwable, modifierId: ModifierId, modifierTypeId: ModifierTypeId): Invalid =
    getError(id, InvalidModifier(e.getMessage, modifierId, modifierTypeId))

  def getError(id: Short, invalidMod: InvalidModifier): ValidationResult.Invalid

  def isActive(id: Short): Boolean
}
