package scorex.core.validation

import org.ergoplatform.modifiers.NetworkObjectTypeId
import scorex.core.validation.ValidationResult.Invalid
import scorex.util.ModifierId

/**
  * Specifies the strategy to by used (fail-fast or error-accumulative), a set of
  * activated validation rules with corresponding error messages
  */
abstract class ValidationSettings {
  val isFailFast: Boolean

  /**
    * @return validation error of type `id` for block section `modifierId` of type `modifierTypeId`, error details in `e`
    */
  def getError(id: Short, e: Throwable, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): Invalid =
    getError(id, InvalidModifier(e.getMessage, modifierId, modifierTypeId))

  def getError(id: Short, invalidMod: InvalidModifier): ValidationResult.Invalid

  def isActive(id: Short): Boolean
}
