package scorex.core.validation

import org.ergoplatform.modifiers.NetworkObjectTypeId
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.ScorexEncoder
import scorex.core.validation.ValidationResult._
import scorex.util.{ModifierId, bytesToId}

import scala.util.{Failure, Success, Try}

/**
  * Object with helpers for the modifier validation process.
  *
  * Allows to initialize ValidationState from current ValidationSettings,
  *
  * It is designed to perform multiple checks for the same object without any transformation.
  * See the example of that
  * kind of validation in Ergo `org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor.HeaderValidator`.
  * Some other examples could also be found in `scorex.core.validation.ValidationSpec`.
  *
  * The second distinction from cats `Validated` is that we do support both fail-fast and error-accumulating validation
  * while cats `Validated` supports only accumulative approach.
  */
object ModifierValidator {

  def apply(settings: ValidationSettings)(implicit e: ScorexEncoder): ValidationState[Unit] = {
    ValidationState(ModifierValidator.success, settings)(e)
  }

  /** report recoverable modifier error that could be fixed by later retries */
  def error(error: String, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): Invalid =
    invalid(new RecoverableModifierError(error, modifierId, modifierTypeId, None))

  /** report recoverable modifier error that could be fixed by later retries */
  def error(error: String, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value, cause: Throwable): Invalid =
    invalid(new RecoverableModifierError(msg(error, cause), modifierId, modifierTypeId, Option(cause)))

  /** report recoverable modifier error that could be fixed by later retries */
  def error(description: String, detail: String, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): Invalid =
    error(msg(description, detail), modifierId, modifierTypeId)

  /** report non-recoverable modifier error that could not be fixed by retries and requires modifier change */
  def fatal(error: String, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): Invalid =
    invalid(new MalformedModifierError(error, modifierId, modifierTypeId, None))

  /** report non-recoverable modifier error that could not be fixed by retries and requires modifier change */
  def fatal(error: String, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value, cause: Throwable): Invalid =
    invalid(new MalformedModifierError(msg(error, cause), modifierId, modifierTypeId, Option(cause)))

  /** report non-recoverable modifier error that could not be fixed by retries and requires modifier change */
  def fatal(description: String, detail: String, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): Invalid =
    fatal(msg(description, detail), modifierId, modifierTypeId)

  /** unsuccessful validation with a given error; also logs the error as an exception */
  def invalid(error: ModifierError): Invalid = Invalid(Seq(error))

  /** successful validation without payload */
  val success: Valid[Unit] = Valid(())

  private def msg(descr: String, e: Throwable): String = msg(descr, Option(e.getMessage).getOrElse(e.toString))

  private def msg(description: String, detail: String): String = s"$description: $detail"
}

/** This is the place where all the validation DSL lives */
case class ValidationState[T](result: ValidationResult[T], settings: ValidationSettings)(implicit e: ScorexEncoder) {

  /** Create the next validation state as the result of given `operation` */
  def pass[R](operation: => ValidationResult[R]): ValidationState[R] = {
    result match {
      case Valid(_) => copy(result = operation)
      case Invalid(_) if settings.isFailFast || result == operation => asInstanceOf[ValidationState[R]]
      case invalid@Invalid(_) => copy(result = invalid.accumulateErrors(operation))
    }
  }

  /** Replace payload with the new one, discarding current payload value. This method catches throwables
    */
  def payload[R](payload: => R): ValidationState[R] = {
    pass(result(payload))
  }

  /** Map payload if validation is successful
    */
  def payloadMap[R](f: T => R): ValidationState[R] = {
    copy(result = result.map(f))
  }

  /** Validate the condition is `true` or else return the `error` given
    */
  def validate(id: Short, condition: => Boolean, invalidModifier: => InvalidModifier): ValidationState[T] = {
    pass(if (!settings.isActive(id) || condition) result else settings.getError(id, invalidModifier))
  }

  /** Reverse condition: Validate the condition is `false` or else return the `error` given */
  def validateNot(id: Short, condition: => Boolean, invalidModifier: => InvalidModifier): ValidationState[T] = {
    validate(id, !condition, invalidModifier)
  }

  /** Validate the first argument equals the second. This should not be used with `ModifierId` of type `Array[Byte]`.
    * The `error` callback will be provided with detail on argument values for better reporting
    */
  def validateEquals[A](id: Short, given: => A, expected: => A, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): ValidationState[T] = {
    pass((given, expected) match {
      case _ if !settings.isActive(id) => result
      case (a: Array[_], b: Array[_]) if a sameElements[Any] b => result
      case (_: Array[_], _) => settings.getError(id, InvalidModifier(s"Given: $given, expected: $expected. Use validateEqualIds when comparing Arrays", modifierId, modifierTypeId))
      case _ if given == expected => result
      case _ => settings.getError(id, InvalidModifier(s"Given: $given, expected $expected", modifierId, modifierTypeId))
    })
  }

  /** Validate the `id`s are equal. The `error` callback will be provided with detail on argument values
    */
  def validateEqualIds(id: Short, given: => ModifierId, expected: => ModifierId, modifierTypeId: NetworkObjectTypeId.Value): ValidationState[T] = {
    pass {
      if (!settings.isActive(id) || given == expected) result
      else settings.getError(id, InvalidModifier(s"Given: ${e.encodeId(given)}, expected ${e.encodeId(expected)}", given, modifierTypeId))
    }
  }

  /** Wrap semantic validity to the validation state: if semantic validity was not Valid, then return the `error` given
    */
  def validateSemantics(id: Short, validity: => ModifierSemanticValidity, details: => InvalidModifier): ValidationState[T] = {
    validateNot(id, validity == ModifierSemanticValidity.Invalid, details)
  }

  /** Validate the `condition` is `Success`. Otherwise the `error` callback will be provided with detail
    * on a failure exception
    */
  def validateNoFailure(id: Short, condition: => Try[_], modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): ValidationState[T] = {
    pass(if (!settings.isActive(id)) result else condition.fold(e => settings.getError(id, e, modifierId, modifierTypeId), _ => result))
  }

  /**
    * Validate the `condition` is `Success`. Otherwise the `error` callback will be provided with detail
    * on a failure exception of type ModifierError which contains modifierId and modifierType information
    * @param id validation identifier
    * @param condition true => Valid, false => Invalid
    * @param modifierTypeId provide for a case when it cannot be resolved from a ModifierError
    * @return validation state
    */
  def validateNoFailure(id: Short, condition: => Try[_], modifierTypeId: NetworkObjectTypeId.Value): ValidationState[T] = {
    pass {
      if (!settings.isActive(id)) {
        result
      } else {
        condition match {
          case Failure(ex: ModifierError) =>
            settings.getError(id, ex, ex.modifierId, ex.modifierTypeId)
          case Success(_) =>
            result
          case Failure(unexpectedEx) =>
            settings.getError(id, unexpectedEx, ModifierId @@@ bytesToId(Array.fill(32)(0.toByte)), modifierTypeId)
        }
      }
    }
  }

  /** Validate the `block` doesn't throw an Exception. Otherwise the `error` callback will be provided with detail
    * on the exception
    */
  def validateNoThrow(id: Short, block: => Any, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): ValidationState[T] = {
    validateNoFailure(id, Try(block), modifierId, modifierTypeId)
  }

  /** Validate `operation` against payload is `Valid` or else return the `error`
    */
  def validateTry[A](tryValue: => Try[A], error: Throwable => Invalid)
                    (operation: (ValidationState[T], A) => ValidationResult[T]): ValidationState[T] = {
    pass(tryValue.fold(error, v => operation(this, v)))
  }

  /** Validate `condition` against payload is `true` or else return the `error`
    */
  def validateTryFlatten(id: Short, operation: T => Try[T], condition: T => Boolean,
                         modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): ValidationState[T] = {
    pass(result.toTry.flatMap(r => operation(r)) match {
      case Failure(ex) => settings.getError(id, ex, modifierId, modifierTypeId)
      case Success(v) if settings.isActive(id) && !condition(v) => settings.getError(id, InvalidModifier(modifierId, modifierId, modifierTypeId))
      case Success(v) => result(v)
    })
  }


  /** Validate `operation` against option value if it's not `None`.
    * If given option is `None` then pass the previous result as success.
    * Return `error` if option is `Some` amd condition is `Invalid`
    */
  def validateOrSkip[A](option: => Option[A])
                       (operation: (ValidationState[T], A) => ValidationResult[T]): ValidationState[T] = {
    option
      .map(value => pass(operation(this, value)))
      .getOrElse(this)
  }

  /** Validate condition against option value if it's not `None`.
    * If given option is `None` then pass the previous result as success.
    * Return `error` if option is `Some` amd condition is `false`
    */
  def validateOrSkipFlatten[A](id: Short, option: => Option[A], condition: A => Boolean,
                               modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value): ValidationState[T] = {
    pass(option match {
      case Some(v) if settings.isActive(id) && !condition(v) => settings.getError(id, InvalidModifier(modifierId, modifierId, modifierTypeId))
      case _ => result
    })
  }

  /** This could add some sugar when validating elements of a given collection
    */
  def validateSeq[A](seq: Iterable[A])
                    (operation: (ValidationState[T], A) => ValidationResult[T]): ValidationState[T] = {
    seq.foldLeft(this) { (state, elem) =>
      state.pass(operation(state, elem))
    }
  }

  /** This is for nested validations that allow mixing fail-fast and accumulate-errors validation strategies
    */
  def validate(operation: => ValidationResult[T]): ValidationState[T] = pass(operation)

}
