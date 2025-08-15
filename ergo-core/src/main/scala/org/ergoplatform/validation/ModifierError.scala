package org.ergoplatform.validation

import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.modifiers.NetworkObjectTypeId.Value
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.util.ModifierId
import sigma.exceptions.SoftFieldAccessException

import scala.util.control.NoStackTrace

/**
  * Container for error details in regards with block section turned out to be invalid. Wraps validation error,
  * block section id, and block section type id.
  */
case class InvalidModifier(error: String, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value)

/** Base trait for errors that were occurred during NodeView Modifier validation
  */
trait ModifierError {
  def message: String
  def isFatal: Boolean
  def modifierId: ModifierId
  def modifierTypeId: NetworkObjectTypeId.Value
  def toThrowable: Throwable

  def info: String = {
    val fatality = if (isFatal) "fatally" else "recoverably"
    s"Validation of modifier id $modifierId of type $modifierTypeId failed $fatality: $message"
  }
}

/** Permanent modifier error that could not be recovered in future even after any history updates
  */
@SuppressWarnings(Array("org.wartremover.warts.Null"))
class MalformedModifierError(val message: String, val modifierId: ModifierId, val modifierTypeId: NetworkObjectTypeId.Value, cause: Option[Throwable] = None)
    extends Exception(message, cause.orNull) with ModifierError {
  def isFatal: Boolean = true
  def toThrowable: Throwable = this
}

/** Temporary modifier error that may be recovered in future after some history updates.
  * When an instance is created, the stack trace is not collected which makes this exception lightweight.
  */
@SuppressWarnings(Array("org.wartremover.warts.Null"))
class RecoverableModifierError(val message: String, val modifierId: ModifierId, val modifierTypeId: NetworkObjectTypeId.Value, cause: Option[Throwable] = None)
    extends Exception(message, cause.orNull) with ModifierError with NoStackTrace {
  def isFatal: Boolean = false
  def toThrowable: Throwable = this
}


/** Composite error class that can hold more than one modifier error inside. This was not made a `ModifierError` instance
  * intentionally to prevent nesting `MultipleErrors` to `MultipleErrors`
  */
@SuppressWarnings(Array("org.wartremover.warts.Null"))
case class MultipleErrors(errors: Seq[ModifierError])
     extends Exception(errors.mkString(" | "), errors.headOption.map(_.toThrowable).orNull) {
  def isFatal: Boolean = errors.exists(_.isFatal)
}


class SoftFieldsAccessError(cause: SoftFieldAccessException, txId: ModifierId)
  extends Exception(cause.message, cause) with ModifierError with NoStackTrace {

  def isFatal: Boolean = false
  def toThrowable: Throwable = this

  override def message: String = cause.message

  override def modifierId: ModifierId = txId

  override def modifierTypeId: Value = ErgoTransaction.modifierTypeId
}
