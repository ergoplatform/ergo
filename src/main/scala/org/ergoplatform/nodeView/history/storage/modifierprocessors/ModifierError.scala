package org.ergoplatform.nodeView.history.storage.modifierprocessors

/** Base trait for errors that were occurred during NodeView Modifier validation
  */
trait ModifierError { this: Exception =>
  def message: String
  def isFatal: Boolean
}

/** Permanent modifier error that could not be recovered in future even after any history updates
  */
case class MalformedModifierError(message: String) extends Exception(message) with ModifierError {
  def isFatal: Boolean = true
}

/** Temporary modifier error that may be recovered in future after some history updates
  */
case class RecoverableModifierError(message: String) extends Exception(message) with ModifierError {
  def isFatal: Boolean = false
}
